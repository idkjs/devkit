open Printf;
open ExtLib;
open Prelude;

let log = Log.from("parallel");

module type WorkerT = {
  type task;
  type result;
};

module type Workers = {
  type task;
  type result;
  type t;
  let create: (task => result, int) => t;
  let perform: (t, ~autoexit: bool=?, Enum.t(task), result => unit) => unit;
  let stop: (~wait: int=?, t) => unit;
};

/** @return list of reaped and live pids */

let reap = l =>
  Unix.(
    List.partition(
      pid =>
        try(pid == fst(waitpid([WNOHANG], pid))) {
        | [@implicit_arity] Unix_error(ECHILD, _, _) => true /* exited */
        | exn =>
          log#warn(~exn, "Worker PID %d lost (wait)", pid);
          true;
        },
      l,
    )
  );

let hard_kill1 = pid =>
  Unix.(
    try(
      {
        kill(pid, Sys.sigkill);
        log#warn("Worker PID %d killed with SIGKILL", pid);
      }
    ) {
    | [@implicit_arity] Unix_error(ESRCH, _, _) => ()
    | exn => log#warn(~exn, "Worker PID %d (SIGKILL)", pid)
    }
  );

let hard_kill = l => {
  let (_, live) = reap(l);
  List.iter(hard_kill1, live);
};

let killall = (signo, pids) =>
  pids
  |> List.iter(pid =>
       try(Unix.kill(pid, signo)) {
       | exn =>
         log#warn(~exn, "PID %d lost (trying to send signal %d)", pid, signo)
       }
     );

let do_stop = (~wait=?, pids) => {
  let rec reap_loop = (timeout, l) => {
    let (_, live) = reap(l);
    switch (timeout, live) {
    | (_, []) => `Done
    | (Some(0), l) =>
      hard_kill(l);
      `Killed(List.length(l));
    | (_, l) =>
      Nix.sleep(1.);
      reap_loop(Option.map(pred, timeout), l);
    };
  };

  killall(Sys.sigterm, pids);
  reap_loop(wait, pids);
};

module Forks = (T: WorkerT) => {
  type task = T.task;
  type result = T.result;
  type instance = {
    mutable ch: option((in_channel, out_channel)),
    pid: int,
  };
  type t = {
    mutable running: list(instance),
    execute: task => result,
    mutable alive: bool,
    mutable gone: int,
  };

  let worker = (execute: task => result) => {
    let (main_read, child_write) = Unix.pipe();
    let (child_read, main_write) = Unix.pipe();
    switch (Nix.fork()) {
    | `Child =>
      /* child */
      Unix.close(main_read);
      Unix.close(main_write);
      Unix.set_close_on_exec(child_read);
      Unix.set_close_on_exec(child_write);
      let output = Unix.out_channel_of_descr(child_write);
      let input = Unix.in_channel_of_descr(child_read);
      let rec loop = () =>
        switch (
          Nix.restart(
            () =>
              ExtUnix.All.(
                poll([|(child_read, Poll.(pollin + pollpri))|], -1.)
              ),
            (),
          )
        ) {
        | []
        | [_, _, ..._] => assert(false)
        | [(_fd, revents)] =>
          assert(!ExtUnix.All.Poll.(is_set(revents, pollpri)));
          assert(ExtUnix.All.Poll.(is_inter(revents, pollin + pollhup)));
          switch ((Marshal.from_channel(input): task)) {
          | exception End_of_file => ()
          | exception exn =>
            log#error(~exn, "Parallel.worker failed to unmarshal task")
          | v =>
            let r = execute(v);
            Marshal.to_channel(output, r: result, []);
            flush(output);
            loop();
          };
        };

      try(loop()) {
      | exn =>
        log#error(
          ~exn,
          ~backtrace=true,
          "Parallel.worker aborting on uncaught exception",
        )
      };
      close_in_noerr(input);
      close_out_noerr(output);
      exit(0);
    | `Forked(pid) =>
      Unix.close(child_read);
      Unix.close(child_write);
      /* prevent sharing these pipes with other children */
      Unix.set_close_on_exec(main_write);
      Unix.set_close_on_exec(main_read);
      let cout = Unix.out_channel_of_descr(main_write);
      let cin = Unix.in_channel_of_descr(main_read);
      {ch: Some((cin, cout)), pid};
    };
  };

  let create = (execute, n) => {
    let running = List.init(n, _ => worker(execute));
    {running, execute, alive: true, gone: 0};
  };

  let close_ch = w =>
    switch (w.ch) {
    | Some((cin, cout)) =>
      w.ch = None;
      close_in_noerr(cin);
      close_out_noerr(cout);
    | None => ()
    };

  let stop = (~wait=?, t) => {
    let gone = () =>
      if (t.gone == 0) {
        "";
      } else {
        sprintf(" (%d workers vanished)", t.gone);
      };
    log#info("Stopping %d workers%s", List.length(t.running), gone());
    t.alive = false;
    let l =
      t.running
      |> List.map(w => {
           close_ch(w);
           w.pid;
         });
    Nix.sleep(0.1); /* let idle workers detect EOF and exit peacefully (frequent io-in-signal-handler deadlock problem) */
    t.running = [];
    switch (do_stop(~wait?, l)) {
    | `Done =>
      log#info("Stopped %d workers properly%s", List.length(l), gone())
    | `Killed(killed) =>
      log#info(
        "Timeouted, killing %d (of %d) workers with SIGKILL%s",
        killed,
        List.length(l),
        gone(),
      )
    };
  };

  let perform = (t, ~autoexit=false, tasks, finish) =>
    switch (t.running) {
    | [] => Enum.iter(x => finish(t.execute(x)), tasks) /* no workers */
    | _ =>
      let workers = ref(0);
      t.running
      |> List.iter(w =>
           switch (w.ch) {
           | None => ()
           | Some((_, cout)) =>
             switch (Enum.get(tasks)) {
             | None => ()
             | Some(x) =>
               incr(workers);
               Marshal.to_channel(cout, x: task, []);
               flush(cout);
             }
           }
         );
      /*       Printf.printf "workers %u\n%!" !workers; */
      let events = ExtUnix.All.Poll.(pollin + pollpri);
      while (workers^ > 0 && t.alive) {
        let fds =
          List.filter_map(
            fun
            | {ch: Some((cin, _)), _} =>
              Some(Unix.descr_of_in_channel(cin))
            | _ => None,
            t.running,
          );
        let r =
          Nix.restart(
            () =>
              ExtUnix.All.poll(
                Array.of_list(List.map(fd => (fd, events), fds)),
                -1.,
              ),
            (),
          );
        assert(
          !
            List.exists(
              ((_fd, revents)) =>
                ExtUnix.All.Poll.(is_set(revents, pollpri)),
              r,
            ),
        );
        let channels =
          r
          |> List.map(((fd, _revents)) =>
               t.running
               |> List.find(
                    fun
                    | {ch: Some((cin, _)), _} =>
                      Unix.descr_of_in_channel(cin) == fd
                    | _ => false,
                  )
             );

        let answers =
          channels
          |> List.filter_map(w =>
               switch (w.ch) {
               | None => None
               | Some((cin, cout)) =>
                 try(
                   switch ((Marshal.from_channel(cin): result)) {
                   | exception exn =>
                     log#warn(~exn, "no result from PID %d", w.pid);
                     t.gone = t.gone + 1;
                     decr(workers);
                     /* close pipes and forget dead child, do not reap zombie so that premature exit is visible in process list */
                     close_ch(w);
                     t.running =
                       List.filter(w' => w'.pid != w.pid, t.running);
                     None;
                   | answer =>
                     switch (Enum.get(tasks)) {
                     | None =>
                       if (autoexit) {
                         close_ch(w);
                       };
                       decr(workers);
                     | Some(x) =>
                       Marshal.to_channel(cout, x: task, []);
                       flush(cout);
                     };
                     Some(answer);
                   }
                 ) {
                 | exn =>
                   log#warn(~exn, "perform (from PID %d)", w.pid);
                   decr(workers);
                   None;
                 }
               }
             );

        List.iter(finish, answers);
      };
      switch (t.gone) {
      | 0 => log#info("Finished")
      | n => log#warn("Finished, %d workers vanished", n)
      };
    };
};

let invoke = (f: 'a => 'b, x): (unit => 'b) => {
  let (input, output) = Unix.pipe();
  switch (Nix.fork()) {
  | exception _ =>
    Unix.close(input);
    Unix.close(output);
    let v = f(x);
    (() => v);
  | `Child =>
    Unix.close(input);
    let output = Unix.out_channel_of_descr(output);
    Marshal.to_channel(
      output,
      try(`Res(f(x))) {
      | e => `Exn(e)
      },
      [],
    );
    close_out(output);
    exit(0);
  | `Forked(pid) =>
    Unix.close(output);
    let input = Unix.in_channel_of_descr(input);
    (
      () => {
        let v = Marshal.from_channel(input);
        ignore(Nix.restart(Unix.waitpid([]), pid));
        close_in(input);
        switch (v) {
        | `Res(x) => x
        | `Exn(e) => raise(e)
        };
      }
    );
  };
};

/*

 (* example *)
 open Printf

 module W = Workers(struct type task = string type result = string list end)

 let execute s = for i = 1 to 100_000 do Thread.delay 0. done; printf "%u : %s\n%!" (Unix.getpid()) s; [s;s;s;s]

 let () =
   let workers = W.create execute 4 in
   print_endline "go";
   let e = Enum.init 100 (sprintf "<%u>") in
   let f l = printf "got [%s]\n%!" (Stre.list Prelude.id l) in
   for i = 1 to 2 do
     W.perform workers (Enum.clone e) f;
     Thread.delay 1.
   done;
   print_endline "Done"
 */

let rec launch_forks = f =>
  fun
  | [] => ()
  | [x, ...xs] =>
    switch (Nix.fork()) {
    | `Child => f(x)
    | `Forked(_) => launch_forks(f, xs)
    };

/** keep the specifed number of workers running */

let run_forks_simple = (~revive=false, ~wait_stop=?, f, args) => {
  let workers = Hashtbl.create(1);
  let launch = (f, x) =>
    switch (Nix.fork()) {
    | `Child =>
      let () =
        try(f(x)) {
        | exn => log#warn(~exn, "worker failed")
        };
      exit(0);
    | `Forked(pid) =>
      Hashtbl.add(workers, pid, x);
      pid;
    };

  args
  |> List.iter(x =>{
       let _: int = launch(f, x);
       ();
     });
  let pids = () => Hashtbl.keys(workers) |> List.of_enum;
  let rec loop = pause => {
    Nix.sleep(pause);
    let total = Hashtbl.length(workers);
    if (total == 0 && !revive) {
      log#info("All workers dead, stopping");
    } else {
      Daemon.should_exit()
        ? {
          log#info("Stopping %d workers", total);
          switch (
            do_stop(~wait=?wait_stop, Hashtbl.keys(workers) |> List.of_enum)
          ) {
          | `Done => log#info("Stopped %d workers", total)
          | `Killed(n) =>
            log#info("Killed %d (of %d) workers with SIGKILL", n, total)
          };
        }
        : {
          let (dead, _live) = reap(pids());
          switch (dead) {
          | [] => loop(max(1., pause /. 2.))
          | dead when revive =>
            let pause = min(10., pause *. 1.5);
            dead
            |> List.iter(pid =>
                 switch (Hashtbl.find(workers, pid)) {
                 | exception Not_found =>
                   log#warn("WUT? Not my worker %d", pid)
                 | x =>
                   Hashtbl.remove(workers, pid);
                   switch (launch(f, x)) {
                   | exception exn => log#error(~exn, "restart")
                   | pid' =>
                     log#info("worker %d exited, replaced with %d", pid, pid')
                   };
                 }
               );
            loop(pause);
          | dead =>
            log#info(
              "%d child workers exited (PIDs: %s)",
              List.length(dead),
              Stre.list(string_of_int, dead),
            );
            List.iter(Hashtbl.remove(workers), dead);
            loop(pause);
          };
        };
    };
  };

  loop(1.);
};

let run_workers_enum =
    (workers, ~wait_stop=?, type t, type u, f: t => u, g: u => unit, enum) => {
  assert(workers > 0);
  module Worker = {
    type task = t;
    type result = u;
  };
  module W = Forks(Worker);
  let worker = x => {
    /* sane signal handler FIXME restore? */
    Signal.set_exit(Daemon.signal_exit);
    f(x);
  };

  let proc = W.create(worker, workers);
  Nix.handle_sig_exit_with(~exit=true, () => W.stop(~wait=?wait_stop, proc)); /* FIXME: output in signal handler */
  W.perform(~autoexit=true, proc, enum, g);
  W.stop(proc);
};

let run_workers = (workers, ~wait_stop=?, type t, f: t => unit, l) =>
  run_workers_enum(workers, ~wait_stop?, f, id, List.enum(l));

let run_forks =
    (~wait_stop=?, ~revive=?, ~wait=?, ~workers=?, type t, f: t => unit, l) => {
  let wait_stop =
    if (wait_stop == None) {
      wait;
    } else {
      wait_stop;
    };
  switch (workers) {
  | None => run_forks_simple(~wait_stop?, ~revive?, f, l)
  | Some(n) => run_workers(n, ~wait_stop?, f, l)
  };
};

let run_forks' = (f, l) =>
  switch (l) {
  | [] => ()
  | [x] => f(x)
  | l => run_forks(f, l)
  };

module Services = {
  type t = {
    mutable pids: list(int),
    work: int => Lwt.t(unit),
  };

  let start = (n, work) => {
    let rec start_forked = i =>
      if (i >= n) {
        Lwt.return_nil;
      } else {
        switch (Nix.fork()) {
        | `Child =>
          let%lwt () = work(i);
          exit(0);
        | `Forked(pid) =>
          log#debug("Starting worker %d with pid %d", i, pid);
          Lwt.map(pids => [pid, ...pids], start_forked(i + 1));
        };
      };

    Lwt.map(pids => {pids, work}, start_forked(0));
  };

  let wait = pid =>
    try%lwt(Lwt.map(fst, Lwt_unix.waitpid([], pid))) {
    | [@implicit_arity] Unix.Unix_error(ECHILD, _, _) => Lwt.return(pid)
    | exn =>
      log#warn(~exn, "Worker PID %d lost (wait)", pid);
      Lwt.return(pid);
    };

  let kill = (~timeout, pid) => {
    let graceful = {
      Unix.kill(pid, Sys.sigterm);
      let%lwt _ = wait(pid);
      log#debug("Worker PID %d killed with SIGTERM", pid);
      Lwt.return_unit;
    };

    let ungraceful = {
      let%lwt () = Lwt_unix.sleep(timeout);
      hard_kill1(pid);
      Lwt.return_unit;
    };

    Lwt.pick([graceful, ungraceful]);
  };

  let rolling_restart = (~wait=?, ~timeout, workers) => {
    let%lwt pids =
      Lwt_list.mapi_s(
        (i, pid) => {
          log#debug("Restarting worker %d with PID %d\n%!", i, pid);
          let%lwt () = kill(~timeout, pid);
          Option.may(Unix.sleep, wait);
          switch (Nix.fork()) {
          | `Child =>
            let%lwt () = workers.work(i);
            exit(0);
          | `Forked(pid') =>
            log#debug("Worker %d started with PID %d\n%!", i, pid');
            Lwt.return(pid');
          };
        },
        workers.pids,
      );

    workers.pids = pids;
    Lwt.return_unit;
  };

  let stop = (~timeout, {pids, _}) => {
    log#info("Stopping workers");
    Lwt_list.iteri_p(
      (i, pid) => {
        log#debug("Stopping worker %d with PID %d", i, pid);
        kill(~timeout, pid);
      },
      pids,
    );
  };
};
