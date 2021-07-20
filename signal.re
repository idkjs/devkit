/** Signal handling */;

open ExtLib;
module U = ExtUnix.All;
module Ev = Async.Ev;

let log = Log.from("signal");

/** {2 libevent + signalfd} */;

type t = {
  ev: Ev.event,
  fd: Unix.file_descr,
  h: Hashtbl.t(int, int => unit),
  mutable active: bool,
};

let init = events => {
  let fd = U.signalfd(~sigs=[], ~flags=[], ());
  Unix.set_nonblock(fd);
  let t = {ev: Ev.create(), fd, h: Hashtbl.create(1), active: true};
  Ev.set(events, t.ev, t.fd, ~persist=true, [Ev.READ], (_, _) =>
    /* references to t keep it alive with ev */
    try({
      let ssi = U.signalfd_read(t.fd);
      let signo = U.ssi_signo_sys(ssi);
      switch (Hashtbl.find_option(t.h, signo)) {
      | None => Exn.fail("no handler for %d", signo)
      | Some(f) => f(signo)
      };
    }) {
    | exn => log#warn(~exn, "signal handler")
    }
  );
  Ev.add(t.ev, None);
  t;
};

let stop = t =>
  switch (t.active) {
  | false => ()
  | true =>
    Ev.del(t.ev);
    Hashtbl.clear(t.h);
    Unix.close(t.fd);
    t.active = false;
  };

let handle = (t, sigs, f) => {
  List.iter(signo => Hashtbl.replace(t.h, signo, f), sigs);
  let sigs = List.of_enum(Hashtbl.keys(t.h));
  let _: list(int) = Unix.sigprocmask(Unix.SIG_BLOCK, sigs);
  let _ = U.signalfd(~fd=t.fd, ~sigs, ~flags=[], ());
  ();
};

/** {2 Lwt} */;

let h_lwt = Hashtbl.create(10);

let lwt_handle = (sigs, f) =>
  sigs
  |> List.iter(signo => {
       Option.may(Lwt_unix.disable_signal_handler) @@
       Hashtbl.find_option(h_lwt, signo);
       let sig_id = Lwt_unix.on_signal(signo, (_: int) => f());
       Hashtbl.replace(h_lwt, signo, sig_id);
     });

/** {2 generic registration} */;

let install_sys = (signo, f) => Sys.set_signal(signo, Sys.Signal_handle(f));
let install_libevent = (t, signo, f) => handle(t, [signo], f);
let install_lwt = (signo, f) => lwt_handle([signo], () => f(signo));

let h = Hashtbl.create(10);
let verbose = ref(false);
let do_install = ref(install_sys);
let is_safe_output = () => verbose^;

let set = (sigs, f) =>
  sigs
  |> List.iter(signo =>{
       let f =
         switch (Hashtbl.find_option(h, signo)) {
         | None => f
         | Some(g) => (
             n => {
               g(n);
               f(n);
             }
           )
         };

       Hashtbl.replace(h, signo, f);
       do_install^(signo, f);
    } );

let set1 = (signal, f) => set([signal], _ => f());

type state = Hashtbl.t(int, int => unit);
let save = () => Hashtbl.copy(h);
let restore = x => {
  Hashtbl.clear(h);
  Hashtbl.iter(Hashtbl.add(h), x);
};

let replace = (sigs, f) =>
  sigs
  |> List.iter(signo => {
       Hashtbl.replace(h, signo, f);
       do_install^(signo, f);
     });

let reinstall = () => Hashtbl.iter(do_install^, h);

let wrap = (name, f, n) => {
  if (verbose^) {
    log#info("Received signal %i (%s)...", n, name);
  };
  try(f()) {
  | exn =>
    if (verbose^) {
      log#warn(~exn, "Signal handler failed");
    }
  };
  if (verbose^) {
    log#info("Signal handler done.");
  };
};

let set_exit = f => set([Sys.sigterm, Sys.sigint], wrap("exit", f));
let set_reload = f => set([Sys.sighup], wrap("reload", f));

let setup_sys = () => {
  verbose := false; /* potential deadlock */
  do_install := install_sys;
  reinstall();
};

let setup_libevent' = t => {
  verbose := true;
  do_install := install_libevent(t);
  reinstall();
};

let setup_libevent = setup_libevent';
let setup_libevent_ = events => setup_libevent' @@ init(events);

let setup_lwt = () => {
  verbose := true;
  do_install := install_lwt;
  reinstall();
};
