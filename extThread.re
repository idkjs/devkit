let log = Log.self;

type t('a) = (ref([ | `Exn(exn) | `None | `Ok('a)]), Thread.t);
let detach = (f, x) => {
  let result = ref(`None);
  (result, Thread.create(() => result := Exn.map(f, x), ()));
};
let join = ((result, thread)) => {
  Thread.join(thread);
  switch (result^) {
  | `None => assert(false)
  | (`Ok(_) | `Exn(_)) as x => x
  };
};
let join_exn = t =>
  switch (join(t)) {
  | `Ok(x) => x
  | `Exn(exn) => raise(exn)
  };
let map = (f, a) => Array.map(join_exn) @@ Array.map(detach(f), a);
let mapn = (~n=8, f, l) => {
  assert(n > 0);
  Action.distribute(n, l)
  |> map(List.map @@ Exn.map(f))
  |> Action.undistribute;
};

let locked = (mutex, f) => {
  Mutex.lock(mutex);
  Std.finally(() => Mutex.unlock(mutex), f, ());
};

module LockMutex = {
  type t = Mutex.t;
  let create = Mutex.create;
  let locked = locked;
};

module Async_fin = {
  open Async;
  module U = ExtUnix.All;

  type t = {
    q: Mtq.t(unit => unit),
    evfd: Unix.file_descr,
  };

  let is_available = () => ExtUnix.Config.have(`EVENTFD);

  let setup = events => {
    let fin = {q: Mtq.create(), evfd: U.eventfd(0)};
    let rec loop = () =>
      switch (Mtq.try_get(fin.q)) {
      | None => ()
      | Some(f) =>
        try(f()) {
        | exn => log#warn(~exn, "fin loop")
        };
        loop();
      };

    let reset = fd =>
      try(ignore(U.eventfd_read(fd))) {
      | Unix.Unix_error(Unix.EAGAIN, _, _) => ()
      | exn =>
        log#warn(~exn, "fin reset");
        ();
      };

    setup_simple_event(
      events,
      fin.evfd,
      [Ev.READ],
      (_, fd, _) => {
        reset(fd);
        loop();
      },
    );
    fin;
  };

  let shutdown = ({q, evfd}) => {
    Mtq.clear(q);
    Unix.close(evfd);
  };

  let callback = (fin, f) => {
    Mtq.put(fin.q, f);
    U.eventfd_write(fin.evfd, 1L);
  };
};

let log_create = (~name=?, f, x) =>
  Thread.create(() => Action.log(~name?, f, x), ());

let run_periodic = (~delay, ~now=false, f) => {
  let _: Thread.t =
    Thread.create(
      () => {
        if (!now) {
          Nix.sleep(delay);
        };
        while (try(f()) {
               | exn =>
                 Log.self#warn(~exn, "ExtThread.run_periodic");
                 true;
               }) {
          Nix.sleep(delay);
        };
      },
      (),
    );

  ();
};

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

module Workers = (T: WorkerT) => {
  type task = T.task;
  type result = T.result;
  type t = (Mtq.t(task), Mtq.t(result), int);

  let worker = (qi, f, qo) =>
    while (true) {
      Mtq.put(qo, f(Mtq.get(qi)));
    };

  let stop = (~wait as _=?, (qi, _, _)) => Mtq.clear(qi);

  let create = (f, n) => {
    let qi = Mtq.create()
    and qo = Mtq.create();
    for (_ in 1 to n) {
      ignore(Thread.create(() => worker(qi, f, qo), ()));
    };
    (qi, qo, n);
  };

  let perform = ((qi, qo, n), ~autoexit as _=?, e, f) => {
    let active = ref(0);
    for (_ in 1 to n) {
      switch (Enum.get(e)) {
      | Some(x) =>
        Mtq.put(qi, x);
        incr(active);
      | None => ()
      };
    };
    while (active^ > 0) {
      let res = Mtq.get(qo);
      switch (Enum.get(e)) {
      | Some(x) => Mtq.put(qi, x)
      | None => decr(active)
      };
      f(res);
    };
  };
};

let atomic_incr = incr;
let atomic_decr = decr;
let atomic_get = x => x^;

module Pool = {
  type t = {
    q: Mtq.t(unit => unit),
    total: int,
    free: ref(int),
    mutable blocked: bool,
  };

  let create = n => {
    let t = {q: Mtq.create(), total: n, free: ref(-1), blocked: false};
    t;
  };

  let init = t => {
    let worker = _i =>
      while (true) {
        let f = Mtq.get(t.q);
        atomic_decr(t.free);
        try(f()) {
        | exn => log#warn(~exn, "ThreadPool")
        };
        atomic_incr(t.free);
      };

    t.free := t.total;
    for (i in 1 to t.total) {
      let _: Thread.t = log_create(worker, i);
      ();
    };
  };

  let status = t =>
    Printf.sprintf(
      "queue %d threads %d of %d",
      Mtq.length(t.q),
      atomic_get(t.free),
      t.total,
    );

  let put = t => {
    if (atomic_get(t.free) == (-1)) {
      init(t);
    };
    while (t.blocked) {
      Nix.sleep(0.05);
    };
    Mtq.put(t.q);
  };

  let wait_blocked = (~n=0, t) =>
    if (atomic_get(t.free) != (-1)) {
      while (t.blocked) {
        Nix.sleep(0.05);
      }; /* Wait for unblock */
      t.blocked = true;
      assert(n >= 0);
      let i = ref(1);
      while (Mtq.length(t.q) + (t.total - atomic_get(t.free)) > n) {
        /* Notice that some workers can be launched! */
        if (i^ == 100 || i^ mod 1000 == 0) {
          log#info("Thread Pool - waiting block : %s", status(t));
        };
        Nix.sleep(0.05);
        incr(i);
      };
      t.blocked = false;
    };
};
