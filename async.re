/** Asynchronous IO helpers */;

open Prelude;

module Ev = Libevent;

module Internal = {
  let log = Log.from("async");
};

open Internal;

/** Create a new event or use the provided [ev] and make it persistent
  with the infinite timeout (or use the provided [timeout]).
  Schedule this event with provided callback [f].
  Don't forget [del] to unschedule. */

let simple_event = (events, ~ev=Ev.create(), ~timeout=?, fd, flags, f) => {
  Ev.set(events, ev, fd, flags, ~persist=true, (fd, flags) =>
    try(f(ev, fd, flags)) {
    | exn => log#warn(~exn, "simple_event")
    }
  );
  Ev.add(ev, timeout);
  ev;
};

let setup_simple_event = (events, ~ev=?, ~timeout=?, fd, flags, f) => {
  let _: Ev.event = simple_event(events, ~ev?, ~timeout?, fd, flags, f);
  ();
};

type result =
  | End
  | Data(int)
  | Block
  | Exn(exn);

let read_some = (fd, buf, ofs, len) =>
  try(
    switch (Unix.read(fd, buf, ofs, len)) {
    | 0 => End
    | n => Data(n)
    }
  ) {
  | [@implicit_arity] Unix.Unix_error(Unix.EAGAIN | Unix.EWOULDBLOCK, _, _) =>
    Block
  | exn =>
    log#warn(~exn, "read_some");
    Exn(exn);
  };

let write_some = (fd, buf, ofs, len) =>
  try(Unix.write_substring(fd, buf, ofs, len)) {
  | [@implicit_arity] Unix.Unix_error(Unix.EAGAIN | Unix.EWOULDBLOCK, _, _) => 0
  };

/** Read out all immediately available input (no blocking)
  @return `Limit when [limit] is exceeded, `Chunk (data,final) otherwise
  */

let read_available = (~limit, fd) => {
  let buf = Buffer.create(1024);
  let s = Bytes.create(1024);
  let rec loop = () =>
    switch (read_some(fd, s, 0, Bytes.length(s))) {
    | End => `Chunk((Buffer.contents(buf), true))
    | Block => `Chunk((Buffer.contents(buf), false))
    | Exn(exn) => raise(exn)
    | Data(len) =>
      Buffer.add_substring(buf, Bytes.to_string(s), 0, len);
      if (Buffer.length(buf) > limit) {
        `Limit(Buffer.contents(buf));
      } else {
        loop();
      };
    };

  loop();
};

let show_error =
  fun
  | `Timeout => "timeout"
  | `EofImm => "eof (immediate)"
  | `Eof => "eof (async)"
  | `Exn(exn) => Printf.sprintf("exn %s (async)", Exn.str(exn))
  | `ExnImm(exn) => Printf.sprintf("exn %s (immediate)", Exn.str(exn));

/** [read_buf buf fd err k] - asynchronously fill [buf] with data from [fd] and call [k buf] when done (buffer is full).
  [fd] should be nonblocking. Call [err] on error (EOF). */

let read_buf = (base, ~ev=?, ~timeout=?, buf, fd, err, k) => {
  let len = Bytes.length(buf);
  let later = cur => {
    let cur = ref(cur);
    setup_simple_event(base, ~ev?, ~timeout?, fd, [Ev.READ], (ev, fd, flags) =>
      switch (flags) {
      | Ev.TIMEOUT =>
        Ev.del(ev);
        err(`Timeout, cur^);
      | Ev.WRITE
      | Ev.SIGNAL => assert(false)
      | Ev.READ =>
        switch (read_some(fd, buf, cur^, len - cur^)) {
        | End =>
          Ev.del(ev);
          err(`Eof, cur^);
        | Exn(exn) =>
          Ev.del(ev);
          err(`Exn(exn), cur^);
        | Data(n) =>
          cur := cur^ + n;
          if (cur^ == len) {
            Ev.del(ev);
            k(buf);
          };
        | Block =>
          log#warn(
            "Async.read_buf: useless wakeup on fd %d, ignoring",
            U.int_of_file_descr(fd),
          )
        }
      }
    );
  };

  switch (read_some(fd, buf, 0, len)) {
  | End => err(`EofImm, 0)
  | Exn(exn) => err(`ExnImm(exn), 0)
  | Data(n) when n == len => k(buf)
  | Block => later(0)
  | Data(n) => later(n)
  };
};

let read_n = (base, ~ev=?, ~timeout=?, n, fd, err, k) =>
  read_buf(base, ~ev?, ~timeout?, Bytes.create(n), fd, err, buf =>
    k(Bytes.unsafe_to_string(buf))
  );

/** Call [f] with [delay]-second pauses between invocations.
    Set [stop] to [true] to stop the timer.
    NB do not [Ev.del] the event inside the [f] callback. */

let periodic_timer_0 = (events, stop, first_delay, delay, ~name="", f) => {
  let timer = Ev.create();
  Ev.set_timer(
    events,
    timer,
    ~persist=false,
    () => {
      if (! stop^) {
        try(f()) {
        | exn => log#warn(~exn, "periodic_timer %s", name)
        };
      };
      if (! stop^) {
        Ev.add(timer, Some(delay));
      };
    },
  );
  if (! stop^) {
    Ev.add(timer, Some(first_delay));
  };
  timer;
};

let periodic_timer_now = (events, ~stop=ref(false), delay, ~name=?, f) =>
  periodic_timer_0(events, stop, 0., delay, ~name?, f);
let periodic_timer_wait = (events, ~stop=ref(false), delay, ~name=?, f) =>
  periodic_timer_0(events, stop, delay, delay, ~name?, f);

let setup_periodic_timer_now = (events, ~stop=?, delay, ~name=?, f) => {
  let _: Ev.event = periodic_timer_now(events, ~stop?, delay, ~name?, f);
  ();
};

let setup_periodic_timer_wait = (events, ~stop=?, delay, ~name=?, f) => {
  let _: Ev.event = periodic_timer_wait(events, ~stop?, delay, ~name?, f);
  ();
};

/*
 (** Call [f] with [delay]-second pauses between invocations.
     Set [stop] to [true] to stop the timer.
     NB do not [Ev.del] the event inside the [f] callback. *)
 let periodic_timer_0 stop first_delay delay ?(name="") f =
   let rec loop () =
     if not !stop then begin try f () with exn -> log #warn ~exn "periodic_timer %s" name end;
     if not !stop then Ev.add timer (Some delay);
   end;
   if not !stop then Ev.add timer (Some first_delay);
   timer

 let periodic_timer_now events ?(stop=ref false) delay ?name f = periodic_timer_0 events stop 0. delay ?name f
 let periodic_timer_wait events ?(stop=ref false) delay ?name f = periodic_timer_0 events stop delay delay ?name f
 */

module Peer = {
  type t = {
    events: Ev.event_base,
    read: Ev.event,
    write: Ev.event,
    timeout: option(float),
    fd: Unix.file_descr,
    addr: Unix.sockaddr,
    err: unit => unit,
  };

  let create = (events, ~err=id, ~timeout=?, (fd, addr)) => {
    Unix.set_nonblock(fd);
    {events, fd, addr, timeout, read: Ev.create(), write: Ev.create(), err};
  };

  let add_event = (p, ev, timeout) => {
    let timeout =
      if (timeout == None) {
        p.timeout;
      } else {
        timeout;
      };
    Ev.add(ev, timeout);
  };

  let finish = p => {
    Ev.del(p.read);
    Ev.del(p.write);
    try(Unix.shutdown(p.fd, Unix.SHUTDOWN_ALL)) {
    | _ => ()
    };
    Unix.close(p.fd);
  };

  let error = (~exn=?, p, msg) => {
    log#warn(~exn?, "Peer %s %s", Nix.show_addr(p.addr), msg);
    Std.finally(() => finish(p), p.err, ());
  };

  let receive = (p, ~timeout=?, buf, k) => {
    let len = Bytes.length(buf);
    let later = cur => {
      let cur = ref(cur);
      Ev.set(p.events, p.read, p.fd, [Ev.READ], ~persist=true, (fd, flags) =>
        try(
          switch (flags) {
          | Ev.TIMEOUT => error(p, "receive timeout")
          | Ev.WRITE => assert(false)
          | Ev.SIGNAL => assert(false)
          | Ev.READ =>
            switch (read_some(fd, buf, cur^, len - cur^)) {
            | End => error(p, "receive eof")
            | Exn(exn) => error(~exn, p, "receive")
            | Data(n) =>
              cur := cur^ + n;
              if (cur^ == len) {
                Ev.del(p.read);
                k(buf);
              };
            | Block =>
              log#warn(
                "Async.receive: useless wakeup on fd %d, ignoring",
                U.int_of_file_descr(fd),
              )
            }
          }
        ) {
        | exn => error(~exn, p, "receive")
        }
      );
      add_event(p, p.read, timeout);
    };

    later(0);
  };
  /*
     match read_some p.fd buf 0 len with
     | End -> error p "receive eof"
     | Exn exn -> error ~exn p "receive"
     | Data n when n = len -> k buf
     | Block -> later 0
     | Data n -> later n
   */

  let send = (p, ~timeout=?, buf, k) => {
    let len = String.length(buf);
    let later = cur => {
      let cur = ref(cur);
      Ev.set(p.events, p.write, p.fd, [Ev.WRITE], ~persist=true, (fd, flags) =>
        try(
          switch (flags) {
          | Ev.TIMEOUT => error(p, "send timeout")
          | Ev.READ => assert(false)
          | Ev.SIGNAL => assert(false)
          | Ev.WRITE =>
            cur := cur^ + write_some(fd, buf, cur^, len - cur^);
            if (cur^ == len) {
              Ev.del(p.write);
              k();
            };
          }
        ) {
        | exn => error(~exn, p, "send")
        }
      );
      add_event(p, p.write, timeout);
    };

    later(0);
  };
  /*
     match try Some (write_some p.fd buf 0 len) with exn -> error ~exn p "send"; None with
     | Some n when n = len -> k ()
     | Some n -> later n
     | None -> ()
   */

  let send_all = (p, ~timeout=?, bufs, k) => {
    let rec loop =
      fun
      | [] => k()
      | [x, ...xs] => send(p, ~timeout?, x, () => loop(xs));

    loop(bufs);
  };

  let connect = (p, ~timeout=?, k) =>
    try(Unix.connect(p.fd, p.addr)) {
    | [@implicit_arity] Unix.Unix_error(Unix.EINPROGRESS, _, _) =>
      Ev.set(p.events, p.read, p.fd, [Ev.READ], ~persist=false, (fd, flags) =>
        try(
          switch (flags) {
          | Ev.TIMEOUT => error(p, "connect timeout")
          | Ev.WRITE => assert(false)
          | Ev.SIGNAL => assert(false)
          | Ev.READ =>
            switch (Unix.getsockopt_error(fd)) {
            | Some(err) =>
              error(
                p,
                ~exn=[@implicit_arity] Unix.Unix_error(err, "connect", ""),
                "connect",
              )
            | None => k()
            }
          }
        ) {
        | exn => error(~exn, p, "connect")
        }
      );
      add_event(p, p.read, timeout);
    | exn => error(~exn, p, "connect")
    };
}; /* Peer */

let delay = (events, timeout, f, x) => {
  let timer = Ev.create();
  Ev.set_timer(
    events,
    timer,
    ~persist=false,
    () => {
      try(f(x)) {
      | exn => log#warn(~exn, "pause")
      };
      Ev.del(timer);
    },
  );
  Ev.add(timer, Some(timeout));
};

let poll = events => Ev.loop(events, Ev.NONBLOCK);
