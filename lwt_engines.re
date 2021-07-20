open ExtLib;

module U = ExtUnix.Specific;
module Ev = Libevent;

class poll = {
  let readmask = U.Poll.(pollin + pollerr + pollhup + pollpri + pollrdhup);
  let writemask = U.Poll.(pollout + pollerr + pollhup);
  let convert = ((fd, i, o)) => (
    fd,
    U.Poll.((if (i) {pollin} else {none}) + (if (o) {pollout} else {none})),
  );
  as _;
  val mutable buffer = [||];
  inherit class Lwt_engine.poll_based;
  pub poll = (fds, timeout) => {
    /*
       let show = Stre.list (fun (fd,i,o) -> sprintf "%d%s%s" (U.int_of_file_descr fd) (if i then "r" else "") (if o then "w" else "")) in
       log #info "lwt poll %f %s" timeout (show fds);
     */
    let nfds = List.length(fds);
    if (nfds <= Array.length(buffer) && nfds * 2 > Array.length(buffer)) {
      List.iteri((i, x) => buffer[i] = convert(x), fds);
    } else {
      buffer = Array.of_list @@ List.map(convert, fds);
    };

    let timeout =
      if (timeout < 0.) {
        (-1.) /. 1000.;
      } else {
        timeout;
      };

    let l =
      U.poll(buffer, ~n=nfds, timeout)
      |> List.map(((fd, f)) =>
           (fd, U.Poll.is_inter(f, readmask), U.Poll.is_inter(f, writemask))
         );
    /*   log #info "lwt poll done %s" (show l); */
    l;
  };
};

/** libevent-based engine for lwt */
class libevent = {
  let once_block = Ev.[ONCE];
  let once_nonblock = Ev.[ONCE, NONBLOCK];
  as self;
  inherit class Lwt_engine.abstract;
  val events_ = Ev.init();
  val mutable pid = Unix.getpid();
  pub events = {
    if (Unix.getpid() != pid) {
      pid = Unix.getpid();
      Ev.reinit(events_);
    };
    events_;
  };
  pri cleanup = Ev.free(events_);
  pub iter = block =>
    try(Ev.(loops(self#events, if (block) {once_block} else {once_nonblock}))) {
    | exn => Exn.fail(~exn, "Lwt_engines.libevent#iter")
    };
  pri register_readable = (fd, f) => {
    let ev = Ev.create();
    Ev.set(self#events, ev, fd, [Ev.READ], ~persist=true, (_, _) => f());
    Ev.add(ev, None);
    lazy(Ev.del(ev));
  };
  pri register_writable = (fd, f) => {
    let ev = Ev.create();
    Ev.set(self#events, ev, fd, [Ev.WRITE], ~persist=true, (_, _) => f());
    Ev.add(ev, None);
    lazy(Ev.del(ev));
  };
  pri register_timer = (delay, repeat, f) => {
    let ev = Ev.create();
    let stop = ref(false);
    Ev.set_timer(
      self#events,
      ev,
      ~persist=false,
      () => {
        if (! stop^) {
          f();
        };
        if (repeat && ! stop^) {
          Ev.add(ev, Some(delay));
        };
      },
    );
    Ev.add(ev, Some(delay));
    lazy(
      {
        stop := true;
        Ev.del(ev);
      }
    );
  };
};
