/**
  *nix support
*/;

open Unix;
open Printf;

open Control;
open Prelude;
open ExtLib;

let log = Log.from("nix");

let fork = () =>
  switch (Lwt_unix.fork()) {
  | (-1) => Exn.fail("failed to fork")
  | 0 =>
    Random.self_init();
    `Child;
  | pid => `Forked(pid)
  };

/** fork off and die */

let unparent = () =>
  switch (fork()) {
  | `Child => ()
  | `Forked(_) => exit(0)
  };

/*
   http://www.itp.uzh.ch/~dpotter/howto/daemonize
 */

let daemonize = () => {
  unparent();
  if (setsid() < 0) {
    failwith("Can't setsid");
  };
  /* Ignore TTY signals, SIGHUP and SIGPIPE */

  List.iter(
    n => Sys.set_signal(n, Sys.Signal_ignore),
    [Sys.sigtstp, Sys.sigttou, Sys.sigttin, Sys.sighup, Sys.sigpipe],
  ) /*   chdir "/"; */ /*   umask 0; */; /* TODO investigate */ /* TODO this will break lots of code - fix */
  /* redirect standard channels */
  let null = openfile("/dev/null", [O_RDWR], 0);
  dup2(null, stdin);
  dup2(null, stdout);
  dup2(null, stderr);
  close(null);
  ();
};

let write_pidfile = path =>
  Control.bracket(
    open_out_gen(
      [Open_wronly, Open_creat, Open_excl, Open_text],
      0o644,
      path,
    ),
    close_out,
    ch =>
    try(
      {
        Printf.fprintf(ch, "%u\n", getpid());
        flush(ch);
      }
    ) {
    | exn =>
      log#warn(~exn, "cannot write pidfile %s, will remove", path);
      Sys.remove(path);
      raise(exn);
    }
  );

let read_pidfile = path =>
  Control.with_open_in_txt(
    path,
    ch => {
      let ib = Scanf.Scanning.from_channel(ch);
      Scanf.bscanf(ib, " %u ", Prelude.id);
    },
  );

let probe_pidfile = path =>
  if (Sys.file_exists(path)) {
    try({
      let pid = read_pidfile(path);
      kill(pid, 0);
      `Alive(pid);
    }) {
    |  Unix_error(ESRCH, _, _) => `Stale
    | e => `Error(e)
    };
  } else {
    `Missing;
  };

let check_pidfile = path =>
  switch (probe_pidfile(path)) {
  | `Missing => () /* free to go */
  | `Stale =>
    log#info("removing stale pidfile at %s", path);
    Sys.remove(path);
  | `Alive(pid) =>
    log#info("pid %d at %s is alive, exiting", pid, path);
    exit(133);
  | `Error(exn) =>
    log#warn(~exn, "wrong pid file at %s, exiting", path);
    exit(3);
  };

let manage_pidfile = path => {
  check_pidfile(path);
  write_pidfile(path);
  let pid = getpid();
  at_exit(() =>
    if (getpid() == pid) {Exn.suppress(Sys.remove, path);} /* else forked child */
  );
};

let restart = (f, x) => {
  let rec loop = () =>
    try(f(x)) {
    |  Unix.Unix_error(EINTR, _, _) => loop()
    };
  loop();
};

/** NB be careful with mutexes in signal handlers.
    Outputting anything to ocaml channels is a potential deadlock.
    Use signalfd which invokes signal handlers in predictable context.
    @deprecated easy to deadlock, use signalfd instead
*/

let handle_sig_exit_with = (~exit, fin) =>
  List.iter(
    signal =>
      Sys.set_signal(
        signal,
        Sys.Signal_handle(
          _signo => {
            /*         log #info "Received signal %i (exit)..." n; */
            try(fin()) {
            | exn => log#warn(~exn, "handle_sig_exit")
            };
            /*         log #info "Signal handler done.%s" (if exit then " Exiting." else ""); */
            if (exit) {
              Stdlib.exit(0);
            };
          },
        ),
      ),
    [Sys.sigint, Sys.sigterm],
  );

/**
  @deprecated easy to deadlock, use signalfd instead
*/

let handle_sig_reload_with = fin =>
  List.iter(
    signal =>
      Sys.set_signal(
        signal,
        Sys.Signal_handle(
          _signo =>
            /*         log #info "Received signal %i (reload)..." n;  */
            try(fin()) {
            | exn => log#warn(~exn, "handle_sig_reload")
            },
            /*         log #info "Signal handler done." */
        ),
      ),
    [Sys.sighup],
  );

let show_addr =
  fun
  | ADDR_UNIX(s) => sprintf("unix:%s", s)
  |  ADDR_INET(addr, port) =>
    sprintf("%s:%u", string_of_inet_addr(addr), port);

let get_inet_addr_exn =
  fun
  |  ADDR_INET(addr, _) => addr
  | addr => Exn.fail("get_inet_addr %s", show_addr(addr));

let show_inet_addr_exn = addr =>
  string_of_inet_addr(get_inet_addr_exn(addr));

let make_inet_addr_exn = (host, port) => {
  let a = gethostbyname(host).h_addr_list;
  if (Array.length(a) == 0) {
    Exn.fail("make_inet_addr %s %d", host, port);
  } else {
     ADDR_INET(a[0], port);
  };
};

let inet_addr_of_string = s =>
  Unix.(
    try(
      if (String.contains(s, ':')) {
        let (host, port) = String.split(s, ":");
        let port = int_of_string(port);
        switch (host) {
        | "*" =>  ADDR_INET(inet_addr_any, port)
        | host => make_inet_addr_exn(host, port)
        };
      } else {
        let port = int_of_string(s);
         ADDR_INET(inet_addr_loopback, port);
      }
    ) {
    | _ =>
      /* The port or the host is invalid */
      Exn.fail("invalid INET addr %S", s)
    }
  );

let unix_addr_of_string = s =>
  Unix.(
    if (String.starts_with(s, "unix:")) {
      ADDR_UNIX(String.slice(~first=5, s));
    } else {
      Exn.fail("invalid UNIX addr %S", s);
    }
  );

let parse_addr_port_exn = s =>
  switch (Stre.splitc(s, ':')) {
  | exception Not_found => Exn.fail("bad host in %S (must be host:port)", s)
  | (host, port) =>
    let port =
      try(int_of_string(port)) {
      | exn => Exn.fail(~exn, "bad port %s in %S", port, s)
      };
    (host, port);
  };

/** Parse input as [ip:port]
  @return a tuple representing ip and port */

let parse_ip_port_exn = s => {
  let (ip, port) = parse_addr_port_exn(s);
  let ip =
    try(Unix.inet_addr_of_string(ip)) {
    | exn => Exn.fail(~exn, "bad ip %s in %S", ip, s)
    };
  (ip, port);
};

/**
   Convert a string to a {Unix.sockaddr}.

   Formats supported are:
   - unix:file_path
   - host:port
   - *:port, using {Unix.inet_addr_any}
   - port, using {Unix.inet_addr_loopback}
 */

let sockaddr_of_string = s =>
  try(unix_addr_of_string(s)) {
  | Failure(_) =>
    try(inet_addr_of_string(s)) {
    | Failure(_) => Exn.fail("sockaddr_of_string %s", s)
    }
  };

/** Execute process and capture stdout to string, @return empty string on error */

let read_process = cmd =>
  try({
    let cin = Unix.open_process_in(cmd);
    let input = IO.input_channel(cin);
    let data = IO.read_all(input);
    IO.close_in(input);
    ignore(Unix.close_process_in(cin));
    data;
  }) {
  | _ => ""
  };

module Ev = Libevent;

/** @return IO.t to feed stdin of spawned process */

let output_process_exit = cmd => {
  let cout = Unix.open_process_out(cmd);
  let close = () => Unix.close_process_out(cout);
  IO.create_out(
    ~write=output_char(cout),
    ~output=
      (s, o, l) => {
        output(cout, s, o, l);
        l;
      },
    ~flush=() => flush(cout),
    ~close,
  );
};

/** @return IO.t to feed stdin of spawned process */

let output_process = cmd => {
  let cout = Unix.open_process_out(cmd);
  let close = () =>
    switch (Unix.close_process_out(cout)) {
    | Unix.WEXITED(0) => ()
    | Unix.WEXITED(n) => Exn.fail("Command \"%s\": Exit code %u", cmd, n)
    | Unix.WSIGNALED(n)
    | Unix.WSTOPPED(n) =>
      Exn.fail("Command \"%s\": Terminated with signal %u", cmd, n)
    };

  IO.create_out(
    ~write=output_char(cout),
    ~output=
      (s, o, l) => {
        output(cout, s, o, l);
        l;
      },
    ~flush=() => flush(cout),
    ~close,
  );
};

let write_process_exn = (cmd, data) =>
  with_output(
    output_process(cmd),
    out => {
      IO.nwrite(out, data);
      IO.flush(out);
    },
  );

let write_process = (cmd, data) =>
  try(
    {
      write_process_exn(cmd, data);
      true;
    }
  ) {
  | _ => false
  };

let mounts = () =>
  with_open_in_txt("/proc/mounts") @@
  (
    ch =>
      Std.input_lines(ch)
      |> Enum.filter_map(s =>
           switch (String.nsplit(s, " ")) {
           | ["rootfs", _, "rootfs", _, _, _] => None
           | [dev, mount, _fs, opt, _, _] =>
             Some((dev, mount, String.nsplit(opt, ",")))
           | _ => Exn.fail("bad mount : %s", s)
           }
         )
      |> List.of_enum
  );

/** @param path must be normalized */

let find_mount = path => {
  assert((!) @@ Filename.is_relative(path));
  assert((!) @@ String.exists(path, "//"));
  assert((!) @@ String.exists(path, "/./"));
  assert((!) @@ String.exists(path, "/../"));
  let mount = ref(("", "", []));
  let bound = x => {
    let (_, b, _) = x;
    b;
  };
  mounts()
  |> List.iter(((_, bind, _) as part) =>
       if (String.starts_with(path, bind)
           && String.length(bind) > String.length(bound(mount^))) {
         mount := part;
       }
     );
  assert(bound(mount^) != "");
  mount^;
};

/* in seconds */
let sleep = Unix.sleepf;

/**
  Buffered output to [Unix.file_descr].
  Doesn't own the file descriptor.
*/

let output_buf_fd = (~bufsize=1 * 1024 * 1024, fd) => {
  if (bufsize <= 0) {
    Exn.invalid_arg("output_fd: bufsize %d", bufsize);
  };
  let buf = Bytes.create(bufsize);
  let len = ref(0);
  let flush = () =>
    switch (len^) {
    | 0 => ()
    | _ =>
      let written = Unix.write(fd, buf, 0, len^);
      if (len^ != written) {
        Exn.fail("output_fd: flush failed: %d <> %d", len^, written);
      };
      len := 0;
    };

  let check_flush = () =>
    if (len^ == bufsize) {
      flush();
    };
  let rec output = (s, p, l) =>
    if (l + len^ > bufsize) {
      let miss = bufsize - len^;
      Bytes.blit(s, p, buf, len^, miss);
      len := bufsize;
      flush();
      output(s, p + miss, l - miss);
    } else {
      Bytes.blit(s, p, buf, len^, l);
      len := len^ + l;
      check_flush();
    };

  IO.create_out(
    ~write=
      c => {
        buf.[len^] = c;
        incr(len);
        check_flush();
      },
    ~output=
      (s, p, l) => {
        output(s, p, l);
        l;
      },
    ~flush,
    ~close=flush,
  );
}; /* do not close file descriptor, flush the buffer */

let unlimit_soft = r => {
  let (soft, hard) = U.getrlimit(r);
  try(U.setrlimit(r, ~soft=hard, ~hard)) {
  |  Unix_error((EPERM | EINVAL) as error, _, _)
      when r == U.RLIMIT_NOFILE =>
    log#warn(
      "failed to unlimit NOFILE %s -> %s : %s (check kernel limits fs.nr_open/kern.maxfilesperproc/etc), ignored",
      U.Rlimit.to_string(soft),
      U.Rlimit.to_string(hard),
      error_message(error),
    )
  };
};

/** raise core and nofile soft limits (to the corresponding hard limits) */

let raise_limits = () => {
  unlimit_soft(U.RLIMIT_CORE);
  unlimit_soft(U.RLIMIT_NOFILE);
};

let connect = (fd, sockaddr) =>
  Unix.(
    try(connect(fd, sockaddr)) {
    |  Unix_error(e, f, "") =>
      raise( Unix_error(e, f, show_addr(sockaddr)))
    }
  );

let connect_lwt = (fd, sockaddr) =>
  Lwt_unix.(
    Lwt.catch(
      () => connect(fd, sockaddr),
      fun
      |  Unix_error(e, f, "") =>
        Lwt.fail( Unix_error(e, f, show_addr(sockaddr)))
      | exn => Lwt.fail(exn),
    )
  );

let get_xdg_dir = (~env, dir) =>
  try(Sys.getenv(env)) {
  | Not_found =>
    try(sprintf("%s/.%s", Sys.getenv("HOME"), dir)) {
    | Not_found => Exn.fail("get_xdg_dir: unable to find %s directory", dir)
    }
  };

let xdg_cache_dir = lazy(get_xdg_dir(~env="XDG_DATA_CACHE", "cache"));
let xdg_config_dir = lazy(get_xdg_dir(~env="XDG_CONFIG_HOME", "config"));

let quote_if_needed = s =>
  try(Scanf.sscanf(s, "%_[a-zA-Z0-9:_/.-]%!", s)) {
  | _ => Filename.quote(s)
  };

let args = List.tl(Array.to_list(Sys.argv)); /* Action.args */
let cmdline =
  String.concat(" ") @@ List.map(quote_if_needed) @@ Array.to_list(Sys.argv);
