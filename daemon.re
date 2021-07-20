/** daemon utilities */;

module U = ExtUnix.Specific;

let log = Log.from("daemon");

let logfile = ref(None);
let pidfile = ref(None);
let runas = ref(None);
let foreground = ref(false);

let managed = ref(false);

/** global flag indicating that process should exit,
    [manage] will automatically set this flag on SIGTERM unless default signal handling is overriden
*/

let should_exit_ = ref(false);

/** [should_exit_lwt] usage is discouraged.
    Use [wait_exit] instead, which makes it harder to ignore "should exit" state and loop infinitely
*/

let (should_exit_lwt, signal_exit_lwt) = Lwt.wait();
let should_exit = () => should_exit_^;
let should_run = () => ! should_exit_^;

/** exception to be raised by functions that wish to signal premature termination due to [!should_exit = true] */

exception ShouldExit;

let signal_exit = {
  let do_lwt = lazy(Lwt.wakeup_later(signal_exit_lwt, ()));
  /* invariant: should_exit_ = (Lwt.state should_exit_lwt = Lwt.Return) */
  () => {
    should_exit_ := true;
    Lazy.force(do_lwt);
  };
};

/** @raise ShouldExit if [should_exit] condition is set, otherwise do nothing */

let break = () =>
  if (should_exit_^) {
    raise(ShouldExit);
  };

/** wait until [should_exit] is set and raise [ShouldExit] */

let wait_exit = {
  /* NOTE
       Bind to should_exit_lwt only once, because every bind will create an immutable waiter on
       should_exit_lwt's sleeper, that is only removed after should_exit_lwt thread terminates.
     */
  let thread = lazy(Lwt.bind(should_exit_lwt, () => Lwt.fail(ShouldExit)));
  () => Lazy.force(thread);
};

/** [break_lwt = Lwt.wrap break] */

let break_lwt = () => Lwt.wrap(break);

/** [unless_exit x] resolves promise [x] or raises [ShouldExit] */

let unless_exit = x => Lwt.pick([wait_exit(), x]);

let get_args = () => [
  (
    "-loglevel",
    Arg.String(Log.set_loglevels),
    " ([<facil|prefix*>=]debug|info|warn|error[,])+",
  ),
  ExtArg.may_str("logfile", logfile, "<file> Log file"),
  ExtArg.may_str("pidfile", pidfile, "<file> PID file"),
  (
    "-runas",
    Arg.String(
      name =>
        try(runas := Some(Unix.getpwnam(name))) {
        | exn => Exn.fail(~exn, "runas: unknown user %s", name)
        },
    ),
    "<user> run as specified user",
  ),
  ("-fg", Arg.Set(foreground), " Stay in foreground"),
];

let args = get_args();

let install_signal_handlers = () => {
  let unix_stderr = s => {
    let s = Log.State.format_simple(`Info, log#facility, s);
    try({
      let _: int = Unix.write_substring(Unix.stderr, s, 0, String.length(s));
      ();
    }) {
    | _ => ()
    };
  }; /* do not fail, can be ENOSPC */

  Signal.set([Sys.sigpipe], ignore);
  Signal.set([Sys.sigusr1], _ => Log.reopen(logfile^));
  Signal.set([Sys.sigusr2], _ =>
    Signal.is_safe_output()
      ? {
        Memory.log_stats();
        Memory.reclaim();
      }
      /* output directly to fd to prevent deadlock, but breaks buffering */
      : {
        Memory.get_stats() |> List.iter(unix_stderr);
        Memory.reclaim_s() |> unix_stderr;
      }
  );
  Signal.set_exit(signal_exit);
};

let manage = () =>
  managed^
    ? ()  /* be smart */
    /*
       this will fail if files don't exists :(
       (* fail before fork if something is wrong *)
       Option.may (fun path -> Unix.(access path [R_OK;W_OK])) !logfile;
       Option.may (fun path -> Unix.(access path [R_OK;W_OK])) !pidfile;
     */
    : {
      Option.may(Nix.check_pidfile, pidfile^); /* check pidfile before fork to fail early */
      if (! foreground^) {
        Nix.daemonize();
      };
      switch (runas^) {
      | None => ()
      | Some(pw) =>
        let uid = pw.Unix.pw_uid
        and gid = pw.Unix.pw_gid;
        U.setreuid(uid, uid);
        U.setregid(gid, gid);
      };
      Log.reopen(logfile^); /* immediately after fork */
      Log.read_env_config();
      Option.may(Nix.manage_pidfile, pidfile^); /* write pidfile after fork! */
      if (Option.is_some(logfile^)) {
        log#info("run: %s", Nix.cmdline);
        log#info("GC settings: %s", Action.gc_settings());
      };
      install_signal_handlers();
      Nix.raise_limits();
      managed := true;
      ();
    };
