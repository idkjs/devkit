open ExtLib;

let log = Log.from("systemd");

module Daemon = {
  /* https://github.com/systemd/systemd/blob/cb3108669d623afe58a36077e36ae4c66ff7d1c3/src/systemd/sd-daemon.h#L56 */
  /* The first passed file descriptor is fd 3. */
  let sd_listen_fds_start = 3;

  let booted =
    /* https://github.com/systemd/systemd/blob/cb31086/src/libsystemd/sd-daemon/sd-daemon.c#L607 */
    try(
      {
        Unix.access("/run/systemd/system/", Unix.[F_OK]);
        true;
      }
    ) {
    | Unix.Unix_error(_) => false
    };

  let listen_pid = Option.map(int_of_string, Sys.getenv_opt("LISTEN_PID"));

  let listen_fds = (): list(Unix.file_descr) =>
    /* https://github.com/systemd/systemd/blob/cb31086/src/libsystemd/sd-daemon/sd-daemon.c#L42-L90 */
    switch (booted) {
    | false =>
      log#debug("listen_fds: not booted with systemd");
      [];
    | true =>
      switch (listen_pid) {
      | None =>
        log#debug("listen_fds: no LISTEN_PID");
        [];
      | Some(listen_pid) =>
        let self_pid = Unix.getpid();
        switch (listen_pid == self_pid) {
        | false =>
          log#warn(
            "listen_fds: LISTEN_PID %d and process pid %d are not equal, ignoring",
            listen_pid,
            self_pid,
          );
          [];
        | true =>
          let listen_fds =
            Option.map(int_of_string, Sys.getenv_opt("LISTEN_FDS"));
          switch (listen_fds) {
          | None =>
            log#warn("listen_fds: LISTEN_PID, but no LISTEN_FDS");
            [];
          | Some(n) when n <= 0 =>
            log#warn("listen_fds: LISTEN_FDS %d is not positive", n);
            [];
          | Some(n) =>
            let fds =
              List.init(n, x =>
                ExtUnix.All.file_descr_of_int(x + sd_listen_fds_start)
              );
            List.iter(Unix.set_close_on_exec, fds);
            fds;
          };
        };
      }
    };

  let listen_fds_lwt = () =>
    List.map(Lwt_unix.of_unix_file_descr, listen_fds());
};
