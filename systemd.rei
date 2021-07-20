/**
   Misc utils for systemd.
*/;

/** Subset of [sd-daemon] in ocaml.

    The following functionalities are provided:
    - File descriptor passing for socket-based activation
    - Detection of systemd boots
*/

module Daemon: {
  /** [true] if the system was booted with systemd. */

  let booted: bool;

  /**
     Returns file descriptors that have been passed by systemd.

     This function call ensures that the [FD_CLOEXEC] flag is set for
     the passed file descriptors, to make sure they are not passed on
     to child processes. If [FD_CLOEXEC] shall not be set, the caller
     needs to unset it after this call for all file descriptors that
     are used.
  */

  let listen_fds: unit => list(Unix.file_descr);

  /** Same as {!listen_fds} but return lwt file descriptors. */

  let listen_fds_lwt: unit => list(Lwt_unix.file_descr);
};
