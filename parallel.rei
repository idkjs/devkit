/** Parallel */;

/** Invoke function in a forked process and return result */

let invoke: ('a => 'b, 'a, unit) => 'b;

/** Launch function for each element of the list in the forked process.
  Does not wait for children to finish - returns immediately. */

let launch_forks: ('a => unit, list('a)) => unit;

/** Launch forks for each element of the list and wait for all workers to finish.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
  @param revive to keep workers running (restarting with same param if exited) [default: false]
*/

let run_forks:
  (
    ~wait_stop: int=?,
    ~revive: bool=?,
    ~wait: int=?,
    ~workers: int=?,
    'a => unit,
    list('a)
  ) =>
  unit;

/** Same as [run_forks] but do not fork for one worker */

let run_forks': ('a => unit, list('a)) => unit;

/** Process list with specified number of workers.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
*/

let run_workers: (int, ~wait_stop: int=?, 'a => unit, list('a)) => unit;

/** Process enum with specified number of workers, collect results via provided callback.
  Pass exit signals to the workers, see {!Forks.stop} for the description of [wait_stop] parameter.
*/

let run_workers_enum:
  (int, ~wait_stop: int=?, 'a => 'b, 'b => unit, Enum.t('a)) => unit;

module type WorkerT = {
  type task;
  type result;
};

module type Workers = {
  type task;
  type result;
  type t;

  /** [create f n] starts [n] parallel workers waiting for tasks */

  let create: (task => result, int) => t;

  /** [perform workers tasks f] distributes [tasks] to all [workers] in parallel,
    collecting results with [f] and returns when all [tasks] are finished */

  let perform: (t, ~autoexit: bool=?, Enum.t(task), result => unit) => unit;

  /** [stop ?wait workers] kills worker processes with SIGTERM
  is [wait] is specified it will wait for at most [wait] seconds before killing with SIGKILL,
  otherwise it will wait indefinitely
  @param autoexit determines whether workers will exit once there are no more tasks, it means [perform] shouldn't be called again
    for this instance
*/

  let stop: (~wait: int=?, t) => unit;
}; /* Workers */

/*
 val create : ('a -> 'b) -> int -> ('a,'b) t
 val perform : ('a,'b) t -> 'a Enum.t -> ('b -> unit) -> unit
 */

/** Forked workers */

module Forks:
  (T: WorkerT) => Workers with type task = T.task and type result = T.result;

module Services: {
  type t;

  let start: (int, int => Lwt.t(unit)) => Lwt.t(t);

  let rolling_restart: (~wait: int=?, ~timeout: float, t) => Lwt.t(unit);

  let stop: (~timeout: float, t) => Lwt.t(unit);
};
