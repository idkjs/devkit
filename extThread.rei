/** Thread utilities */;

let locked: (Mutex.t, unit => 'a) => 'a;

type t('a);
let detach: ('a => 'b, 'a) => t('b);
let join: t('a) => Exn.result('a);
let join_exn: t('a) => 'a;

/** parallel Array.map */

let map: ('a => 'b, array('a)) => array('b);

/** parallel map with the specified number of workers, default=8 */

let mapn: (~n: int=?, 'a => 'b, list('a)) => list(Exn.result('b));

module LockMutex: {
  type t;
  let create: unit => t;
  let locked: (t, unit => 'a) => 'a;
};

/**
  Communication from worker threads to the main event loop
*/

module Async_fin: {
  type t;

  /** @return if OS has necessary support for this module */

  let is_available: unit => bool;

  let setup: Libevent.event_base => t;

  /** Destructor. All queued events are lost */

  let shutdown: t => unit;

  /** Arrange for callback to be executed in libevent loop, callback should not throw (exceptions are reported and ignored) */

  let callback: (t, unit => unit) => unit;
};

/** Create new thread wrapped in {!Action.log} */

let log_create: (~name: string=?, 'a => unit, 'a) => Thread.t;

/** run [f] in thread periodically once in [delay] seconds.
  @param f returns [false] to stop the thread, [true] otherwise
  @param now default [false]
*/

let run_periodic: (~delay: float, ~now: bool=?, unit => bool) => unit;

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

/** Thread workers */

module Workers:
  (T: WorkerT) => Workers with type task = T.task and type result = T.result;

module Pool: {
  type t;
  let create: int => t;
  let status: t => string;
  let put: (t, unit => unit) => unit;
  let wait_blocked: (~n: int=?, t) => unit;
};
