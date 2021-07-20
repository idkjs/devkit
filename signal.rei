/** Signal handling */;

/** {2 libevent + signalfd}

  explicit interface (for compatibility)
*/;

type t;
let init: Async.Ev.event_base => t;
let stop: t => unit;

/** {2 generic registration} */;

let is_safe_output: unit => bool;

/** add signal handler for specified signals */

let set: (list(int), int => unit) => unit;
let set1: (int, unit => unit) => unit;
let set_exit: (unit => unit) => unit;
let set_reload: (unit => unit) => unit;

/** replace signal handler for specified signals */

let replace: (list(int), int => unit) => unit;

/** setup "standard" signal driver, deadlock-friendly, default */

let setup_sys: unit => unit;

/** setup signals via libevent (signalfd), requires event loop */

let setup_libevent: t => unit;

let setup_libevent_: Async.Ev.event_base => unit;
let setup_libevent': t => unit;

/** setup signals via lwt, requires {!Lwt_main.run} */

let setup_lwt: unit => unit;

type state;
let save: unit => state;
let restore: state => unit;
