type json = [ | `Float(float) | `Int(int) | `String(string)];

/** Export counters registered with {!Var} as logstash events */

let get: unit => list([> | `Assoc(list((string, [> json])))]);

/** Setup periodic saving of counters as logstash json rows along the logfile */

let setup: (~pause: Time.t=?, Libevent.event_base) => unit;
let setup_lwt: (~pause: Time.t=?, unit) => unit;

type logger = {
  .
  /** write event manually */ event: list((string, Yojson.Safe.t)) => unit,
  /** write Var counters explicitly */ write: unit => unit,
  /** reopen output file */ reload: unit => unit,
  /** force flush */ flush: unit => unit,
};

/* Setup logger for a stream of events */
let log:
  (
    ~autoflush: float=?,
    ~verbose: bool=?,
    ~add_timestamp_only: bool=?,
    ~name: string=?,
    unit
  ) =>
  logger;

let setup_error_log: unit => unit;

/** Counters with arbitrary attributes */

module Dyn: {
  type t = pri list((string, json));
  let make: (~attrs: list((string, json))=?, string) => t;
  /* val add : t -> ?attrs:(string * string) list -> Var.t -> unit */
  /* val set : t -> ?attrs:(string * string) list -> Var.t -> unit */
  let set_count: (t, list((string, json)), int) => unit;
  let set_bytes: (t, list((string, json)), int) => unit;
  let set_time: (t, list((string, json)), Time.t) => unit;
  let add_count: (t, list((string, json)), int) => unit;
  let add_bytes: (t, list((string, json)), int) => unit;
  let add_time: (t, list((string, json)), Time.t) => unit;
};

/** Log events related to the life of the program:
    - [start]
    - [signal.stop]
    - [exit]
*/

let lifetime:
  (~extra: string=?, ~events: logger, ~version: string, unit) => unit;
