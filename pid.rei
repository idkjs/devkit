/** Unique process identification */;

type t = {
  /** machine hostname (no spaces allowed) */
  host: string,
  /** process id */
  id: int,
  /** application id (no spaces allowed), for information. */
  name: string,
  /** stamp for uniqueness to guard against pid reuse */
  stamp: int,
};

/** dummy instance, use sparingly */

let dummy: t;

/** @return pretty-printed pid (human readable) */

let show: t => string;

/** @return machine hostname */

let host: t => string;

/** @return application name */

let name: t => string;

/** @return string representation of pid, can be read back by [parse_pid_exn] */

let to_string: t => string;

let make: (~id: int, ~host: string, ~stamp: int, string) => t;

let compare: (t, t) => int;
let equal: (t, t) => bool;

let parse_exn: string => t;

/** {1 Current process identifier} */;

let set_name: string => unit;
let self: unit => t;
let self_name: unit => string;
let self_as: string => t;
let show_self: unit => string;


let set_fake: t => unit;
