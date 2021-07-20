/** Static mapping of simple config format.

Format is simple key-value pairs. Key must start with a letter and may include letters, numbers and underscore.
Lines starting with '#' are treated as comments, i.e. ignored.
Value can be arbitrary - there are several ways to represent them :

* if the value doesn't contain any spaces, just write out the value directly

  <key> = <value>

* if the value is single-line, wrap it with any symbol (<DELIMITER> can be a quote, a doublequote or
  any other character that doesn't occur in the value itself)

  <key> := <DELIMITER><value><DELIMITER>

* multi-line symbols are written verbatim prefixed with the number of lines occupied

  <key> : <N>
  <line 1>
  <line 2>
  [...]
  <line N>


Example usage:

  module CONF = struct
    open Static_config

    (* fresh new group *)
    let root = new_root ()
    let save = save root
    let load = load root

    (* values stored *)
    let last_id = int root "last_id" 0
    let last_key = string root "last_key" ""
  end

or as an object:

  let simple_config filename =
    let open Static_config in
    let root = new_root () in
    let port = int root "port" 8080 in
    object
      inherit base root filename
      method port = port
    end

  let conf = simple_config "some.config" in (* get's loaded here *)
  conf#port#set 8081;
  conf#save ()

*/;

exception Error(string);

type value('a) = {
  .
  get: 'a,
  set: 'a => unit,
  dirty: bool,
};
type group;

let group: (group, string) => group;

let new_root: unit => group;

let int: (group, string, int) => value(int);
let long: (group, string, int64) => value(int64);
let string: (group, string, string) => value(string);
let float: (group, string, float) => value(float);
let bool: (group, string, bool) => value(bool);

let show: (~all: bool=?, group) => string;
let read: (group, string) => unit;

let reset: group => unit;
let load: (group, string) => unit;
let save: (~all: bool=?, group, string) => unit;

class base:
  (group, string) =>
  {
    pub load: unit => unit;
    pub save: unit => unit;
  };
