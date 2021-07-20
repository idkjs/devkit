open ExtLib;
open Printf;

type t = {
  host: string,
  id: int,
  name: string,
  stamp: int,
};

let dummy = {host: "*", id: 0, name: "*", stamp: 0};

let show = ({id, name, host, stamp: _}) =>
  sprintf("%u:%s@%s", id, name, host);
let to_string = ({id, name, host, stamp}) =>
  sprintf("%u:%u:%s@%s", stamp, id, name, host);
let compare = (pid1: t, pid2) => compare(pid1, pid2);
let equal = (pid1, pid2) => 0 == compare(pid1, pid2);
let name = ({name, _}) => name;
let host = ({host, _}) => host;

let get_name = s =>
  try(fst @@ String.split(s, ".")) {
  | _ => s
  };
let validate_name = (descr, s) =>
  try(Scanf.sscanf(s, "%_[a-zA-Z0-9_.-]%!", ())) {
  | _ => Exn.fail("Pid.self: bad %s %S", descr, s)
  };
let sanitize_name =
  String.map(
    fun
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.') as c => c
    | _ => '_',
  );

let parse_exn = s =>
  /* cf self_pid */
  Scanf.sscanf(
    s,
    "%u:%u:%[a-zA-Z0-9_.-]@@%[a-zA-Z0-9_.-]%!",
    (stamp, id, name, host) => {
      if (host == "") {
        Exn.fail("empty hostname");
      };
      if (name == "") {
        Exn.fail("empty name");
      };
      {
        id,
        host: String.lowercase(host),
        name: get_name @@ String.lowercase(name),
        stamp,
      };
    },
  );

let make = (~id, ~host, ~stamp, name) => {
  validate_name("name", name);
  validate_name("host", host);
  {id, host, stamp, name};
};

let new_self = (name, stamp) => {
  let id = Unix.getpid();
  let host = String.lowercase @@ Unix.gethostname();
  /* cf parse_exn */
  validate_name("host", host);
  validate_name("name", name);
  {host, id, name: get_name @@ String.lowercase(name), stamp};
};

let self = ref @@ dummy;
let self_s = ref @@ "";

let assign_self = t => {
  self := t;
  self_s := show(t);
};

let update_self = (name, fresh) =>
  assign_self @@
  new_self(
    name,
    if (fresh) {
      Time.(int @@ now());
    } else {
      self^.stamp;
    },
  );

let () =
  update_self(sanitize_name @@ Filename.basename(Sys.executable_name), true);

let fake_id = ref(false);

let set_name = name => {
  validate_name("name", name);
  update_self(name, fake_id^);
  fake_id := false;
};

let update = () =>
  fake_id^
    ? ()
    : self^.id == Unix.getpid()
        ? ()  /* fork */
        : update_self(self^.name, true);

let set_fake = t => {
  assign_self(t);
  fake_id := true;
};

let self = () => {
  update();
  self^;
};
let self_name = () => name @@ self();
let show_self = () => {
  update();
  self_s^;
};
let self_as = name => {
  set_name(name);
  self();
};
