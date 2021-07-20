open Prelude;
open Printf;
open ExtLib;

exception Error(string);

let fail = fmt => ksprintf(s => raise(Error(s)), fmt);

module Label: {
  type t = pri string;
  let make: string => t;
} = {
  type t = string;
  let make = s =>
    Stre.ASCII.(
      if (s != ""
          && is_alpha(s.[0])
          && List.for_all(c => c == '_' || is_alnum(c)) @@
          String.explode(s)) {
        s;
      } else {
        fail("bad label %S", s);
      }
    );
};

let make_value = (v, show: 'a => string, load: string => 'a) => {
  as _;
  val mutable contents = v;
  val mutable dirty = false;
  pub get = contents;
  pub set = y => {
    dirty = true;
    contents = y;
  };
  pub show = show(contents);
  pub load = s => {
    dirty = true;
    contents = load(s);
  };
  pub dirty = dirty;
  pub reset = {
    dirty = false;
    contents = v;
  }
};

type any_value = {
  .
  show: string,
  load: string => unit,
  dirty: bool,
  reset: unit,
};
type value('a) = {
  .
  get: 'a,
  set: 'a => unit,
  dirty: bool,
};

type group = {
  label: Label.t,
  groups: Hashtbl.t(Label.t, group),
  values: Hashtbl.t(Label.t, any_value),
  parent: option(group),
};

let group_name = g => {
  let rec loop = (acc, g) =>
    switch (g.parent) {
    | None => String.concat(".", List.rev(acc))
    | Some(g') => loop([(g.label :> string), ...acc], g')
    };

  loop([], g);
};

let value_name = (g, k: Label.t) =>
  switch (group_name(g)) {
  | "" => (k :> string)
  | s => s ++ "." ++ (k :> string)
  };

let make_node = (show, load, group, label, v: 'a) => {
  let label = Label.make(label);
  if (Hashtbl.mem(group.values, label)) {
    fail("duplicate label %S", (label :> string));
  };
  let v = make_value(v, show, load);
  Hashtbl.replace(group.values, label, (v :> any_value));
  (v :> value('a));
};

let int = make_node(string_of_int, int_of_string);
let long = make_node(Int64.to_string, Int64.of_string);
let string = make_node(id, id);
let float = make_node(string_of_float, float_of_string);
let bool =
  make_node(string_of_bool, s =>
    switch (String.lowercase(s)) {
    | "false"
    | "no" => false
    | "true"
    | "yes" => true
    | s => fail("not a boolean : %S", s)
    }
  );

let group = (parent, label) => {
  let label = Label.make(label);
  switch (Hashtbl.find_option(parent.groups, label)) {
  | Some(x) => x
  | None =>
    let group = {
      label,
      parent: Some(parent),
      groups: Hashtbl.create(1),
      values: Hashtbl.create(1),
    };
    Hashtbl.add(parent.groups, label, group);
    group;
  };
};

let new_root = () => {
  parent: None,
  groups: Hashtbl.create(1),
  values: Hashtbl.create(1),
  label: Label.make("whatever"),
};

let rec iter = (f, g) => {
  Hashtbl.iter((k, v) => f(value_name(g, k), v), g.values);
  Hashtbl.iter((_, g) => iter(f, g), g.groups);
};

let reset = iter((_, v) => v#reset);

let read = (root, s) => {
  reset(root);
  let store = (k, v) => {
    let rec loop = g =>
      fun
      | [name] => Hashtbl.find(g.values, name)
      | [x, ...xs] => loop(Hashtbl.find(g.groups, x), xs)
      | [] => fail("bad key %S", k);

    let o = loop(root, List.map(Label.make) @@ Stre.nsplitc(k, '.'));
    o#load(v);
  };

  let store = (k, v) =>
    try(store(k, v)) {
    /*     | Not_found -> prerr_endline (Printf.sprintf "Skipping unknown option : %S = %S" k v) */
    | exn =>
      fail("Failed to store option : %S = %S : %s", k, v, Exn.to_string(exn))
    };

  let io = IO.input_string(s);
  let line = ref(0);
  try(
    while (true) {
      switch (Exn.catch(IO.read_line, io)) {
      | None => raise(Exit)
      | Some(s) =>
        let s = s ++ "\n";
        incr(line);
        try(Scanf.sscanf(s, " #", ())) {
        | Scanf.Scan_failure(_)
        | End_of_file =>
          try(Scanf.sscanf(s, " %!", ())) {
          | Scanf.Scan_failure(_)
          | End_of_file =>
            try(
              Scanf.sscanf(s, "%s = %s@\n%!", (k, v) =>
                store(k, String.strip(v))
              )
            ) {
            | Scanf.Scan_failure(_)
            | End_of_file =>
              try(
                Scanf.sscanf(
                  s,
                  "%s := %c%s@\n%!",
                  (k, c, tail) => {
                    let pos = String.index(tail, c);
                    String.iter(
                      fun
                      | ' '
                      | '\t' => ()
                      | _ =>
                        fail("extra characters after %C-delimtied value", c),
                      String.slice(tail, ~first=pos + 1),
                    );
                    store(k, String.slice(tail, ~last=pos));
                  },
                )
              ) {
              | Scanf.Scan_failure(_)
              | End_of_file =>
                try(
                  Scanf.sscanf(
                    s,
                    "%s : %d\n%!",
                    (k, n) => {
                      assert(n >= 0);
                      let l =
                        List.init(
                          n + 1,
                          _ => {
                            incr(line);
                            IO.read_line(io);
                          },
                        );
                      store(k, String.concat("\n", l));
                    },
                  )
                ) {
                | Scanf.Scan_failure(_)
                | End_of_file => fail("can't parse line")
                }
              }
            }
          }
        };
      };
    }
  ) {
  | Exit => ()
  | exn =>
    let s =
      switch (exn) {
      | Failure(s) => s
      | Error(s) => s
      | exn => Exn.to_string(exn)
      };
    fail("error at line %d : %s", line^, s);
  };
};

let choose_quote = s => {
  let preferred = ['"', '\'', '`', '|', '!', '@', '#', '%'];
  let ok = Array.make(256, true);
  String.iter(c => ok[Char.code(c)] = false, s);
  try(Some(List.find(c => ok[Char.code(c)], preferred))) {
  | Not_found => None
  };
};

let show = (~all=false, root) => {
  let iter = f =>
    iter((name, v) =>
      if (v#dirty || all) {
        f(name, v#show);
      }
    );
  let b = Buffer.create(10);
  iter(
    (name, v) =>
      switch (
        String.fold_left(
          (n, c) =>
            if (c == '\n') {
              n + 1;
            } else {
              n;
            },
          0,
          v,
        )
      ) {
      | 0 =>
        if (String.starts_with(v, " ") || String.ends_with(v, " ")) {
          switch (choose_quote(v)) {
          | None => bprintf(b, "%s :%d\n%s\n", name, 0, v)
          | Some(c) => bprintf(b, "%s := %c%s%c\n", name, c, v, c)
          };
        } else {
          bprintf(b, "%s = %s\n", name, v);
        }
      | n => bprintf(b, "%s :%d\n%s\n", name, n, v)
      },
    root,
  );
  Buffer.contents(b);
};

let load = (root, file) => {
  reset(root);
  switch (Exn.catch(Std.input_file, file)) {
  | None => ()
  | Some(s) => read(root, s)
  };
};
let save = (~all=?, root, file) =>
  Files.save_as(file, ch => output_string(ch, show(~all?, root)));

class base (root, filename) = {
  as _;
  initializer (load(root, filename));
  pub save = () => save(root, filename);
  pub load = () => load(root, filename);
};
