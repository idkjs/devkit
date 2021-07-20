open Printf;

open Prelude;

include Arg;

let describe = (t, name) =>
  fun
  | "" => sprintf("<%s> %s", t, name)
  | s when s.[0] == ' ' => sprintf("<%s>%s", t, s)
  | s => s;

let make_arg = (x, name, var, desc) => (
  "-" ++ name,
  x#store(var),
  sprintf("%s (default: %s)", describe(x#kind, name, desc), x#show(var)),
);

let test_int = f => {
  as _;
  pub store = v =>
    Arg.Int(
      x => {
        if (!f(x)) {
          Exn.fail("Bad value %d", x);
        };
        v := x;
      },
    );
  pub kind = "int";
  pub show = v => string_of_int(v^)
};

let int = {
  as _;
  pub store = v => Arg.Set_int(v);
  pub kind = "int";
  pub show = v => string_of_int(v^)
};

let float = {
  as _;
  pub store = v => Arg.Set_float(v);
  pub kind = "float";
  pub show = v => string_of_float(v^)
};

let string = {
  as _;
  pub store = v => Arg.Set_string(v);
  pub kind = "string";
  pub show = v => v^
};

let duration = {
  as _;
  pub store = v => Arg.String(s => v := Time.of_compact_duration(s));
  pub kind = "duration";
  pub show = v => Time.compact_duration(v^)
};

let int_option = {
  as _;
  pub store = v => Arg.Int(x => v := Some(x));
  pub kind = "int";
  pub show = v => Option.map_default(string_of_int, "none", v^)
};

let float_option = {
  as _;
  pub store = v => Arg.Float(x => v := Some(x));
  pub kind = "float";
  pub show = v => Option.map_default(string_of_float, "none", v^)
};

let str_option = {
  as _;
  pub store = v => Arg.String(x => v := Some(x));
  pub kind = "string";
  pub show = v => Option.map_default(id, "none", v^)
};

let int = make_arg(int);
let float = make_arg(float);
let str = make_arg(string);
let duration = make_arg(duration);
let may_int = make_arg(int_option);
let may_float = make_arg(float_option);
let may_str = make_arg(str_option);
let positive_int = make_arg(test_int(x => x > 0));

let bool = (name, var, desc) => (
  "-" ++ name,
  Arg.Set(var),
  if (desc == "") {
    sprintf(" enable %s", name);
  } else if (desc.[0] != ' ') {
    " " ++ desc;
  } else {
    desc;
  },
);

let usage_header = "Available options are:";

let align = (~sep="#", args) => {
  open ExtString;
  let convert = (~sub, ~by, (a, b, doc)) => {
    let doc: doc =
      try(
        if (doc == "" || doc.[0] == ' ') {
          doc;
        } else {
          let (left, right) = String.split(doc, by);
          Stre.replace_all(~str=left, ~sub, ~by) ++ " " ++ right;
        }
      ) {
      | Invalid_string => doc
      };

    (a, b, doc);
  };

  args
  |> List.map(convert(~sub=" ", ~by=sep))
  |> align
  |> List.map(convert(~sub=sep, ~by=" "));
};

let parse = (~f=?, args) => {
  let f =
    Option.default(
      s => Exn.fail("unrecognized argument %S, try \"-help\"", s),
      f,
    );
  parse(align(args), f, usage_header);
};

let usage = args => Arg.usage(align(args), usage_header);

/*
   "-"^name,
   Arg.Set_int var,
   sprintf "%s (default: %i)" (describe "int" name desc) !var
 */

/*
 let arg_str name ?desc var =
   "-"^name,
   Arg.Set_string var,
   sprintf "%s (default: %s)" (describe "string" name desc) !var
 */

let two_strings = k => {
  let old = ref("");
  Arg.Tuple([Arg.String(x => old := x), Arg.String(s => k(old^, s))]);
};

let rest = () => {
  let n = Array.length(Sys.argv);
  if (Arg.current^ >= n) {
    [];
  } else {
    Array.to_list @@
    Array.sub(
      Sys.argv,
      Arg.current^ + 1,
      Array.length(Sys.argv) - Arg.current^ - 1,
    );
  };
};
