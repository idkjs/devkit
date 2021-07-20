/** Useful shortcuts */;

module U = ExtUnix.Specific;
module Enum = ExtEnum;

let ($) = (f, g, x) => f(g(x));
let ($$) = (f, g, x, y) => f(g(x), g(y));
let (!!) = Lazy.force;

external id: 'a => 'a = "%identity";
external identity: 'a => 'a = "%identity";
let flip = (f, x, y) => f(y, x);
let some = x => Some(x);
let const = (x, ()) => x;

let apply2 = (f, (x, y)) => (f(x), f(y));

let printfn = fmt => Printf.ksprintf(print_endline, fmt);
let eprintfn = fmt => Printf.ksprintf(prerr_endline, fmt);

let curry = (f, a, b) => f((a, b));
let uncurry = (f, (a, b)) => f(a, b);

module Fresh = (T: {
                  type t;
                  let compare: (t, t) => int;
                }, ()) => {
  type t = T.t;
  let inject = id;
  let project = id;
  let inject_list = id;
  let project_list = id;
  let compare = T.compare;
  let equal = (a, b) => T.compare(a, b) == 0;
};

let (+=) = (a, b) => a := a^ + b;
let (-=) = (a, b) => a := a^ - b;
let tuck = (l, x) => l := [x, ...l^];
let cons = (l, x) => [x, ...l];

let round = f => {
  let bot = floor(f);
  if (f -. bot < 0.5) {
    bot;
  } else {
    bot +. 1.;
  };
};

let atoi = (name, v) =>
  try(int_of_string(v)) {
  | _ => Exn.fail("%s %S not integer", name, v)
  };

let call_me_maybe = (f, x) =>
  switch (f) {
  | None => ()
  | Some(f) => f(x)
  };

let () = Lwt_engine.set @@ (new Lwt_engines.poll);
