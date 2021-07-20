/**
  Dealing with exceptions
*/;

open Printf;
open ExtLib;

type result('a) = [ | `Ok('a) | `Exn(exn)];

let catch = (f, x) =>
  try(Some(f(x))) {
  | _ => None
  };
let default = (def, f, x) =>
  try(f(x)) {
  | _ => def
  };
let suppress = (f, x) =>
  try(f(x)) {
  | _ => ()
  };
let map = (f, x) =>
  try(`Ok(f(x))) {
  | exn => `Exn(exn)
  };

let to_string = exn =>
  switch (exn) {
  | Unix.Unix_error(e, f, s) =>
    sprintf("Unix_error %s(%s) %s", f, s, Unix.error_message(e))
  | Curl.CurlException((_, n, s)) =>
    sprintf("Curl.CurlException(%u,%s)", n, s)
  | Pcre.Error(err) =>
    sprintf(
      "Pcre.Error(%s)",
      switch (err) {
      | Partial => "Partial"
      | BadPartial => "BadPartial"
      | BadPattern(m, p) => sprintf("BadPattern(%s,%i)", m, p)
      | BadUTF8 => "BadUTF8"
      | BadUTF8Offset => "BadUTF8Offset"
      | MatchLimit => "MatchLimit"
      | RecursionLimit => "RecursionLimit"
      | InternalError(s) => sprintf("InternalError(%s)", s)
      | _ => Printexc.to_string(exn)
      },
    )
  | exn => Printexc.to_string(exn)
  };

let str = to_string;

let fail = (~exn=?, fmt) => {
  let fails = s =>
    switch (exn) {
    | None => failwith(s)
    | Some(exn) => failwith(s ++ " : " ++ to_string(exn))
    };
  ksprintf(fails, fmt);
};

let invalid_arg = fmt => ksprintf(invalid_arg, fmt);

let get_backtrace = () => String.nsplit(Printexc.get_backtrace(), "\n");
