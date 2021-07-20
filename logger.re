open Printf;

type level = [ | `Debug | `Info | `Warn | `Error];
type facil = {
  name: string,
  mutable show: int,
};
let int_level =
  fun
  | `Debug => 0
  | `Info => 1
  | `Warn => 2
  | `Error => 3;
let set_filter = (facil, level) => facil.show = int_level(level);
let get_level = facil =>
  switch (facil.show) {
  | 0 => `Debug
  | 1 => `Info
  | 2 => `Warn
  | _ => `Error
  }; /* ! */
let allowed = (facil, level) => int_level(level) >= facil.show;

let string_level =
  fun
  | `Debug => "debug"
  | `Info => "info"
  | `Warn => "warn"
  | `Error => "error";

let level =
  fun
  | "info" => `Info
  | "debug" => `Debug
  | "warn" => `Warn
  | "error" => `Error
  | s => Exn.fail("unrecognized level %s", s);

module type Target = {
  let format: (level, facil, string) => string;
  let output: (level, facil, string) => unit;
};

module type Put = {let put: (level, facil, string) => unit;};

module PutSimple = (T: Target) : Put => {
  let put = (level, facil, str) =>
    if (allowed(facil, level)) {
      T.output(level, facil, T.format(level, facil, str));
    };
};

module PutLimited = (T: Target) : Put => {
  let last = ref((`Debug, ""));
  let n = ref(0);

  /** FIXME not thread safe */

  let put = (level, facil, str) =>
    switch (allowed(facil, level)) {
    | false => ()
    | true =>
      let this = (level, str);
      if (last^ == this) {
        incr(n);
      } else {
        if (n^ != 0) {
          T.output(
            level,
            facil,
            sprintf("last message repeated %u times, suppressed\n", n^),
          );
          n := 0;
        };
        last := this;
        T.output(level, facil, T.format(level, facil, str));
      };
    };
};

module Make = (T: Put) => {
  let debug_s = T.put(`Debug);
  let info_s = T.put(`Info);
  let warn_s = T.put(`Warn);
  let error_s = T.put(`Error);
  let put_s = T.put;

  let debug = (f, fmt) => ksprintf(debug_s(f), fmt);
  let info = (f, fmt) => ksprintf(info_s(f), fmt);
  let warn = (f, fmt) => ksprintf(warn_s(f), fmt);
  let error = (f, fmt) => ksprintf(error_s(f), fmt);
};
