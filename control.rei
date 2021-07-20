/** Control flow */;

/** [bracket resource destroy k]
    @return [k resource] and guarantee that [resource] is [destroy]'ed at the end. */

let bracket: ('a, 'a => unit, 'a => 'b) => 'b;

/** [wrapped acc result k]

  Computation [k] accumulates result into resource [acc] which
  is guaranteed to be released at the end. Rarely useful (e.g. {!IO.output_string})
  @return [result acc] */

let wrapped: ('a, 'a => 'b, 'a => unit) => 'b;

/** File IO */;

/** Protected file IO, stdlib interface */;

let with_open_in_bin: (string, in_channel => 'a) => 'a;
let with_open_in_txt: (string, in_channel => 'a) => 'a;

let with_open_out_bin: (string, out_channel => 'a) => 'a;
let with_open_out_txt: (string, out_channel => 'a) => 'a;

let with_open_out_temp_file:
  (
    ~temp_dir: string=?,
    ~mode: list(open_flag),
    ((string, out_channel)) => 'a
  ) =>
  'a;
let with_open_out_temp_bin: (((string, out_channel)) => 'a) => 'a;
let with_open_out_temp_txt: (((string, out_channel)) => 'a) => 'a;

/** Protected file IO, extlib interface */;

let wrapped_output: (IO.output('a), IO.output('a) => unit) => 'a;
let wrapped_outs: (IO.output(string) => unit) => string;

let with_input: (IO.input, IO.input => 'a) => 'a;
let with_input_bin: (string, IO.input => 'a) => 'a;
let with_input_txt: (string, IO.input => 'a) => 'a;

let with_output: (IO.output(unit), IO.output(unit) => 'a) => 'a;
let with_output_bin: (string, IO.output(unit) => 'a) => 'a;
let with_output_txt: (string, IO.output(unit) => 'a) => 'a;

/** Misc. */;

let with_opendir: (string, Unix.dir_handle => 'b) => 'b;
