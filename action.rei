/** miscellaneous */;

/** [period p f]

    @return a function [pf] such that [pf ()] = [f i] when [i]
    mod [p] = 0, and [()] otherwise. */

let period: (int, int => unit, unit) => unit;

/** [timely p f]

    @param first the earliest time [f x] must be executed ([now + p] by default).
    @return a function [pf] such that [pf x] = [f x] if the
    last execution of [pf x] was done more than [p] seconds ago, or
    [()] otherwise. */

let timely: (float, ~first: float=?, 'a => unit, 'a) => unit;

/** Combination of the above, see the code for more info. */

let timely_counter: (float, int => unit, unit) => unit;

/** Enum utilities */;

/** [uniq f e]

    @return [enum] that will not contain two values [x] and [x'] such
    that [f x] = [f x'].
*/

let uniq: ('a => 'b, Enum.t('a)) => Enum.t('a);

/** [all_uniq f e]

    @return [true] iff there is no two values [x] and [x'] in [e] such
    that [f x] = [f x'].
*/

let all_uniq: ('a => 'b, Enum.t('a)) => bool;

/** [chunk_e n e] splits enum [e] into chunks of [n] elements each (except the last which can be shorter).
  NB the order in result is not specified */

let chunk_e: (int, Enum.t('a)) => Enum.t(Enum.t('a));

/** List utilities */;

/**
  find the minimum element in the list
  @param cmp compare function, default [Stdlib.compare]
  @raise Empty_list when list is empty
*/

let list_min: (~cmp: ('a, 'a) => int=?, list('a)) => 'a;

/** [list_uniq f l]

    @return copy of [l] that will not contain two values [x] and [x'] such
    that [f x] = [f x'].
*/

let list_uniq: ('a => 'b, list('a)) => list('a);

/** [list_sorted_uniq eq_f l]

    @return [l] without consecutive elements [x], [x'] such that [eq_f
    x] = [eq_f x'].
*/

let list_sorted_uniq: (('a, 'a) => bool, list('a)) => list('a);

/** Get a random element from a list. */

let list_random_exn: (~state: Random.State.t=?, list('a)) => 'a;
let list_random: (~state: Random.State.t=?, list('a)) => option('a);

/** extract sublist from a list, e.g. [slice 1 3 \[0;1;2;3;4\]] will return [\[1;2;3\]]. */

let slice: (int, int, list('a)) => list('a);

/** Partitioning a list into chunks */;

/** [chunk n l] splits list [l] into chunks of [n] elements each (except the last which can be shorter).
  NB the order in result is not specified FIXME? */

let chunk: (int, list('a)) => list(list('a));

/** [distribute n l] splits [l] into [n] chunks, does not preserve the order of the elements. */

let distribute: (int, list('a)) => array(list('a));
let undistribute: array(list('a)) => list('a);

[@ocaml.deprecated "use Action.distribute"]
let partition: (int, list('a)) => array(list('a));
[@ocaml.deprecated "use Action.undistribute"]
let unpartition: array(list('a)) => list('a);

/** [stable_partition l n] splits [l] into [n] chunks, preserves the order of the elements. */

let stable_partition: (int, list('a)) => list(list('a));
let stable_unpartition: list(list('a)) => list('a);

/** Array utilities */;

let array_random_exn: (~state: Random.State.t=?, array('a)) => 'a;
let array_random: (~state: Random.State.t=?, array('a)) => option('a);

/** [array_rfindi p a]

    @return index of first element matching [p] when iterating [a] in reverse.
    @raise Not_found if no such element exists.
*/

let array_rfindi: ('a => bool, array('a)) => int;

/** [array_rfind p a]

    @return value index of first element matching [p] when iterating [a] in reverse.
    @raise Not_found if no such element exists.
*/

let array_rfind: ('a => bool, array('a)) => 'a;

/** [array_iter_rev f a] calls [f] on each elements of [a] in reverse
    order. */

let array_iter_rev: ('a => unit, array('a)) => unit;

/** [shuffle ?state a] shuffles an array, giving a uniform random distribution.
  @param state random state to use (default: global Random state)
*/

let shuffle: (~state: Random.State.t=?, array('a)) => unit;

/** array must be sorted */

let binary_search': (array('a), ('a, 'b) => int, 'b) => option('a);
let binary_search: (array('a), ('a, 'b) => int, 'b) => bool;

/** [chunk_a n a] splits array [a] into chunks of [n] elements each (except the last which can be shorter), preserving
  the order of elements, i.e. reverse operation is [Array.concat] */

let chunk_a: (int, array('a)) => list(array('a));

/** DynArray utilities */;

let quick_sort:
  (DynArray.t('a), ~start: int=?, ~n: int=?, ('a, 'a) => int) => unit;

/** Hashtbl utilities */;

/** [hashtbl_find ht f_default k] associates [f_default ()] to [k] in
    [ht], if no previous association exists.

    @return [Hashtbl.find ht k] if [k] is associated with an element
    in [ht], or [f_default ()] otherwise.
*/

let hashtbl_find: (Hashtbl.t('a, 'b), unit => 'b, 'a) => 'b;

/** Gc / Memory utilities */;

/** Memory format parsing/pretty-printing */;

/** Parse memory size specification, accepts: MB KB 1MB 20gb */

let parse_bytes_unit: string => int;

/** Pretty-print memory size in a way that can be parsed back by [parse_bytes_unit] */

let show_bytes_unit: int => string;

/** Pretty-printing */;

let bytes_of_words: int => int;
let bytes_of_words_f: float => float;

/** short human-readable display for memory measures */

let bytes_string: int => string;
let bytes_string_i64: int64 => string;
let bytes_string_f: float => string;

let caml_words: int => string;
let caml_words_f: float => string;

/** string describing gc current settings. */

let gc_diff: (Gc.stat, Gc.stat) => string;

let gc_show: (string, 'a => 'b, 'a) => 'b;
let gc_settings: unit => string;

/** File IO */;

/** Counting bytes. Not closing underlying io. */

let count_bytes_to: (ref(int64), IO.output('a)) => IO.output(int64);
let count_bytes: IO.output('a) => IO.output(int64);

/** Copy all data from [input] to [output] */

let io_copy: (IO.input, IO.output('a)) => unit;

/** /dev/null -like */

let io_null: IO.output(unit);

/** Extracting lines from a file. */;

let file_lines_exn: string => list(string);
let file_lines: string => list(string);

/** read lines from file skipping empty lines and comments (lines starting with '#') */

let make_config_lines: list(string) => list(string);
let config_lines_exn: string => list(string);
let config_lines: string => list(string);

/** Time utilities */;

/** Basic timer. Also allows recording a sequence of interesting times from the given start point.
    Can serialize recorded events to json (useful for Logstash events) */

class timer_start:
  (Time.t) =>
  {
    pub record: (string, Time.t) => unit;
    pub mark: string => unit;
    pub show: string;
    pub json: list((string, Yojson.Safe.t));
    pub get: Time.t;
    pub get_str: string;
    pub get_state: (Time.t, list((string, Time.t)));
    /** Resets both the internal start value, in addition to the list of recorded events */
    pub reset: unit;
  };

/** Convenience wrapper to start timer_start with Time.now() */

class timer: {
  inherit timer_start;
};

/** Timer running from the start of the program execution. */

let uptime: timer;
let speed: (int, float) => float;

/** Log or time execution of a function */;

let log: (~name: string=?, 'a => unit, 'a) => unit;
let log_do: (~name: string=?, unit => unit) => unit;

let perform: (~name: string=?, 'a => unit, 'a) => bool;

/** Comparison */;

let compare_by: ('a => 'b, 'a, 'a) => int;
let compare2: (('a, 'b) => int, ('c, 'd) => int, ('a, 'c), ('b, 'd)) => int;
let compare2_by: ('a => 'b, 'c => 'd, ('a, 'c), ('a, 'c)) => int;
let compare_fst: (('a, 'b) => 'c, ('a, 'd), ('b, 'e)) => 'c;

/** Benchmarking functions */;

let bench: (~compact: unit => unit=?, int, unit => 'a) => string;
let run_bench:
  (~compact: unit => unit=?, int, list((string, unit => 'a))) => unit;

/** Command-line arguments */;

/** Does not contains Sys.argv.(0). */

let args: list(string);

/** Misc. */;

/** [name01 name02 name09 name10 name11] -> [name0{1..2} name{09..11}] */

let shell_sequence: list(string) => list(string);

let hexdump: string => string;

/** Exponential Weighted Moving Average
  (smooth) 0.05 < alpha < 0.15 (dynamic)
*/

type ewma = (float => unit, unit => float);

/** [ewma alpha]

    @return [f_store, f_get] such that [f_store] is used to add a
    value to the EWMA and [f_get] returns the current EWMA including
    all the value already stored.
*/

let ewma: float => ewma;

/** generates a string of n random bytes. */

let random_bytes: (~state: Random.State.t=?, int) => string;

/** generates a string of n random ascii chars. */

let random_ascii: (~state: Random.State.t=?, int) => string;
