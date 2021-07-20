/** Misc */;

open ExtLib;
open Printf;

open Prelude;

let period = (n, f) => {
  let count = ref(0);
  () => {
    incr(count);
    if (count^ mod n == 0) {
      f(count^);
    };
  };
};

let timely = (period, ~first=?, f) => {
  assert(period > 0.);
  let next =
    ref(
      switch (first) {
      | Some(first) => first
      | None => Time.get() +. period
      },
    );
  x =>
    if (Time.get() > next^) {
      Std.finally(() => next := Time.get() +. period, f, x);
    };
};

let timely_counter = (period, f) => {
  let cnt = ref(0);
  let logger = timely(period, () => f(cnt^));
  () => {
    incr(cnt);
    logger();
  };
};

type ewma = (float => unit, unit => float);

let ewma = (alpha): ewma => {
  let x = ref(nan);
  (
    f =>
      if (compare(nan, x^) == 0) {
        x := f;
      } else {
        x := x^ +. alpha *. (f -. x^);
      },
    () =>
      if (compare(nan, x^) == 0) {
        0.;
      } else {
        x^;
      },
  );
};

let uniq = (p, e) => {
  let h = Hashtbl.create(16);
  Enum.filter(
    x => {
      let k = p(x);
      if (Hashtbl.mem(h, k)) {
        false;
      } else {
        Hashtbl.add(h, k, ());
        true;
      };
    },
    e,
  );
};

let all_uniq = (p, e) => {
  let h = Hashtbl.create(16);
  let rec loop = () =>
    switch (Enum.get(e)) {
    | None => true
    | Some(x) =>
      let k = p(x);
      if (Hashtbl.mem(h, k)) {
        false;
      } else {
        Hashtbl.add(h, k, ());
        loop();
      };
    };

  loop();
};

let list_uniq = p => List.of_enum $ uniq(p) $ List.enum;

let list_sorted_uniq = p =>
  List.rev
  $ List.fold_left(
      (acc, x) =>
        switch (acc) {
        | [y, ..._] when p(x, y) => acc
        | _ => [x, ...acc]
        },
      [],
    );

let random_int = state =>
  switch (state) {
  | None => Random.int
  | Some(t) => Random.State.int(t)
  };

let list_random_exn = (~state=?, l) =>
  List.nth(l, random_int(state, List.length(l)));

let list_random = (~state=?) =>
  fun
  | [] => None
  | l => Some(list_random_exn(~state?, l));

let array_random_exn = (~state=?, a) => a[random_int(state, Array.length(a))];
let array_random = (~state=?) =>
  fun
  | [||] => None
  | a => Some(array_random_exn(~state?, a));

let array_rfindi = (p, a) => {
  let j = ref(0);
  try(
    {
      for (i in Array.length(a) - 1 downto 0) {
        if (p(Array.unsafe_get(a, i))) {
          j := i;
          raise(Exit);
        };
      };
      raise(Not_found);
    }
  ) {
  | Exit => j^
  };
};

let array_rfind = (p, a) => a[array_rfindi(p, a)];

let array_iter_rev = (f, a) =>
  for (i in Array.length(a) - 1 downto 0) {
    f(Array.unsafe_get(a, i));
  };

let shuffle = (~state=?, a) => {
  let random = random_int(state);
  for (i in pred(Array.length(a)) downto 1) {
    let j = random(succ(i));
    if (i != j) /* faster to omit this test with arrays of about 100000 elements or more */ {
      let tmp = Array.unsafe_get(a, i);
      Array.unsafe_set(a, i, Array.unsafe_get(a, j));
      Array.unsafe_set(a, j, tmp);
    };
  };
};

let distribute = (n, l) => {
  assert(n >= 0);
  if (n < 2) {
    [|l|];
  } else {
    let a = Array.make(n, []);
    List.iteri(
      (i, x) => {
        let i = i mod n;
        a[i] = [x, ...a[i]];
      },
      l,
    );
    a;
  };
};

let undistribute = a =>
  switch (a) {
  | [|l|] => l
  | _ =>
    let a = Array.map(List.rev, a);
    let l = ref([]);
    let more = ref(true);
    while (more^) {
      more := false;
      for (i in 0 to Array.length(a) - 1) {
        switch (a[i]) {
        | [] => ()
        | [x, ...xs] =>
          more := true;
          a[i] = xs;
          l := [x, ...l^];
        };
      };
    };
    assert(Array.for_all((==)([]), a));
    List.rev(l^);
  };

let partition = distribute;
let unpartition = undistribute;

let stable_partition = (n, l) => {
  assert(n >= 0);
  if (n <= 1) {
    [l];
  } else {
    let c = List.length(l) / n;
    let rec loop = (acc, uneven, rest, idx) =>
      if (idx <= 1) {
        [rest, ...acc];
      } else {
        let d = (uneven + n - 1) / n;
        let (xs, ys) = List.split_nth(c + d, rest);
        loop([xs, ...acc], uneven - d, ys, idx - 1);
      };

    List.rev @@ loop([], List.length(l) mod n, l, n);
  };
};

let stable_unpartition = List.flatten;

let slice = (b, e) => List.take(e - b + 1) $ List.drop(b);

let file_lines_exn = file =>
  Control.with_open_in_txt(file, ch => Std.input_lines(ch) |> List.of_enum);

let make_config_lines =
  List.filter_map(s =>{
    let (s, _comment) = Stre.dividec(s, '#');
    let s = String.strip(s);
    if (s != "") {
      Some(s);
    } else {
      None;
    };
  });

let config_lines_exn = make_config_lines $ file_lines_exn;

let file_lines_exn = file =>
  try(file_lines_exn(file)) {
  | exn => Exn.fail(~exn, "file_lines %s", file)
  };
let config_lines_exn = file =>
  try(config_lines_exn(file)) {
  | exn => Exn.fail(~exn, "config_lines %s", file)
  };

let file_lines = file =>
  try(file_lines_exn(file)) {
  | _ => []
  };
let config_lines = file =>
  try(config_lines_exn(file)) {
  | _ => []
  };

let hashtbl_find = (h, f, k) =>
  try(Hashtbl.find(h, k)) {
  | Not_found =>
    let v = f();
    Hashtbl.replace(h, k, v);
    v;
  };

let binary_search' = (arr, cmp, x) => {
  let rec loop = (a, b) =>
    switch (b - a) {
    | 0 => None
    | 1 =>
      if (cmp(arr[a], x) == 0) {
        Some(arr[a]);
      } else {
        None;
      }
    | n =>
      let mid = a + n / 2;
      let v = arr[mid];
      switch (cmp(v, x)) {
      | 0 => Some(v)
      | n when n > 0 => loop(a, mid)
      | _ /* n when n < 0 */ => loop(mid + 1, b)
      };
    };

  loop(0, Array.length(arr));
};

let binary_search = (a, b, c) => Option.is_some @@ binary_search'(a, b, c);

let chunk = (n, l) => {
  assert(n > 0);
  let chunks = ref([]);
  let get_chunk = e => {
    let rec loop = acc =>
      fun
      | 0 => acc
      | n =>
        switch (Enum.get(e)) {
        | None => acc
        | Some(x) => loop([x, ...acc], n - 1)
        };

    chunks := [loop([], n), ...chunks^];
  };

  let rec loop = e =>
    switch (Enum.peek(e)) {
    | None => List.rev(chunks^)
    | _ =>
      get_chunk(e);
      loop(e);
    };

  loop(List.enum(l));
};

let chunk_e = (n, e) => {
  assert(n > 0);
  let fin = () => raise(Enum.No_more_elements);
  Enum.from(() =>{
    let i = ref(n);
    if (Enum.is_empty(e)) {
      fin();
    } else {
      Enum.from(() =>
        switch (i^) {
        | 0 => fin()
        | _ =>
          decr(i);
          switch (Enum.get(e)) {
          | None => fin()
          | Some(x) => x
          };
        }
      );
    };
  });
};

let chunk_a = (n, a) => {
  assert(n > 0);
  let chunks = Array.length(a) / n;
  let last_n = Array.length(a) mod n;
  let last =
    if (last_n == 0) {
      0;
    } else {
      1;
    };
  List.init(chunks + last, i =>
    Array.sub(
      a,
      i * n,
      if (i == chunks) {
        last_n;
      } else {
        n;
      },
    )
  );
};

let bytes_string_f = {
  let kbyte = 1024.;
  let mbyte = kbyte *. 1024.;
  let gbyte = mbyte *. 1024.;
  let tbyte = gbyte *. 1024.;
  let pbyte = tbyte *. 1024.;
  let ebyte = pbyte *. 1024.;
  f => {
    let a = abs_float(f);
    if (a < kbyte) {
      sprintf("%dB", int_of_float(f));
    } else if (a < mbyte) {
      sprintf("%dKB", int_of_float(f /. kbyte));
    } else if (a < gbyte) {
      sprintf("%.1fMB", f /. mbyte);
    } else if (a < tbyte) {
      sprintf("%.1fGB", f /. gbyte);
    } else if (a < pbyte) {
      sprintf("%.1fTB", f /. tbyte);
    } else if (a < ebyte) {
      sprintf("%.1fPB", f /. pbyte);
    } else {
      sprintf("%.1fEB", f /. ebyte);
    };
  };
};

let bytes_string = bytes_string_f $ float_of_int;
let bytes_string_i64 = bytes_string_f $ Int64.to_float;

let bytes_of_words = x => Sys.word_size / 8 * x;
let bytes_of_words_f = x => float(Sys.word_size / 8) *. x;

let caml_words = bytes_string $ bytes_of_words;
let caml_words_f = bytes_string_f $ bytes_of_words_f;

/* EMXIF */

class timer_start (start) = {
  as _;
  val mutable start = start;
  val mutable l = [];
  pub reset = {
    start = Time.now();
    l = [];
  };
  pub record = (name, t) => l = [(name, t), ...l];
  pub mark = name => l = [(name, Time.now()), ...l];
  pub show =
    List.rev(l)
    |> List.map(((name, t)) =>
         sprintf("%s:%s", name, Time.compact_duration @@ t -. start)
       )
    |> String.concat(" ");
  pub json: list((string, Yojson.Safe.t)) =
    List.rev(l)
    |> List.map(((name, t)) => (name, `Int(Time.to_ms(t -. start))));
  pub get = Time.ago(start);
  pub get_str = Time.ago_str(start);
  pub get_state = (start, l);
};

class timer = {
  as _;
  inherit (class timer_start)(Time.now());
};

let uptime = new timer;

let speed = (n, t) => float(n) /. max(t, epsilon_float);

let perform = (~name=?, f, x) => {
  let t = new timer;
  try(
    {
      Option.may(Log.self#info("Action %S started"), name);
      let () = f(x);
      Option.may(
        name =>
          Log.self#info("Action %S finished (elapsed %s)", name, t#get_str),
        name,
      );
      true;
    }
  ) {
  | exn =>
    let name = Option.map_default(Printf.sprintf(" %S"), "", name);
    Log.self#error(
      ~exn,
      ~backtrace=true,
      "Action%s aborted with uncaught exception (elapsed %s)",
      name,
      t#get_str,
    );
    false;
  };
};

let log = (~name=?, f, x) => {
  let _: bool = perform(~name?, f, x);
  ();
};
let log_do = (~name=?, f) => log(~name?, f, ());

let io_copy = (input, output) =>
  try({
    let size = 16 * 1024;
    let s = String.create(size);
    while (true) {
      let n = IO.input(input, s, 0, size);
      if (n == 0) {
        raise(IO.No_more_input);
      };
      let _: int = IO.really_output(output, s, 0, n);
      ();
    };
  }) {
  | IO.No_more_input => ()
  };

let io_null =
  IO.create_out(
    ~write=_ => (),
    ~output=(_, _, len) => len,
    ~flush=id,
    ~close=id,
  );

let compare_by = (f, a, b) => Stdlib.compare(f(a), f(b));
let compare2 = (f, g, (a, b), (a', b')) =>
  switch (f(a, a')) {
  | 0 => g(b, b')
  | x => x
  };
let compare2_by = (f, g, (a, b), (a', b')) =>
  switch (Stdlib.compare(f(a), f(a'))) {
  | 0 => Stdlib.compare(g(b), g(b'))
  | x => x
  };
let compare_fst = (f, (a, _), (a', _)) => f(a, a');

let hexdump = str => {
  let buf = Buffer.create(80)
  and num = ref(0);
  let rec loop = chars =>
    switch (List.take(16, chars)) {
    | [] => Buffer.contents(buf)
    | l =>
      if (Buffer.length(buf) != 0) {
        Buffer.add_char(buf, '\n');
      };
      bprintf(buf, "%08x|  ", num^);
      num := num^ + 16;
      let rec bytes = pos => (
        fun
        | [] => blanks(pos)
        | [x, ...l] => {
            if (pos == 8) {
              Buffer.add_char(buf, ' ');
            };
            Printf.bprintf(buf, "%02x ", Char.code(x));
            bytes(pos + 1, l);
          }
      )
      and blanks = pos =>
        if (pos < 16) {
          if (pos == 8) {
            Buffer.add_string(buf, "    ");
          } else {
            Buffer.add_string(buf, "   ");
          };
          blanks(pos + 1);
        };

      bytes(0, l);
      Buffer.add_string(buf, " |");
      List.iter(
        ch =>
          Buffer.add_char(
            buf,
            if (ch >= ' ' && ch <= '~') {
              ch;
            } else {
              '.';
            },
          ),
        l,
      );
      Buffer.add_char(buf, '|');
      loop(List.drop(16, chars));
    };

  loop(String.explode(str));
};

open Gc;

let gc_diff = (st1, st2) => {
  let allocated = st => st.minor_words +. st.major_words -. st.promoted_words;
  let a = allocated(st2) -. allocated(st1);
  let minor = st2.minor_collections - st1.minor_collections;
  let major = st2.major_collections - st1.major_collections;
  let compact = st2.compactions - st1.compactions;
  let heap = st2.heap_words - st1.heap_words;
  Printf.sprintf(
    "allocated %10s, heap %10s, collection %d %d %d",
    caml_words_f(a),
    caml_words(heap),
    compact,
    major,
    minor,
  );
};

let gc_show = (name, f, x) => {
  let t = new timer;
  let st = Gc.quick_stat();
  Std.finally(
    () => {
      let st2 = Gc.quick_stat();
      Log.main#info(
        "GC DIFF %s : %s, elapsed %s",
        name,
        gc_diff(st, st2),
        t#get_str,
      );
    },
    f,
    x,
  );
};

let gc_settings = () => {
  let gc = Gc.get();
  sprintf(
    "minor %s incr %s major %d%% compact %d%% policy %d",
    caml_words(gc.Gc.minor_heap_size),
    {
      let n = gc.Gc.major_heap_increment;
      if (n <= 1_000) {
        sprintf("%d%%", n);
      } else {
        caml_words(n);
      };
    },
    gc.Gc.space_overhead,
    gc.Gc.max_overhead,
    gc.Gc.allocation_policy,
  );
};

/*
 let mem_usage v =
   let x = Objsize.objsize v in
   Printf.sprintf "%s (data %s)" (Action.bytes_string (Objsize.size_with_headers x)) (Action.bytes_string (Objsize.size_without_headers x))
 */

let count_bytes_to = (count, out) =>
  IO.create_out(
    ~write=
      c => {
        count := Int64.succ(count^);
        IO.write(out, c);
      },
    ~output=
      (s, o, l) => {
        count := Int64.add(count^, Int64.of_int(l));
        IO.output(out, s, o, l);
      },
    ~flush=() => IO.flush(out),
    ~close=() => count^,
  );

let count_bytes = out => {
  let count = ref(0L);
  count_bytes_to(count, out);
};

let bench = (~compact=Gc.compact, count, f) => {
  compact();
  let t = new timer;
  let st = Gc.quick_stat();
  let res =
    Exn.map(
      () =>
        for (_ in 1 to count) {
          ignore @@ f();
        },
      (),
    );
  let st2 = Gc.quick_stat();
  let elapsed = t#get;
  let res =
    switch (res) {
    | `Ok () => "ok"
    | `Exn(exn) => "exn " ++ Exn.str(exn)
    };

  sprintf(
    "%s, elapsed %s, %.2f/sec : %s",
    gc_diff(st, st2),
    Time.duration_str(elapsed),
    speed(count, elapsed),
    res,
  );
};

let run_bench = (~compact=?, count, l) => {
  let max_len =
    List.fold_left(
      (acc, (name, _)) => max(acc, String.length(name)),
      0,
      l,
    );
  let align = s =>
    String.make(max(0, max_len - String.length(s)), ' ') ++ s;
  printfn("run_bench %d cases (count %d)", List.length(l), count);
  List.iter(
    ((name, f)) =>
      printfn("%s : %s", align(name), bench(~compact?, count, f)),
    l,
  );
};

/* sorting DynArray */

let rec quick_sort = (d, left, right, cmp) => {
  let i = ref(left)
  and j = ref(right);
  let pivot = DynArray.unsafe_get(d, (left + right) / 2);
  while (i^ <= j^) {
    while (cmp(DynArray.unsafe_get(d, i^), pivot) < 0) {
      incr(i);
    };
    while (cmp(pivot, DynArray.unsafe_get(d, j^)) < 0) {
      decr(j);
    };
    if (i^ <= j^) {
      let tmp = DynArray.unsafe_get(d, i^);
      DynArray.unsafe_set(d, i^, DynArray.unsafe_get(d, j^));
      DynArray.unsafe_set(d, j^, tmp);
      incr(i);
      decr(j);
    };
  };
  if (left < j^) {
    quick_sort(d, left, j^, cmp);
  };
  if (i^ < right) {
    quick_sort(d, i^, right, cmp);
  };
};

let quick_sort = (d, ~start=0, ~n=DynArray.length(d) - start, cmp) =>
  quick_sort(d, start, start + n - 1, cmp);

let list_min = (~cmp=compare, l) =>
  List.fold_left(
    (x, y) =>
      if (cmp(x, y) < 0) {
        x;
      } else {
        y;
      },
    List.hd(l),
    l,
  );

let args = List.tl(Array.to_list(Sys.argv));

let random_bytes = (~state=?, n) =>
  String.init(n, _ => Char.chr(random_int(state, 256)));
let random_ascii = (~state=?, n) =>
  String.init(n, _ =>
    Char.chr(
      Char.code('!')
      + random_int(state, Char.code('~') - Char.code('!') + 1),
    )
  );

let parse_bytes_unit = s => {
  let unit_of_string = s =>
    switch (Stre.drop_suffix(String.lowercase(s), "b")) {
    | "" => 1
    | "k" => 1024
    | "m" => 1024 * 1024
    | "g" => 1024 * 1024 * 1024
    | "t" => 1024 * 1024 * 1024 * 1024
    | "p" => 1024 * 1024 * 1024 * 1024 * 1024
    | "e" => 1024 * 1024 * 1024 * 1024 * 1024 * 1024
    | _ => raise(Not_found)
    };

  try(
    if (s == "0") {
      0;
    } else {
      unit_of_string(s);
    }
  ) {
  | Not_found =>
    try(
      Scanf.sscanf(
        s,
        "%d%s%!",
        (n, t) => {
          assert(n != 0);
          n * unit_of_string(t);
        },
      )
    ) {
    | exn => Exn.fail(~exn, "parse_bytes_unit: %S", s)
    }
  };
};

let get_bytes_unit = n => {
  let rec loop = (n, l) =>
    switch (l) {
    | [] => raise(Not_found)
    | [_, ...xs] when n mod 1024 == 0 => loop(n / 1024, xs)
    | [x, ..._] => (n, x)
    };

  assert(n != 0);
  loop(n, ["", "K", "M", "G", "T", "P", "E"]);
};

let show_bytes_unit = n =>
  switch (get_bytes_unit(n)) {
  | (1, s) => s ++ "B"
  | (n, s) => string_of_int(n) ++ s
  };

let show_bytes_unit =
  fun
  | 0 => "0"
  | n when n < 0 => "-" ++ show_bytes_unit(- n)
  | n => show_bytes_unit(n);

let shell_sequence = names => {
  let l = ref([]);
  let fresh = s =>
    try(
      Scanf.sscanf(s, "%[a-z]%[0-9]%!", (prefix, start) =>
        (prefix, start, int_of_string(start))
      )
    ) {
    | _ =>
      tuck(l, s);
      ("", "", 0);
    };

  let flush = ((prefix, start, last)) =>
    if (prefix != "") {
      tuck(l) @@
      (
        if (int_of_string(start) == last) {
          sprintf("%s%s", prefix, start);
        } else {
          sprintf("%s{%s..%d}", prefix, start, last);
        }
      );
    };

  let acc =
    List.fold_left(
      ((prefix, start, last) as acc, s) =>
        switch (prefix) {
        | "" => fresh(s)
        | _ =>
          let next =
            switch (
              String.(
                length(prefix)
                + length(start) == length(s)
                && sub(s, 0, length(prefix)) == prefix
              )
            ) {
            | false => None
            | true =>
              switch (
                int_of_string(
                  String.(sub(s, length(prefix), length(start))),
                )
              ) {
              | exception _ => None
              | n when n == last + 1 => Some((prefix, start, n))
              | _ => None
              }
            };

          switch (next) {
          | Some(acc) => acc
          | None =>
            flush(acc);
            fresh(s);
          };
        },
      ("", "", 0),
      names,
    );

  flush(acc);
  List.rev(l^);
};
