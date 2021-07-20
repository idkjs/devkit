let _ipv4_trans_keys: array(int) = (
  Array.concat([
    [|
      0,
      0,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      46,
      46,
      46,
      57,
      46,
      46,
      46,
      57,
      46,
      46,
      48,
      57,
      48,
      57,
      0,
      0,
      0,
    |],
  ]):
    array(int)
);

let _ipv4_key_spans: array(int) = (
  Array.concat([
    [|0, 10, 12, 10, 12, 10, 12, 10, 12, 1, 12, 1, 12, 1, 10, 10, 0|],
  ]):
    array(int)
);

let _ipv4_index_offsets: array(int) = (
  Array.concat([
    [|
      0,
      0,
      11,
      24,
      35,
      48,
      59,
      72,
      83,
      96,
      98,
      111,
      113,
      126,
      128,
      139,
      150,
    |],
  ]):
    array(int)
);

let _ipv4_indicies: array(int) = (
  Array.concat([
    [|
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      2,
      1,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      1,
      4,
      4,
      4,
      4,
      4,
      4,
      4,
      4,
      4,
      4,
      1,
      5,
      1,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      1,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      1,
      8,
      1,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      1,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      1,
      8,
      1,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      1,
      8,
      1,
      5,
      1,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      1,
      5,
      1,
      2,
      1,
      13,
      13,
      13,
      13,
      13,
      13,
      13,
      13,
      13,
      13,
      1,
      2,
      1,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      1,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      1,
      1,
      0,
    |],
  ]):
    array(int)
);

let _ipv4_trans_targs: array(int) = (
  Array.concat([[|2, 0, 3, 12, 4, 5, 10, 6, 7, 8, 14, 9, 11, 13, 15, 16|]]):
    array(int)
);

let _ipv4_trans_actions: array(int) = (
  Array.concat([[|1, 0, 2, 3, 1, 4, 3, 1, 5, 3, 1, 3, 3, 3, 3, 3|]]):
    array(int)
);

let _ipv4_eof_actions: array(int) = (
  Array.concat([[|0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 6, 6|]]):
    array(int)
);

let ipv4_start: int = (1: int);
let ipv4_first_final: int = (14: int);
let ipv4_error: int = (0: int);

let ipv4_en_main: int = (1: int);

type _ipv4_state = {
  mutable keys: int,
  mutable trans: int,
};
exception Goto_match_ipv4;
exception Goto_again_ipv4;
exception Goto_eof_trans_ipv4;
exception Parse_ipv4(string);

let parse_ipv4 = data => {
  let cs = ref(0)
  and p = ref(0)
  and pe = ref(String.length(data))
  and eof = ref(String.length(data));
  let n = ref(0);
  let ip = ref(0l);
  let set = () => {
    if (n^ > 255) {
      raise(Parse_ipv4(data));
    };
    ip := Int32.logor(Int32.shift_left(ip^, 8), Int32.of_int(n^));
  };

  cs.contents = ipv4_start;

  {
    let state = {keys: 0, trans: 0};
    let rec do_start = () =>
      if (p.contents == pe.contents) {
        do_test_eof();
      } else if (cs.contents == 0) {
        do_out();
      } else {
        do_resume();
      }
    and do_resume = () => {
      try({
        let keys = cs.contents lsl 1;
        let inds = _ipv4_index_offsets[cs.contents];

        let slen = _ipv4_key_spans[cs.contents];
        state.trans = _ipv4_indicies[inds
                                     + (
                                       if (slen > 0
                                           && _ipv4_trans_keys[keys]
                                           <= Char.code(data.[p.contents])
                                           && Char.code(data.[p.contents])
                                           <= _ipv4_trans_keys[keys + 1]) {
                                         Char.code(data.[p.contents])
                                         - _ipv4_trans_keys[keys];
                                       } else {
                                         slen;
                                       }
                                     )];
      }) {
      | Goto_match_ipv4 => ()
      };
      do_eof_trans();
    }
    and do_eof_trans = () => {
      cs.contents = _ipv4_trans_targs[state.trans];

      try(
        {
          if (_ipv4_trans_actions[state.trans] == 0) {
            raise_notrace(Goto_again_ipv4);
          };

          switch (_ipv4_trans_actions[state.trans]) {
          | 3 =>
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 2 =>
            set();
            ();
          | 4 =>
            set();
            ();
          | 5 =>
            set();
            ();
          | 1 =>
            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();

          | _ => ()
          };
        }
      ) {
      | Goto_again_ipv4 => ()
      };

      do_again();
    }
    and do_again = () =>
      switch (cs.contents) {
      | 0 => do_out()
      | _ =>
        p.contents = p.contents + 1;
        if (p.contents != pe.contents) {
          do_resume();
        } else {
          do_test_eof();
        };
      }
    and do_test_eof = () =>
      if (p.contents == eof.contents) {
        try(
          switch (_ipv4_eof_actions[cs.contents]) {
          | 6 =>
            set();
            ();

          | _ => ()
          }
        ) {
        | Goto_again_ipv4 => do_again()
        | Goto_eof_trans_ipv4 => do_eof_trans()
        };
      }

    and do_out = () => ();
    do_start();
  };
  if (cs^ >= ipv4_first_final) {
    ip^;
  } else {
    raise(Parse_ipv4(data));
  };
};

let is_ipv4_slow = data => {
  let cs = ref(0)
  and p = ref(0)
  and pe = ref(String.length(data))
  and eof = ref(String.length(data));
  let n = ref(0);
  let set = () =>
    if (n^ > 255) {
      raise(Not_found);
    };

  cs.contents = ipv4_start;
  try(
    {
      {
        let state = {keys: 0, trans: 0};
        let rec do_start = () =>
          if (p.contents == pe.contents) {
            do_test_eof();
          } else if (cs.contents == 0) {
            do_out();
          } else {
            do_resume();
          }
        and do_resume = () => {
          try({
            let keys = cs.contents lsl 1;
            let inds = _ipv4_index_offsets[cs.contents];

            let slen = _ipv4_key_spans[cs.contents];
            state.trans = _ipv4_indicies[inds
                                         + (
                                           if (slen > 0
                                               && _ipv4_trans_keys[keys]
                                               <= Char.code(data.[p.contents])
                                               && Char.code(
                                                    data.[p.contents],
                                                  )
                                               <= _ipv4_trans_keys[keys + 1]) {
                                             Char.code(data.[p.contents])
                                             - _ipv4_trans_keys[keys];
                                           } else {
                                             slen;
                                           }
                                         )];
          }) {
          | Goto_match_ipv4 => ()
          };
          do_eof_trans();
        }
        and do_eof_trans = () => {
          cs.contents = _ipv4_trans_targs[state.trans];

          try(
            {
              if (_ipv4_trans_actions[state.trans] == 0) {
                raise_notrace(Goto_again_ipv4);
              };

              switch (_ipv4_trans_actions[state.trans]) {
              | 3 =>
                n :=
                  10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
                ();
              | 2 =>
                set();
                ();
              | 4 =>
                set();
                ();
              | 5 =>
                set();
                ();
              | 1 =>
                n := 0;
                n :=
                  10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
                ();

              | _ => ()
              };
            }
          ) {
          | Goto_again_ipv4 => ()
          };

          do_again();
        }
        and do_again = () =>
          switch (cs.contents) {
          | 0 => do_out()
          | _ =>
            p.contents = p.contents + 1;
            if (p.contents != pe.contents) {
              do_resume();
            } else {
              do_test_eof();
            };
          }
        and do_test_eof = () =>
          if (p.contents == eof.contents) {
            try(
              switch (_ipv4_eof_actions[cs.contents]) {
              | 6 =>
                set();
                ();

              | _ => ()
              }
            ) {
            | Goto_again_ipv4 => do_again()
            | Goto_eof_trans_ipv4 => do_eof_trans()
            };
          }

        and do_out = () => ();
        do_start();
      };
      cs^ >= ipv4_first_final;
    }
  ) {
  | Not_found => false
  };
};

let _is_ipv4_trans_keys: array(int) = (
  Array.concat([
    [|
      0,
      0,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      48,
      57,
      46,
      57,
      46,
      46,
      46,
      57,
      46,
      53,
      46,
      57,
      46,
      46,
      46,
      57,
      46,
      53,
      46,
      57,
      46,
      46,
      46,
      57,
      46,
      53,
      48,
      57,
      48,
      57,
      0,
      0,
      48,
      57,
      48,
      53,
      0,
    |],
  ]):
    array(int)
);

let _is_ipv4_key_spans: array(int) = (
  Array.concat([
    [|
      0,
      10,
      12,
      10,
      12,
      10,
      12,
      10,
      12,
      1,
      12,
      8,
      12,
      1,
      12,
      8,
      12,
      1,
      12,
      8,
      10,
      10,
      0,
      10,
      6,
    |],
  ]):
    array(int)
);

let _is_ipv4_index_offsets: array(int) = (
  Array.concat([
    [|
      0,
      0,
      11,
      24,
      35,
      48,
      59,
      72,
      83,
      96,
      98,
      111,
      120,
      133,
      135,
      148,
      157,
      170,
      172,
      185,
      194,
      205,
      216,
      217,
      228,
    |],
  ]):
    array(int)
);

let _is_ipv4_indicies: array(int) = (
  Array.concat([
    [|
      0,
      0,
      2,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      1,
      4,
      1,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      3,
      1,
      5,
      5,
      6,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      1,
      8,
      1,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      7,
      1,
      9,
      9,
      10,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      1,
      12,
      1,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      1,
      13,
      13,
      14,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      1,
      12,
      1,
      16,
      16,
      16,
      16,
      16,
      16,
      16,
      16,
      16,
      16,
      1,
      12,
      1,
      12,
      1,
      11,
      11,
      11,
      11,
      11,
      17,
      16,
      16,
      16,
      16,
      1,
      12,
      1,
      16,
      16,
      16,
      16,
      16,
      16,
      1,
      8,
      1,
      18,
      18,
      18,
      18,
      18,
      18,
      18,
      18,
      18,
      18,
      1,
      8,
      1,
      8,
      1,
      7,
      7,
      7,
      7,
      7,
      19,
      18,
      18,
      18,
      18,
      1,
      8,
      1,
      18,
      18,
      18,
      18,
      18,
      18,
      1,
      4,
      1,
      20,
      20,
      20,
      20,
      20,
      20,
      20,
      20,
      20,
      20,
      1,
      4,
      1,
      4,
      1,
      3,
      3,
      3,
      3,
      3,
      21,
      20,
      20,
      20,
      20,
      1,
      4,
      1,
      20,
      20,
      20,
      20,
      20,
      20,
      1,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      15,
      1,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      1,
      1,
      15,
      15,
      15,
      15,
      15,
      23,
      22,
      22,
      22,
      22,
      1,
      22,
      22,
      22,
      22,
      22,
      22,
      1,
      0,
    |],
  ]):
    array(int)
);

let _is_ipv4_trans_targs: array(int) = (
  Array.concat([
    [|
      2,
      0,
      18,
      16,
      3,
      4,
      14,
      12,
      5,
      6,
      10,
      8,
      7,
      20,
      23,
      21,
      9,
      11,
      13,
      15,
      17,
      19,
      22,
      24,
    |],
  ]):
    array(int)
);

let is_ipv4_start: int = (1: int);
let is_ipv4_first_final: int = (20: int);
let is_ipv4_error: int = (0: int);

let is_ipv4_en_main: int = (1: int);

type _is_ipv4_state = {
  mutable keys: int,
  mutable trans: int,
};
exception Goto_match_is_ipv4;
exception Goto_again_is_ipv4;
exception Goto_eof_trans_is_ipv4;
let is_ipv4 = data => {
  let cs = ref(0)
  and p = ref(0)
  and pe = ref(String.length(data));

  cs.contents = is_ipv4_start;

  {
    let state = {keys: 0, trans: 0};
    let rec do_start = () =>
      if (p.contents == pe.contents) {
        do_test_eof();
      } else if (cs.contents == 0) {
        do_out();
      } else {
        do_resume();
      }
    and do_resume = () => {
      try({
        let keys = cs.contents lsl 1;
        let inds = _is_ipv4_index_offsets[cs.contents];

        let slen = _is_ipv4_key_spans[cs.contents];
        state.trans = _is_ipv4_indicies[inds
                                        + (
                                          if (slen > 0
                                              && _is_ipv4_trans_keys[keys]
                                              <= Char.code(data.[p.contents])
                                              && Char.code(data.[p.contents])
                                              <= _is_ipv4_trans_keys[keys + 1]) {
                                            Char.code(data.[p.contents])
                                            - _is_ipv4_trans_keys[keys];
                                          } else {
                                            slen;
                                          }
                                        )];
      }) {
      | Goto_match_is_ipv4 => ()
      };
      do_eof_trans();
    }
    and do_eof_trans = () => {
      cs.contents = _is_ipv4_trans_targs[state.trans];

      do_again();
    }
    and do_again = () =>
      switch (cs.contents) {
      | 0 => do_out()
      | _ =>
        p.contents = p.contents + 1;
        if (p.contents != pe.contents) {
          do_resume();
        } else {
          do_test_eof();
        };
      }
    and do_test_eof = () => ()
    and do_out = () => ();
    do_start();
  };
  cs^ >= is_ipv4_first_final;
};

let _compact_duration_trans_keys: array(int) = (
  Array.concat([
    [|
      0,
      0,
      115,
      115,
      48,
      110,
      115,
      115,
      46,
      110,
      48,
      109,
      48,
      109,
      48,
      109,
      109,
      109,
      48,
      57,
      46,
      115,
      48,
      115,
      48,
      115,
      48,
      115,
      109,
      115,
      48,
      57,
      0,
      0,
      48,
      57,
      48,
      57,
      46,
      115,
      48,
      57,
      46,
      115,
      48,
      115,
      46,
      115,
      0,
    |],
  ]):
    array(int)
);

let _compact_duration_key_spans: array(int) = (
  Array.concat([
    [|
      0,
      1,
      63,
      1,
      65,
      62,
      62,
      62,
      1,
      10,
      70,
      68,
      68,
      68,
      7,
      10,
      0,
      10,
      10,
      70,
      10,
      70,
      68,
      70,
    |],
  ]):
    array(int)
);

let _compact_duration_index_offsets: array(int) = (
  Array.concat([
    [|
      0,
      0,
      2,
      66,
      68,
      134,
      197,
      260,
      323,
      325,
      336,
      407,
      476,
      545,
      614,
      622,
      633,
      634,
      645,
      656,
      727,
      738,
      809,
      878,
    |],
  ]):
    array(int)
);

let _compact_duration_indicies: array(int) = (
  Array.concat([
    [|
      0,
      1,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      2,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      3,
      1,
      4,
      1,
      5,
      1,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      6,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      3,
      1,
      8,
      8,
      8,
      8,
      8,
      8,
      8,
      8,
      8,
      8,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      9,
      1,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      10,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      1,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      11,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      1,
      7,
      1,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      12,
      1,
      13,
      1,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      14,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      15,
      1,
      1,
      1,
      16,
      1,
      1,
      1,
      1,
      17,
      3,
      1,
      1,
      1,
      1,
      18,
      1,
      19,
      19,
      19,
      19,
      19,
      19,
      19,
      19,
      19,
      19,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      9,
      1,
      1,
      1,
      1,
      1,
      20,
      1,
      21,
      21,
      21,
      21,
      21,
      21,
      21,
      21,
      21,
      21,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      1,
      1,
      1,
      1,
      1,
      18,
      1,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      22,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      1,
      1,
      1,
      1,
      1,
      18,
      1,
      7,
      1,
      1,
      1,
      1,
      1,
      18,
      1,
      23,
      23,
      23,
      23,
      23,
      23,
      23,
      23,
      23,
      23,
      1,
      1,
      24,
      24,
      24,
      24,
      24,
      24,
      24,
      24,
      24,
      24,
      1,
      25,
      25,
      25,
      25,
      25,
      25,
      25,
      25,
      25,
      25,
      1,
      13,
      1,
      26,
      26,
      26,
      26,
      26,
      26,
      26,
      26,
      26,
      26,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      16,
      1,
      1,
      1,
      1,
      17,
      3,
      1,
      1,
      1,
      1,
      18,
      1,
      27,
      27,
      27,
      27,
      27,
      27,
      27,
      27,
      27,
      27,
      1,
      13,
      1,
      28,
      28,
      28,
      28,
      28,
      28,
      28,
      28,
      28,
      28,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      17,
      3,
      1,
      1,
      1,
      1,
      18,
      1,
      29,
      29,
      29,
      29,
      29,
      29,
      29,
      29,
      29,
      29,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      0,
      1,
      13,
      1,
      30,
      30,
      30,
      30,
      30,
      30,
      30,
      30,
      30,
      30,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      1,
      7,
      3,
      1,
      1,
      1,
      1,
      18,
      1,
      0,
    |],
  ]):
    array(int)
);

let _compact_duration_trans_targs: array(int) = (
  Array.concat([
    [|
      15,
      0,
      2,
      3,
      16,
      5,
      4,
      1,
      6,
      1,
      7,
      8,
      10,
      11,
      10,
      18,
      20,
      22,
      17,
      12,
      17,
      13,
      14,
      2,
      4,
      19,
      19,
      21,
      21,
      23,
      23,
    |],
  ]):
    array(int)
);

let _compact_duration_trans_actions: array(int) = (
  Array.concat([
    [|
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      2,
      3,
      4,
      4,
      5,
      0,
      1,
      0,
      0,
      0,
      0,
      2,
      3,
      4,
      4,
      9,
      11,
      13,
      1,
      15,
      1,
      17,
      1,
    |],
  ]):
    array(int)
);

let _compact_duration_eof_actions: array(int) = (
  Array.concat([
    [|
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      6,
      7,
      6,
      6,
      6,
      8,
      10,
      6,
      12,
      6,
      14,
      6,
      16,
      6,
    |],
  ]):
    array(int)
);

let compact_duration_start: int = (9: int);
let compact_duration_first_final: int = (9: int);
let compact_duration_error: int = (0: int);

let compact_duration_en_main: int = (9: int);

type _compact_duration_state = {
  mutable keys: int,
  mutable trans: int,
};
exception Goto_match_compact_duration;
exception Goto_again_compact_duration;
exception Goto_eof_trans_compact_duration;
exception Parse_compact_duration(string);

let parse_compact_duration = data => {
  if (data == "") {
    raise(Parse_compact_duration(data));
  };
  let cs = ref(0)
  and p = ref(0)
  and pe = ref(String.length(data))
  and eof = ref(String.length(data));
  let n = ref(0)
  and f = ref(0.)
  and fna = ref(0)
  and fn = ref(0);
  let t = ref(0);

  cs.contents = compact_duration_start;

  {
    let state = {keys: 0, trans: 0};
    let rec do_start = () =>
      if (p.contents == pe.contents) {
        do_test_eof();
      } else if (cs.contents == 0) {
        do_out();
      } else {
        do_resume();
      }
    and do_resume = () => {
      try({
        let keys = cs.contents lsl 1;
        let inds = _compact_duration_index_offsets[cs.contents];

        let slen = _compact_duration_key_spans[cs.contents];
        state.trans = _compact_duration_indicies[inds
                                                 + (
                                                   if (slen > 0
                                                       && _compact_duration_trans_keys[keys]
                                                       <= Char.code(
                                                            data.[p.contents],
                                                          )
                                                       && Char.code(
                                                            data.[p.contents],
                                                          )
                                                       <= _compact_duration_trans_keys[keys
                                                                    + 1]) {
                                                     Char.code(
                                                       data.[p.contents],
                                                     )
                                                     - _compact_duration_trans_keys[keys];
                                                   } else {
                                                     slen;
                                                   }
                                                 )];
      }) {
      | Goto_match_compact_duration => ()
      };
      do_eof_trans();
    }
    and do_eof_trans = () => {
      cs.contents = _compact_duration_trans_targs[state.trans];

      try(
        {
          if (_compact_duration_trans_actions[state.trans] == 0) {
            raise_notrace(Goto_again_compact_duration);
          };

          switch (_compact_duration_trans_actions[state.trans]) {
          | 1 =>
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 3 =>
            {
              fn := 0;
              fna := 0;
            };
            ();
          | 4 =>
            {
              fn := 10 * fn^ + (Char.code(data.[p.contents]) - Char.code('0'));
              fna := fna^ + 1;
            };
            ();
          | 5 =>
            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 2 =>
            {
              fn := 0;
              fna := 0;
            };
            {
              fn := 10 * fn^ + (Char.code(data.[p.contents]) - Char.code('0'));
              fna := fna^ + 1;
            };
            ();
          | 11 =>
            {
              f := f^ +. float(fn^) /. 10. ** float(fna^);
              t := t^ + n^;
              fn := 0;
              fna := 0;
            };

            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 9 =>
            {
              f :=
                f^
                +. float(n^)
                /. 1_000.
                +. float(fn^)
                /. (1000. *. 10. ** float(fna^));
              fn := 0;
              fna := 0;
            };

            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 13 =>
            t := t^ + n^ * 24 * 60 * 60;
            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 15 =>
            t := t^ + n^ * 60 * 60;
            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();
          | 17 =>
            t := t^ + n^ * 60;
            n := 0;
            n := 10 * n^ + (Char.code(data.[p.contents]) - Char.code('0'));
            ();

          | _ => ()
          };
        }
      ) {
      | Goto_again_compact_duration => ()
      };

      do_again();
    }
    and do_again = () =>
      switch (cs.contents) {
      | 0 => do_out()
      | _ =>
        p.contents = p.contents + 1;
        if (p.contents != pe.contents) {
          do_resume();
        } else {
          do_test_eof();
        };
      }
    and do_test_eof = () =>
      if (p.contents == eof.contents) {
        try(
          switch (_compact_duration_eof_actions[cs.contents]) {
          | 6 =>
            {
              f := f^ +. float(fn^) /. 10. ** float(fna^);
              t := t^ + n^;
              fn := 0;
              fna := 0;
            };
            ();
          | 8 =>
            {
              f :=
                f^
                +. float(n^)
                /. 1_000.
                +. float(fn^)
                /. (1000. *. 10. ** float(fna^));
              fn := 0;
              fna := 0;
            };
            ();
          | 10 =>
            f := f^ +. float(n^) /. 1_000_000_000.;
            ();
          | 12 =>
            t := t^ + n^ * 24 * 60 * 60;
            ();
          | 14 =>
            t := t^ + n^ * 60 * 60;
            ();
          | 16 =>
            t := t^ + n^ * 60;
            ();
          | 7 =>
            {
              fn := 0;
              fna := 0;
            };
            {
              f := f^ +. float(fn^) /. 10. ** float(fna^);
              t := t^ + n^;
              fn := 0;
              fna := 0;
            };
            ();

          | _ => ()
          }
        ) {
        | Goto_again_compact_duration => do_again()
        | Goto_eof_trans_compact_duration => do_eof_trans()
        };
      }

    and do_out = () => ();
    do_start();
  };
  if (cs^ >= compact_duration_first_final) {
    float(t^) +. f^;
  } else {
    raise(Parse_compact_duration(data));
  };
};
