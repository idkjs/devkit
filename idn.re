/* Punycode and IDN library for OCaml */
/* License: without restrictions      */
/* Author: dima@caml.ru               */

/* Fixes by: ygrek and cyberhuman */
/* Version: 2013/08/29 */

module type CONV = {
  let upoints: string => array(int);
  let ustring: array(int) => string;
};

module Make = (CONV: CONV) => {
  exception Bad_input;
  exception Overflow;

  /* Parameters */

  let base = 36;
  let tmin = 1;
  let tmax = 26;
  let skew = 38;
  let damp = 700;
  let initial_bias = 72;
  let initial_n = 0x80;
  let delimiter = 0x2D;

  /* Encoding */

  let basic = p => p < 0x80;

  let encode_digit = d =>
    if (d < 26) {
      d + Char.code('a');
    } else if (d < 36) {
      d - 26 + Char.code('0');
    } else {
      raise(Bad_input);
    };

  let adapt = (delta, num_points, first) => {
    let delta =
      if (first) {
        delta / damp;
      } else {
        delta lsr 1;
      };
    let delta = ref(delta + delta / num_points);
    let k = ref(0);
    let lim = (base - tmin) * tmax / 2;
    while (delta^ > lim) {
      delta := delta^ / (base - tmin);
      k := k^ + base;
    };
    k^ + (base - tmin + 1) * delta^ / (delta^ + skew);
  };

  let encode_data = input_data => {
    let n = ref(initial_n);
    let delta = ref(0);
    let bias = ref(initial_bias);
    let basic_count = ref(0);
    let buf = Buffer.create(32);
    let out = n => Buffer.add_char(buf, Char.chr(n));

    Array.iter(
      c =>
        if (basic(c)) {
          out(c);
          incr(basic_count);
        },
      input_data,
    );

    if (basic_count^ > 0) {
      Buffer.add_char(buf, Char.chr(delimiter));
    };

    let handled_count = ref(basic_count^);

    while (handled_count^ < Array.length(input_data)) {
      let m = ref(max_int);
      Array.iter(
        c =>
          if (c >= n^ && c < m^) {
            m := c;
          },
        input_data,
      );

      if (m^ - n^ > (max_int - delta^) / succ(handled_count^)) {
        raise(Overflow);
      };
      delta := delta^ + (m^ - n^) * succ(handled_count^);
      n := m^;

      Array.iter(
        c => {
          if (c < n^) {
            incr(delta);
            if (delta^ == 0) {
              raise(Overflow);
            };
          };
          if (c == n^) {
            let q = ref(delta^);
            let k = ref(base);
            try(
              while (true) {
                let t =
                  if (k^ <= bias^) {
                    tmin;
                  } else if (k^ >= bias^ + tmax) {
                    tmax;
                  } else {
                    k^ - bias^;
                  };
                if (q^ < t) {
                  raise(Exit);
                };
                out(encode_digit(t + (q^ - t) mod (base - t)));
                q := (q^ - t) / (base - t);
                k := k^ + base;
              }
            ) {
            | Exit => ()
            };
            out(encode_digit(q^));
            bias :=
              adapt(
                delta^,
                succ(handled_count^),
                handled_count^ == basic_count^,
              );
            delta := 0;
            incr(handled_count);
          };
        },
        input_data,
      );
      incr(delta);
      incr(n);
    };
    Buffer.contents(buf);
  };

  /* Decoding */

  let decode_digit = p =>
    if (p < 48) {
      raise(Bad_input);
    } else if (p < 58) {
      p + 26 - 48;
    } else if (p < 65) {
      raise(Bad_input);
    } else if (p < 65 + 26) {
      p - 65;
    } else if (p < 97) {
      raise(Bad_input);
    } else if (p < 97 + 26) {
      p - 97;
    } else {
      raise(Bad_input);
    };

  let decode_data = input_data => {
    let buflen = String.length(input_data);
    let n = ref(initial_n);
    let i = ref(0);
    let bias = ref(initial_bias);
    let buf = Array.make(buflen, 0);

    let input_length = String.length(input_data);

    let out = ref(0);
    let data_pos =
      try({
        let pos = String.rindex(input_data, Char.chr(delimiter));
        for (i in 0 to pos - 1) {
          Array.unsafe_set(buf, i, Char.code(input_data.[i]));
        };
        out := pos;
        pos + 1;
      }) {
      | _ => 0
      };

    let j = ref(data_pos);
    while (j^ < input_length) {
      let oldi = ref(i^);
      let w = ref(1);
      let k = ref(base);
      try(
        while (true) {
          if (j^ >= input_length) {
            raise(Bad_input);
          };
          let digit = decode_digit(Char.code(input_data.[j^]));
          incr(j);
          if (digit > (max_int - i^) / w^) {
            raise(Overflow);
          };
          i := i^ + digit * w^;
          let t =
            if (k^ <= bias^) {
              tmin;
            } else if (k^ >= bias^ + tmax) {
              tmax;
            } else {
              k^ - bias^;
            };

          if (digit < t) {
            raise(Exit);
          };
          if (w^ > max_int / (base - t)) {
            raise(Overflow);
          };
          w := w^ * (base - t);
          k := k^ + base;
        }
      ) {
      | Exit => ()
      };
      let next = succ(out^);
      bias := adapt(i^ - oldi^, next, oldi^ == 0);
      if (i^ / next > max_int - n^) {
        raise(Overflow);
      };
      n := n^ + i^ / next;
      i := i^ mod next;
      if (out^ >= buflen) {
        raise(Overflow);
      };
      if (out^ > i^) {
        Array.blit(buf, i^, buf, i^ + 1, out^ - i^);
      };
      buf[i^] = n^;
      incr(i);
      incr(out);
    };
    Array.sub(buf, 0, out^);
  };

  /* Helpers */

  let split = domain => {
    let rec make = (acc, rest) =>
      try({
        let pos = String.index(rest, '.');
        make(
          [String.sub(rest, 0, pos), ...acc],
          String.sub(rest, succ(pos), String.length(rest) - pos - 1),
        );
      }) {
      | Not_found => List.rev([rest, ...acc])
      };
    make([], domain);
  };

  let join = String.concat(".");

  let need_encoding = s => {
    let l = String.length(s);
    try(
      {
        for (i in 0 to pred(l)) {
          if (!basic(Char.code(String.unsafe_get(s, i)))) {
            raise(Exit);
          };
        };
        false;
      }
    ) {
    | Exit => true
    };
  };

  let need_decoding = s => {
    let l = String.length(s);
    try(
      {
        if (l >= 4) {
          if (String.unsafe_get(s, 0) == 'x'
              && String.unsafe_get(s, 1) == 'n'
              && String.unsafe_get(s, 2) == '-'
              && String.unsafe_get(s, 3) == '-') {
            raise(Exit);
          } else {
            for (i in 0 to pred(l) - 4) {
              if (String.unsafe_get(s, i) == '.'
                  && String.unsafe_get(s, i + 1) == 'x'
                  && String.unsafe_get(s, i + 2) == 'n'
                  && String.unsafe_get(s, i + 3) == '-'
                  && String.unsafe_get(s, i + 4) == '-') {
                raise(Exit);
              };
            };
          };
        };
        false;
      }
    ) {
    | Exit => true
    };
  };

  /* Punycode API */

  let encode = s => encode_data(CONV.upoints(s));
  let decode = s => CONV.ustring(decode_data(s));

  let transcode = s =>
    if (need_encoding(s)) {
      "xn--" ++ encode(s);
    } else {
      s;
    };

  let transtext = s => {
    let l = String.length(s);
    if (l > 4 && String.sub(s, 0, 4) == "xn--") {
      decode(String.sub(s, 4, l - 4));
    } else {
      s;
    };
  };

  /* IDN api */

  let encode_domain = domain =>
    if (need_encoding(domain)) {
      join(List.map(transcode, split(domain)));
    } else {
      domain;
    };

  let decode_domain = domain =>
    if (need_decoding(domain)) {
      join(List.map(transtext, split(domain)));
    } else {
      domain;
    };

  let self_test = () => {
    assert(
      "他们为什么不说中文" == decode("ihqwcrb4cv8a8dqg056pqjye"),
    );
    assert(
      "---禁刊拍賣網址---" == decode("-------5j3ji85am9zsk4ckwjm29b"),
    );
    assert("reality44hire-b9a" == encode(decode("reality44hire-b9a")));
    assert(need_encoding("---禁刊拍賣網址---"));
    assert((!) @@ need_encoding("asdasdasdfs"));
    assert(need_decoding("xn--asd.asda"));
    assert(need_decoding("xn--"));
    assert(need_decoding("a.xn--"));
    assert((!) @@ need_decoding("a.xn-"));
    assert((!) @@ need_decoding("a.b"));
    assert(need_decoding("qwe.xn--werw"));
    assert((!) @@ need_decoding("qwexn--werw.sdfsf"));
    try({
      let _: string =
        decode_domain(
          "xn----7sbksbihemjgbjxflp8bn1jxc.xn--p1aiaudio_orlov_yum",
        );
      assert(false);
    }) {
    | Bad_input => assert(true)
    | _ => assert(false)
    };
    ();
  };

  let () = self_test();
};
