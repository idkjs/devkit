exception Invalid_char;
exception Invalid_table;

let chars = [|
  'A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G',
  'H',
  'I',
  'J',
  'K',
  'L',
  'M',
  'N',
  'O',
  'P',
  'Q',
  'R',
  'S',
  'T',
  'U',
  'V',
  'W',
  'X',
  'Y',
  'Z',
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
  'o',
  'p',
  'q',
  'r',
  's',
  't',
  'u',
  'v',
  'w',
  'x',
  'y',
  'z',
  '0',
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  '+',
  '/',
|];

let make_decoding_table = tbl => {
  if (Array.length(tbl) != 64) {
    raise(Invalid_table);
  };
  let d = Array.make(256, -1);
  for (i in 0 to 63) {
    Array.unsafe_set(d, Char.code(Array.unsafe_get(tbl, i)), i);
  };
  d;
};

let inv_chars = make_decoding_table(chars);

let str_decode = (~relaxed=false, ~tbl=inv_chars, s) => {
  if (Array.length(tbl) != 256) {
    raise(Invalid_table);
  };
  let data = ref(0);
  let count = ref(0);
  let pos = ref(0);
  let fail = ref(false);
  let invalid_char =
    relaxed ? () => fail := true : (() => raise(Invalid_char));

  let rec fetch = () =>
    if (fail^) {
      '?';
    } else if (count^ >= 8) {
      count := count^ - 8;
      let d = data^ asr count^ land 0xFF;
      Char.unsafe_chr(d);
    } else {
      let c = Char.code(String.unsafe_get(s, pos^));
      switch (Array.unsafe_get(tbl, c)) {
      | (-1) =>
        invalid_char();
        '?';
      | c =>
        data := data^ lsl 6 lor c;
        incr(pos);
        count := count^ + 6;
        fetch();
      };
    };

  let n = String.length(s);
  let len =
    if (n < 4) {
      n * 6 / 8;
    } else {
      switch (s.[n - 1], s.[n - 2]) {
      | ('=', '=') =>
        if (n mod 4 != 0) {
          invalid_char();
        };
        (n - 2) * 6 / 8;
      | ('=', _) =>
        if (n mod 4 != 0) {
          invalid_char();
        };
        (n - 1) * 6 / 8;
      | (_, _) => n * 6 / 8
      };
    };

  ExtString.String.init(len, _ => fetch());
};
