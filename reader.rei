/** Simple string reader */;

type t;

exception EOS;
exception Not_equal(string);

let init: string => t;
let eos: t => bool;

/** post-condition: [eos] is true */

let rest: t => string;
let till: (t, string) => string;
let try_till: (t, string) => string;
let tillc: (t, char) => string;
let try_tillc: (t, char) => string;
let take: (t, int) => string;
let try_take: (t, int) => string;
let is_const: (t, string) => bool;
let const: (t, string) => unit;
let try_const: (t, string) => unit;
let while_: (t, char => bool) => string;
let skipc: (t, char) => unit;
