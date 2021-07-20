/** Useful shortcuts */;

module U = ExtUnix.Specific;
module Enum = ExtEnum;

/** function composition : [f $ g] is equivalent to [(fun x -> f (g x))] */

let ($): ('a => 'b, 'c => 'a, 'c) => 'b;

/** 2-function composition : [f $$ g] is equivalent to [(fun x y -> f (g x) (g y))] */

let ($$): (('a, 'a) => 'b, 'c => 'a, 'c, 'c) => 'b;

/** identity */

let id: 'a => 'a;

/** idem */

let identity: 'a => 'a;

/** reverse arguments, [flip f x y] is equivalent to [f y x] */

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c;

/** map over 2-tuple */

let apply2: ('a => 'b, ('a, 'a)) => ('b, 'b);

/** [some x] is equivalent to [Some x] */

let some: 'a => option('a);

/** @return function returning given value */

let const: ('a, unit) => 'a;

/** @return curried version from function of tuple */

let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c;

/** @return function of tuple from curried function */

let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;

/** [Lazy.force] */

let (!!): Lazy.t('a) => 'a;

/** printf to stdout with newline */

let printfn: format4('a, unit, string, unit) => 'a;

/** printf to stderr with newline */

let eprintfn: format4('a, unit, string, unit) => 'a;

/** abstract type generator */

module Fresh:
  (T: {
     type t;
     let compare: (t, t) => int;
   }, ()) =>
   {
    type t;
    let inject: T.t => t;
    let project: t => T.t;
    let inject_list: list(T.t) => list(t);
    let project_list: list(t) => list(T.t);
    let compare: (t, t) => int;
    let equal: (t, t) => bool;
  };

let tuck: (ref(list('a)), 'a) => unit;
let cons: (list('a), 'a) => list('a);

let (+=): (ref(int), int) => unit;
let (-=): (ref(int), int) => unit;

let round: float => float;

/** [atoi name value]
  @return integer of string [value]
  @raise Failure if [value] is not an integer (with [name] and [value] in exception message)
*/

let atoi: (string, string) => int;

let call_me_maybe: (option('a => unit), 'a) => unit;
