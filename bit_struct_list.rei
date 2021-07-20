/** Packed representation for list of integers of fixed bit length */;

module type S = {
  /** number of bits to represent each item */

  let item_bits: int;
  let pp: int => string;
};

module Make:
  (S: S) =>
   {
    type t;
    /** [of_list l] converts a list of int values [l], into a bit structure list. */

    let of_list: list(int) => t;

    /** [to_list b] converts [b] into a list of int values. */

    let to_list: t => list(int);

    /** [project b] returns an internal string representation of [b]. */

    let project: t => string;

    /** [inject s] initializes a bit list with an internal string representation [s], previously returned by [project]. */

    let inject: string => t;

    /** [iter f b] applies function [f] in turn to each item in [b]. */

    let iter: (int => unit, t) => unit;

    /** [iterwhile f b] applies function [f] in turn to each item in [b] until [f] returns [false]. */

    let iterwhile: (int => bool, t) => bool;

    /** [fold_left f a b] applies function [f] in turn to each item in [b]
     and passes the result of previous step, similarly to {!List.fold_left}. */

    let fold_left: (('a, int) => 'a, 'a, t) => 'a;

    /** [exists p b] checks if at least one element of [b] satisfies the predicate [p]. */

    let exists: (int => bool, t) => bool;

    /** [pp b] returns a pretty-print string using [S.pp] for each item of [b]. */

    let pp: t => string;
  };
