/** Various types of in-memory caches */;

module type Lock = {
  type t;
  let create: unit => t;
  let locked: (t, unit => 'a) => 'a;
};

/** see also {!ExtThread.LockMutex} */

module NoLock: Lock;

module TimeLimited2:
  (E: Set.OrderedType, Lock: Lock) =>
   {
    type t;
    type time = int64;
    let create: Time.t => t;
    let add: (t, E.t) => unit;
    let get: (t, E.t) => option((E.t, time));
    let count: t => int;
    let iter: (t, E.t => unit) => unit;
  };

module LRU:
  (K: Hashtbl.HashedType) =>
   {
    type t('v);
    let create: int => t('v);
    let put: (t('v), K.t, 'v) => unit;
    let put_evicted: (t('v), K.t, 'v) => option((K.t, 'v));
    let get: (t('v), K.t) => 'v;
    let get_evicted: (t('v), K.t) => ('v, option((K.t, 'v)));
    let find: (t('v), K.t) => 'v;
    let replace: (t('v), K.t, 'v) => unit;
    let remove: (t('v), K.t) => unit;
    let miss: t('v) => int;
    let hit: t('v) => int;
    let mem: (t('v), K.t) => bool;
    let size: t('v) => int;
    let iter: ((K.t, 'v) => unit, t('v)) => unit;
    let lru_free: t('v) => int;
    let lfu_free: t('v) => int;
  };

/** Count elements */

module Count: {
  type t('a);
  let create: unit => t('a);
  let of_list: list(('a, int)) => t('a);
  let of_enum: Enum.t(('a, int)) => t('a);
  let clear: t('a) => unit;
  let add: (t('a), 'a) => unit;
  let plus: (t('a), 'a, int) => unit;
  let del: (t('a), 'a) => unit;
  let minus: (t('a), 'a, int) => unit;
  let enum: t('a) => Enum.t(('a, int));
  let iter: (t('a), ('a, int) => unit) => unit;
  let fold: (t('a), ('a, int, 'b) => 'b, 'b) => 'b;

  /** number of times given element was seen */

  let count: (t('a), 'a) => int;
  let count_all: t('a) => int;

  /** number of distinct elements */

  let size: t('a) => int;
  let show: (t('a), ~sep: string=?, 'a => string) => string;
  let show_sorted:
    (t('a), ~limit: int=?, ~sep: string=?, 'a => string) => string;
  let stats: (t('a), ~cmp: ('a, 'a) => int=?, 'a => string) => string;
  let report:
    (
      t('a),
      ~limit: int=?,
      ~cmp: ('a, 'a) => int=?,
      ~sep: string=?,
      'a => string
    ) =>
    string;
  let distrib: t(float) => array(float);
  let show_distrib: (~sep: string=?, t(float)) => string;
  let names: t('a) => list('a);
};

module Group: {
  type t('a, 'b);
  let by: ('a => 'b) => t('a, 'b);
  let add: (t('a, 'b), 'a) => unit;
  let get: (t('a, 'b), 'b) => list('a);
  let iter: (t('a, 'b), ('b, list('a)) => unit) => unit;
  let keys: t('a, 'b) => Enum.t('b);
};

let group_fst: Enum.t(('a, 'b)) => Enum.t(('a, list('b)));

/** One-to-one associations */

module Assoc: {
  type t('a, 'b);
  let create: unit => t('a, 'b);

  /** Add association, assert on duplicate key */

  let add: (t('a, 'b), 'a, 'b) => unit;

  /** Get associated value, @raise Not_found if key is not present */

  let get: (t('a, 'b), 'a) => 'b;

  /** Get associated value */

  let try_get: (t('a, 'b), 'a) => option('b);

  /** Delete association, assert if key is not present, @return associated value */

  let del: (t('a, 'b), 'a) => 'b;

  /** Delete association, assert if key is not present */

  let remove: (t('a, 'b), 'a) => unit;
  let size: t('a, 'b) => int;

  let fold: (('a, 'b, 'c) => 'c, t('a, 'b), 'c) => 'c;
};

module Lists: {
  type t('a, 'b);
  let create: unit => t('a, 'b);
  let add: (t('a, 'b), 'a, 'b) => unit;
  let get: (t('a, 'b), 'a) => list('b);
  let set: (t('a, 'b), 'a, list('b)) => unit;
  let enum: t('a, 'b) => Enum.t(('a, list('b)));
  let clear: t('a, 'b) => unit;
  let count_keys: t('a, 'b) => int;
  let count_all: t('a, 'b) => int;
};

class cache ('a):
  (list('a) => unit, ~limit: int) =>
  {
    val mutable l: list('a);
    pub add: 'a => unit;
    pub clear: unit;
    pub dump: unit;
    pub get: list('a);
    pub name: string;
    pub size: int;
    pub to_list: list('a);
  };

type reused('a);
let reuse: (unit => 'a, 'a => unit) => reused('a);
let use: reused('a) => 'a;
let recycle: (reused('a), 'a) => unit;

module Reuse:
  (T: {
     type t;
     let create: unit => t;
     let reset: t => unit;
   }) =>
   {
    type t = T.t;
    let get: unit => t;
    let release: t => unit;
  };
