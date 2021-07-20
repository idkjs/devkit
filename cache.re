open Prelude;

module StdHashtbl = Hashtbl;

open ExtLib;
open Printf;

module type Lock = {
  type t;
  let create: unit => t;
  let locked: (t, unit => 'a) => 'a;
};

module NoLock = {
  type t = unit;
  let create = () => ();
  let locked = ((), f) => f();
};

module TimeLimited2 =
       (
         E: Set.OrderedType,
         Lock: {
           type t;
           let create: unit => t;
           let locked: (t, unit => 'a) => 'a;
         },
       ) => {
  type time = Int64.t;

  let fixed = f => 10000. *. f |> Int64.of_float;
  let current = () => Unix.gettimeofday() |> fixed;

  module Value = {
    type t = (E.t, time);
    let compare = ((v1, _), (v2, _)) => E.compare(v1, v2);
  };

  module M = Set.Make(Value);

  type t = {
    limit: time,
    mutable next: time,
    lock: Lock.t,
    mutable m: M.t,
  };

  let private_purge = t => {
    let cur = current();
    if (cur >= t.next) {
      t.next = Int64.add(t.limit, cur);
      t.m = M.filter(((_, t)) => t > cur, t.m);
    };
  };

  let create = limit => {
    limit: fixed(limit),
    next: 0L,
    lock: Lock.create(),
    m: M.empty,
  };

  let add = (t, x) => {
    let expire = Int64.add(t.limit, current());
    /* FIXME replace not add */
    Lock.locked(
      t.lock,
      () => {
        private_purge(t);
        t.m = M.add((x, expire), t.m);
      },
    );
  };

  let get = (t, v) =>
    /* lock is not needed */
    Lock.locked(t.lock, () => M.find_opt((v, t.limit), t.m));

  let count = t => Lock.locked(t.lock, () => M.cardinal(t.m));

  let iter = (t, f) =>
    Lock.locked(t.lock, () => M.iter(((x, _)) => f(x), t.m));
};

module Count = {
  open Hashtbl;
  type t('a) = Hashtbl.t('a, ref(int));
  let create = (): t('a) => create(16);
  let clear = Hashtbl.clear;
  let entry = (t, x) =>
    switch (find(t, x)) {
    | r => r
    | exception Not_found =>
      let r = ref(0);
      Hashtbl.add(t, x, r);
      r;
    };
  let plus = (t, x, n) => entry(t, x) += n;
  let minus = (t, x, n) => entry(t, x) -= n;
  let of_enum = e => {
    let h = create();
    Enum.iter(((k, n)) => plus(h, k, n), e);
    h;
  };
  let of_list = l => of_enum @@ List.enum(l);
  let add = (t, x) => plus(t, x, 1);
  let del = (t, x) => minus(t, x, 1);
  let enum = t => enum(t) |> Enum.map(((k, n)) => (k, n^));
  let iter = (t, f) => iter((k, n) => f(k, n^), t);
  let fold = (t, f, acc) =>
    Hashtbl.fold((k, n, acc) => f(k, n^, acc), t, acc);
  let count = (t, k) =>
    switch (Hashtbl.find(t, k)) {
    | n => n^
    | exception Not_found => 0
    };
  let count_all = t => Hashtbl.fold((_, n, acc) => acc + n^, t, 0);
  let size = Hashtbl.length;
  let show = (t, ~sep=" ", f) =>
    enum(t)
    |> List.of_enum
    |> List.sort(~cmp=Action.compare_by(fst))
    |> List.map(((x, n)) => sprintf("%S: %u", f(x), n))
    |> String.concat(sep);
  let show_sorted = (t, ~limit=?, ~sep="\n", f) =>
    enum(t)
    |> List.of_enum
    |> List.sort(~cmp=flip @@ Action.compare_by(snd))
    |> (
      switch (limit) {
      | None => id
      | Some(n) => List.take(n)
      }
    )
    |> List.map(((x, n)) => sprintf("%6d : %S", n, f(x)))
    |> String.concat(sep);
  let stats = (t, ~cmp=compare, f) =>
    if (Hashtbl.length(t) == 0) {
      "<empty>";
    } else {
      let a = Array.of_enum(enum(t));
      let total = Array.fold_left((t, (_, n)) => t + n, 0, a);
      let half = total / 2;
      let cmp = ((x, _), (y, _)) => cmp(x, y);
      Array.sort(cmp, a);
      let med = ref(None);
      let (mi, ma, _) =
        Array.fold_left(
          ((mi, ma, sum), x) => {
            let sum = sum + snd(x);
            if (med^ == None && half <= sum) {
              med := Some(x);
            };
            let mi =
              if (snd(x) < snd(mi)) {
                x;
              } else {
                mi;
              };
            let ma =
              if (snd(x) > snd(ma)) {
                x;
              } else {
                ma;
              };
            (mi, ma, sum);
          },
          ((fst(a[0]), max_int), (fst(a[0]), min_int), 0),
          a,
        );

      let show = ((x, n)) => sprintf("%S (%d)", f(x), n);
      sprintf(
        "total %d median %s min %s max %s",
        total,
        switch (med^) {
        | None => "?"
        | Some(x) => show(x)
        },
        show(mi),
        show(ma),
      );
    };
  let distrib = t =>
    if (Hashtbl.length(t) == 0) {
      [||];
    } else {
      let a = Array.of_enum(enum(t));
      let total = Array.fold_left((t, (_, n)) => t + n, 0, a);
      let limits = Array.init(10, i => total * (i + 1) / 10);
      let cmp = ((x, _), (y, _)) => compare(x: float, y);
      Array.sort(cmp, a);
      let distrib =
        limits
        |> Array.map(limit => {
             let (v, _) =
               Array.fold_left(
                 ((found, sum), (v, n)) => {
                   let sum = sum + n;
                   if (found == None && limit <= sum) {
                     (Some(v), sum);
                   } else {
                     (found, sum);
                   };
                 },
                 (None, 0),
                 a,
               );
             switch (v) {
             | None => nan
             | Some(v) => v
             };
           });

      distrib;
    };
  let show_distrib = (~sep="\n", t) =>
    distrib(t)
    |> Array.mapi((i, v) => sprintf("%d%% <= %f", (i + 1) * 10, v))
    |> Array.to_list
    |> String.concat(sep);
  let report = (t, ~limit=?, ~cmp=?, ~sep="\n", f) => {
    let data = show_sorted(t, ~limit?, ~sep, f);
    let stats = stats(t, ~cmp?, f);
    stats ++ sep ++ data;
  };
  let names = (t: t('a)) => List.of_enum @@ Hashtbl.keys(t);
};

/*
   Generationnal LRU cache.
   Elements are store in a first fifo, and get evicted in order.
   If an element is reused while in the first fifo, it is promoted to a second fifo, from which elements are also evicted in order.
   Hits from the second fifo puts back the element in the back of this fifo.

   The goal is to avoid low hit rate due to large workload with some regularly used elements which would get evicted from the LRU
   before being reused
 */
module LRU = (Keys: StdHashtbl.HashedType) => {
  module Hashtbl = StdHashtbl.Make(Keys);
  module Queue = {
    exception Empty;

    type elem('a) = Dllist.node_t('a);
    type t('a) = ref(option(elem('a)));

    let create = () => ref(None);

    let unwrap = Dllist.get;

    let is_singleton = list => Dllist.next(list) === list;

    let drop = elem =>
      is_singleton(elem) ? None : Some(Dllist.rev_drop(elem));

    let append = (t, value) =>
      switch (t^) {
      | None => t := Some(value)
      | Some(queue) =>
        Dllist.splice(value, Dllist.next(queue));
        Dllist.splice(queue, value);
      };

    let push = (t, value) => {
      let node = Dllist.create(value);
      append(t, node);
      node;
    };

    let pop = t =>
      switch (t^) {
      | None => raise(Empty)
      | Some(queue) =>
        t := drop(queue);
        queue;
      };

    let remove = (t, elem) =>
      switch (t^) {
      | None => ()
      | Some(queue) when elem === queue => t := drop(queue)
      | Some(_) => Dllist.remove(elem)
      };
  };

  type entry('v) = {
    key: Hashtbl.key,
    mutable value: 'v,
    mutable queue: [ | `Lru | `Lfu],
  };

  type t('v) = {
    table: Hashtbl.t(Queue.elem(entry('v))),
    mutable lru_avaibl: int,
    mutable lfu_avaibl: int,
    lru: Queue.t(entry('v)),
    lfu: Queue.t(entry('v)),
    mutable hit: int,
    mutable miss: int,
  };

  let create = size => {
    assert(size > 0);
    {
      table: Hashtbl.create(size),
      lru: Queue.create(),
      lfu: Queue.create(),
      hit: 0,
      miss: 0,
      lru_avaibl: size,
      lfu_avaibl: size,
    };
  };

  let size = cache => Hashtbl.length(cache.table);

  let iter = (f, cache) =>
    Hashtbl.iter(
      (key, value) => f(key, Queue.unwrap(value).value),
      cache.table,
    );

  let miss = cache => cache.miss;
  let hit = cache => cache.hit;

  let replace = (cache, key, value) =>
    try({
      let entry = Hashtbl.find(cache.table, key) |> Queue.unwrap;
      entry.value = value;
    }) {
    | Not_found => ()
    };

  let get_evicted = (cache, key) =>
    try({
      let node = Hashtbl.find(cache.table, key);
      let entry = Queue.unwrap(node);
      cache.hit = cache.hit + 1;
      /* first remove the entry from the current queue */
      switch (entry.queue) {
      | `Lru =>
        /* if the node is in the lru queuen it will be moved to the lfu queue */
        cache.lru_avaibl = cache.lru_avaibl + 1;
        entry.queue = `Lfu;
        Queue.remove(cache.lru, node);
      | `Lfu =>
        cache.lfu_avaibl = cache.lfu_avaibl + 1;
        Queue.remove(cache.lfu, node);
      };
      /* If the queue is full, drop one entry */
      let evicted =
        if (cache.lfu_avaibl <= 0) {
          let evicted = Queue.unwrap(Queue.pop(cache.lfu));
          Hashtbl.remove(cache.table, evicted.key);
          Some((evicted.key, evicted.value));
        } else {
          cache.lfu_avaibl = cache.lfu_avaibl - 1;
          None;
        };

      Queue.append(cache.lfu, node);
      (entry.value, evicted);
    }) {
    | Not_found =>
      cache.miss = cache.miss + 1;
      raise(Not_found);
    };

  let find = (cache, key) => {
    let entry = Queue.unwrap @@ Hashtbl.find(cache.table, key);
    entry.value;
  };

  let get = (cache, key) => fst @@ get_evicted(cache, key);

  let mem = (cache, key) => Hashtbl.mem(cache.table, key);
  let lru_free = cache => cache.lru_avaibl;
  let lfu_free = cache => cache.lfu_avaibl;

  let put_evicted = (cache, key, value) =>
    try({
      let node = Hashtbl.find(cache.table, key) |> Queue.unwrap;
      node.value = value;
      None;
    }) {
    | Not_found =>
      let evicted =
        if (cache.lru_avaibl == 0) {
          let evicted = Queue.unwrap(Queue.pop(cache.lru));
          Hashtbl.remove(cache.table, evicted.key);
          Some((evicted.key, evicted.value));
        } else {
          cache.lru_avaibl = cache.lru_avaibl - 1;
          None;
        };

      let node = Queue.push(cache.lru, {key, value, queue: `Lru});
      Hashtbl.add(cache.table, key, node);
      evicted;
    };

  let put = (cache, key, value) => put_evicted(cache, key, value) |> ignore;

  let remove = (cache, key) =>
    try({
      let node = Hashtbl.find(cache.table, key);
      Hashtbl.remove(cache.table, key);
      switch (Queue.unwrap(node).queue) {
      | `Lru =>
        cache.lru_avaibl = cache.lru_avaibl + 1;
        Queue.remove(cache.lru, node);
      | `Lfu =>
        cache.lfu_avaibl = cache.lfu_avaibl + 1;
        Queue.remove(cache.lfu, node);
      };
    }) {
    | Not_found => ()
    };
};

module Group = {
  type t('a, 'b) = (Hashtbl.t('b, list('a)), 'a => 'b);
  let by = f => (Hashtbl.create(32), f);
  let add = ((h, f), x) => {
    let k = f(x);
    try(Hashtbl.replace(h, k, [x, ...Hashtbl.find(h, k)])) {
    | Not_found => Hashtbl.add(h, k, [x])
    };
  };
  let get = ((h, _), k) =>
    try(Hashtbl.find(h, k)) {
    | Not_found => []
    };
  let iter = ((h, _), k) => Hashtbl.iter(k, h);
  let keys = ((h, _)) => Hashtbl.keys(h);
};

let group_fst = e => {
  let h = Hashtbl.create(10);
  Enum.iter(
    ((k, v)) =>
      Hashtbl.replace(
        h,
        k,
        try([v, ...Hashtbl.find(h, k)]) {
        | Not_found => [v]
        },
      ),
    e,
  );
  Hashtbl.enum(h);
};

module Assoc = {
  type t('a, 'b) = Hashtbl.t('a, 'b);
  let create = () => Hashtbl.create(32);
  let add = (h, k, v) => {
    assert(false == Hashtbl.mem(h, k));
    Hashtbl.add(h, k, v);
  };
  let get = Hashtbl.find;
  let try_get = Hashtbl.find_option;
  let del = (h, k) =>
    try({
      let v = Hashtbl.find(h, k);
      Hashtbl.remove(h, k);
      v;
    }) {
    | Not_found => assert(false)
    };
  let remove = (h, k) => {
    assert(true == Hashtbl.mem(h, k));
    Hashtbl.remove(h, k);
  };
  let size = Hashtbl.length;

  let fold = Hashtbl.fold;
};

module Lists = {
  type t('a, 'b) = Hashtbl.t('a, list('b));
  let create = () => Hashtbl.create(16);
  let get = (h, k) =>
    try(Hashtbl.find(h, k)) {
    | Not_found => []
    };
  let set = Hashtbl.replace;
  let add = (h, k, v) => Hashtbl.replace(h, k, [v, ...get(h, k)]);
  let enum = Hashtbl.enum;
  let clear = Hashtbl.clear;
  let count_keys = Hashtbl.length;
  let count_all = h =>
    Hashtbl.fold((_, l, acc) => acc + List.length(l), h, 0);
};

class cache ('a) (cb: list('a) => unit, ~limit) = {
  as self;
  val mutable l = [];
  pub name = "cache";
  pub add = x => {
    l = [x, ...l];
    if (List.length(l) >= limit) {
      cb(l);
      self#clear;
    };
  };
  pub get = l;
  pub dump = {
    cb(l);
    l = [];
  };
  pub clear = l = [];
  pub to_list = l;
  pub size = List.length(l);
};

type reused('a) = {
  cache: Stack.t('a),
  create: unit => 'a,
  reset: 'a => unit,
};
let reuse = (create, reset) => {cache: Stack.create(), create, reset};
let use = t =>
  if (Stack.is_empty(t.cache)) {
    t.create();
  } else {
    Stack.pop(t.cache);
  };
let recycle = (t, x) => {
  t.reset(x);
  Stack.push(x, t.cache);
};

module ReuseLocked =
       (L: Lock, T: {
                   type t;
                   let create: unit => t;
                   let reset: t => unit;
                 })
       : {
         type t = T.t;
         let get: unit => t;
         let release: t => unit;
       } => {
  type t = T.t;
  type cache = {
    cache: Stack.t(t),
    lock: L.t,
  };
  let cache = {cache: Stack.create(), lock: L.create()};
  let get' = () =>
    if (Stack.is_empty(cache.cache)) {
      T.create();
    } else {
      Stack.pop(cache.cache);
    };
  let get = () => L.locked(cache.lock, get');
  let release = x =>
    L.locked(
      cache.lock,
      () => {
        T.reset(x);
        Stack.push(x, cache.cache);
      },
    );
};

module Reuse = ReuseLocked(NoLock);
