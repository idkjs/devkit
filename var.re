open Printf;
open ExtLib;
open Prelude;

let log = Log.from("var");

let show_a = Stre.list(uncurry @@ sprintf("%S:%S"));

module Attr: {
  type t = pri list((string, string));
  let make: list((string, string)) => t;
  let add: ((string, string), t) => t;
  let get: t => list((string, string));
} = {
  type t = list((string, string));
  let add = ((k, _) as x, l) => {
    if (List.mem_assoc(k, l)) {
      Exn.fail("duplicate attribute %S", k);
    };
    List.sort(~cmp=compare, [x, ...l]);
  };
  let make = l => {
    let a = List.unique(~cmp=((a, _), (b, _)) => a == b, l);
    if (List.length(a) != List.length(l)) {
      Exn.fail("duplicate attributes : %s", show_a(l));
    };
    List.sort(~cmp=compare, l);
  };
  let get = identity;
};

type attributes = list((string, string));
type t =
  | Time(Time.t)
  | Count(int)
  | Bytes(int);
type group = {
  k: string,
  attr: Attr.t,
  mutable get: list(unit => list((string, option(t)))),
};

let h_families = Hashtbl.create(10);

let show_value =
  fun
  | Time(t) => Time.compact_duration(t)
  | Count(c) => string_of_int(c)
  | Bytes(b) => Action.bytes_string(b);

let make_family = (~name, ~k, ~attr) => {
  let family = Attr.make([("class", name), ...attr]);
  let _: Attr.t = Attr.add((k, ""), family); /* check that all keys are unique */
  (k, family);
};

let register = ((k, family), get) =>
  switch (Hashtbl.find(h_families, family)) {
  | exception Not_found =>
    Hashtbl.replace(h_families, family, {k, attr: family, get: [get]}) /* register new family */
  | r =>
    /* expand existing family */
    log#warn("duplicate Var %s", show_a @@ Attr.get(family));
    r.get = [get, ...r.get];
  };

let unregister = ((_k, family)) => Hashtbl.remove(h_families, family);

let is_in_families = name =>
  Hashtbl.fold(
    (k, _, a) =>
      a ? true : List.exists(e => e == ("class", name), Attr.get(k)),
    h_families,
    false,
  );

let make_cc = (f, pp, name, ~attr=[], k) => {
  let cc = Cache.Count.create();
  let get = () =>
    Cache.Count.fold(cc, (k, n, acc) => [(pp(k), f(n)), ...acc], []);
  register(make_family(~name, ~k, ~attr), get);
  cc;
};

let cc = f => make_cc(n => Some(Count(n)), f);
let cc_ms = f => make_cc(n => Some(Time(float(n) /. 1000.)), f);

class typ (name, ~attr=[], k_name) = {
  /* name+attr - family of counters */
  let get_all = h =>
    Hashtbl.fold(
      (k, v, acc) =>
        /* return all counters created for this instance */
        switch (v()) {
        | exception exn =>
          log#warn(
            ~exn,
            "variable %S %s failed",
            name,
            show_a @@ [(k_name, k), ...attr],
          );
          acc;
        | v => [(k, v), ...acc]
        },
      h,
      [],
    );
  as self;
  val h = Hashtbl.create(7);
  val family = make_family(~k=k_name, ~attr, ~name);
  initializer (register(family, () => get_all(h)));
  pub ref: 'a. ('a, 'a => t, string) => ref('a) =
    (init, f, name) => {
      let v = ref(init);
      Hashtbl.replace(h, name, () => some @@ f(v^));
      v;
    };
  /* f() either returns Some value, either returns None, informing that value could not be obtained */
  pub get_count = (name, f) =>
    Hashtbl.replace(h, name, () =>
      switch (f()) {
      | Some(x) => Some(Count(x))
      | _ => None
      }
    );
  pub get_bytes = (name, f) =>
    Hashtbl.replace(h, name, () =>
      switch (f()) {
      | Some(x) => Some(Bytes(x))
      | _ => None
      }
    );
  pub get_time = (name, f) =>
    Hashtbl.replace(h, name, () =>
      switch (f()) {
      | Some(x) => Some(Time(x))
      | _ => None
      }
    );
  pub count = name => self#ref(0, x => Count(x), name);
  pub bytes = name => self#ref(0, x => Bytes(x), name);
  pub time = name => self#ref(0., x => Time(x), name);
  pub unregister = () => unregister(family);
  pub get =
    get_all(h)
    |> List.filter_map(((k, v)) =>
         switch (v) {
         | None => None
         | Some(v) => Some((k, v))
         }
       );
  pub show =
    self#get
    |> List.map(((k, v)) => sprintf("%s: %s", k, show_value(v)))
    |> String.concat(", ");
};

let iter = f =>
  h_families
  |> Hashtbl.iter((name, g)
       /* iterate over counter families */
       =>
         switch (g.get) {
         | [get] =>
           /* no duplicates in this family */
           let l = get();
           let l' =
             List.sort_uniq(((a, _), (b, _)) => String.compare(a, b), l);
           if (List.length(l) != List.length(l')) {
             log#warn(
               "var %s : duplicate keys found and will be ignored",
               show_a @@ Attr.get(name),
             );
           };
           l'
           |> List.iter(((k, v)) => {
                let attr = [(g.k, k), ...Attr.get(g.attr)]; /* this was checked to be valid in [register] */
                switch (v) {
                | Some(v) => f(attr, v)
                | _ => ()
                };
              });
         | l =>
           /* list of getters for all instances created with this family name */
           let h = Hashtbl.create(10);
           l
           |> List.iter(get
                =>
                  get()
                  |> List.iter(((k, vl))
                       /* merge values of duplicated counters in family */
                       =>
                         switch (vl) {
                         | Some(v) =>
                           let r =
                             switch (Hashtbl.find(h, k)) {
                             | exception Not_found => Some(v)
                             | Some(x) =>
                               switch (x, v) {
                               | (Time(a), Time(b)) => Some(Time(a +. b))
                               | (Count(a), Count(b)) => Some(Count(a + b))
                               | (Bytes(a), Bytes(b)) => Some(Bytes(a + b))
                               | (Count(_), Bytes(_))
                               | (Count(_), Time(_))
                               | (Bytes(_), Count(_))
                               | (Bytes(_), Time(_))
                               | (Time(_), Count(_))
                               | (Time(_), Bytes(_)) =>
                                 log#warn(
                                   "mismatched value type for %S in %s",
                                   k,
                                   show_a @@ Attr.get(g.attr),
                                 );
                                 Some(v);
                               }
                             | None => None
                             };

                           Hashtbl.replace(h, k, r);
                         | None => Hashtbl.replace(h, k, None)
                         }
                       )
                ) /* if at least one duplicate value is invalid - ignore all data for this counter */;
           h
           |> Hashtbl.iter((k, v) => {
                let attr = [(g.k, k), ...Attr.get(g.attr)]; /* this was checked to be valid in [register] */
                switch (v) {
                | Some(v) => f(attr, v)
                | _ => ()
                };
              });
         }
       );

let list_stats = filter => {
  let l = ref([]);
  iter((attrs, v) =>
    try({
      let klass = List.assoc("class", attrs);
      if ((!) @@ List.mem(klass, filter)) {
        raise(Not_found);
      }; /* not interested stats */
      let attrs =
        List.remove_assoc("class", attrs)
        |> List.map(uncurry @@ sprintf("%s.%s"))
        |> String.join(",");
      let value = show_value(v);
      tuck(l) @@ sprintf("%s %s : %s", klass, attrs, value);
    }) {
    | Not_found => ()
    }
  );
  List.sort(l^);
};

/*
 let show () =
   let b = Buffer.create (Hashtbl.length h_vars * 20) in
   iter begin fun ~t ~k ~kname:_ ~v ->
     Printf.bprintf b "%s[%s]=%s " t k (match v with Int n -> string_of_int n | Float f -> string_of_float f);
   end;
   Buffer.contents b
 */

/* non-monotonic, pointless to log*/
/* let system_memory = new typ "system_memory" "kind" */
/* let () = system_memory#get_bytes "rss" (fun () -> (Memory.get_vm_info ()).rss) */
/* let () = system_memory#get_bytes "vsize" (fun () -> (Memory.get_vm_info ()).vsize) */
/* let () = system_memory#get_bytes "ocaml_heap" (fun () -> let gc = Gc.quick_stat () in Action.bytes_of_words gc.heap_words) */
