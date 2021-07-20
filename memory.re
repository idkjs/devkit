/** Memory reporting - GC and OS, optionally malloc

General background:
- VSZ is not very intersting, this is the amount of memory which is mapped to the process address space.
  It's not really memory use, only the amount of memory the process can access without triggering a segfault.
- RSS is resident set size: this is the real world data. It's tracked by kernel and is the amount of memory
  currently allocated to this process. Most of the time this is what you want to look at.
- Malloc stats: those are metrics tracked by C malloc (jemalloc, tcmalloc, glibc, etc).
  - size is basically like VSZ but from malloc point of view.
    That is it does not include mmap files for instance.
  - used is basically RSS from malloc point of view.
  - heap is the sum of all currently malloced values for which [free] had not been called.
    So this is what application requested, not including metadata, cache, etc
- Gc stats are one level above and are tracked by ocaml gc.
  e.g. heap is the total size allocate for ocaml program. See [Gc] module documentation for more details.
*/;

open Prelude;
open ExtLib;
open Printf;

let log = Log.from("memory");

type t = {
  /** resident set size */
  rss: int,
  /** virtual memory size */
  vsize: int,
  /** number of VM mappings */
  nr_maps: int,
  /** used swap size */
  swap_used: int,
};

let get_num =
  int_of_string
  $ String.replace_chars(c =>
      if (Stre.ASCII.is_digit(c)) {
        String.of_char(c);
      } else {
        "";
      }
    );

let pagesize = Int64.to_int(ExtUnix.Specific.(sysconf(PAGESIZE)));

/** @return virtual memory info */

let get_vm_info = () => {
  let (vsize, rss) =
    switch (Action.file_lines("/proc/self/statm")) {
    | [] =>
      Log.self#warn("cannot read /proc/self/statm, no VM info");
      (0, 0);
    | [s, ..._] =>
      Scanf.sscanf(s, "%d %d", (vsize, rss) =>
        (pagesize * vsize, pagesize * rss)
      )
    };

  let nr_maps = List.length @@ Action.file_lines("/proc/self/maps"); /* FIXME deleted */
  /* process smaps */
  let swap_used =
    Action.file_lines("/proc/self/smaps")
    |> List.fold_left(
         (acc, s) =>
           if (String.starts_with(s, "Swap:")) {
             acc + get_num(s);
           } else {
             acc;
           },
         0,
       );

  {rss, vsize, nr_maps, swap_used: swap_used * 1024};
};

let show_vm_info = () => {
  let bytes = Action.bytes_string;
  let {rss, vsize, nr_maps, swap_used} = get_vm_info();
  sprintf(
    "VM: rss %s, vsz %s, swap %s, maps %d",
    bytes(rss),
    bytes(vsize),
    bytes(swap_used),
    nr_maps,
  );
};

let show_gc_heap = (~st=Gc.quick_stat(), ()) =>
  Action.(
    sprintf(
      "%s (max %s, chunks %d)",
      caml_words(st.Gc.heap_words),
      caml_words(st.Gc.top_heap_words),
      st.Gc.heap_chunks,
    )
  );

let show_gc_info = () => {
  open Action;
  let st = Gc.quick_stat();
  let gc_heap = show_gc_heap(~st, ());
  let gc_ctrs =
    sprintf(
      "%s %s %s",
      caml_words_f(st.Gc.minor_words),
      caml_words_f(st.Gc.promoted_words),
      caml_words_f(st.Gc.major_words),
    );

  let gc_coll =
    sprintf(
      "%u %u %u",
      st.Gc.compactions,
      st.Gc.major_collections,
      st.Gc.minor_collections,
    );

  sprintf(
    "GC: Heap: %s Counters(mi,pr,ma): %s Collections(mv,ma,mi): %s",
    gc_heap,
    gc_ctrs,
    gc_coll,
  );
};

let show_lwt_info = () => {
  let (r, w, t) =
    Lwt_engine.(readable_count(), writable_count(), timer_count());
  sprintf("lwt readable %d, writable %d, timer %d", r, w, t);
};

/* hooks for Memory_gperftools */
let show_crt_info = ref(() => "MALLOC: ?");
let malloc_release = ref(ignore: unit => unit);

let reclaim_s = () => {
  module A = Action;
  let st1 = Gc.stat();
  let {rss, _} = get_vm_info();
  let t1 = Time.now();
  Gc.compact();
  let t2 = Time.now();
  malloc_release^();
  let t3 = Time.now();
  let st3 = Gc.stat();
  let {rss: rss', _} = get_vm_info();
  let changed = (f, a, b) =>
    if (a == b) {
      sprintf("= %s", f(a));
    } else {
      sprintf("%s -> %s", f(a), f(b));
    };

  sprintf(
    "Memory.reclaim: heap %s live %s freelist %s (%s), rss %s",
    changed(A.caml_words, st1.heap_words, st3.heap_words),
    changed(A.caml_words, st1.live_words, st3.live_words),
    changed(string_of_int, st1.free_blocks, st3.free_blocks),
    Time.duration_str @@ t2 -. t1,
    if (malloc_release^ === ignore) {
      A.bytes_string(rss);
    } else {
      sprintf(
        "%s (%s)",
        changed(A.bytes_string, rss, rss'),
        Time.duration_str @@ t3 -. t2,
      );
    },
  );
};

let reclaim = () => log#info("%s") @@ reclaim_s();

let reclaim_silent = () => {
  Gc.compact();
  malloc_release^();
};

let (add_stats, new_stats, log_stats, get_stats) = {
  let f_print = ref([]); /* called in reverse - and it is fine */
  let f_get = ref([]);
  let log_stats = () => {
    List.iter(f => f(), f_print^);
    List.iter(f => log#info_s @@ f(), f_get^);
  };

  let get_stats = () => List.map(f => f(), f_get^);
  (tuck(f_print), tuck(f_get), log_stats, get_stats);
};

let track_global = ref([]);
let show_global_reachable = () => {
  let l =
    List.map(
      ((name, repr)) =>
        sprintf(
          "%s %s",
          name,
          Action.caml_words @@ Obj.reachable_words(repr),
        ),
      track_global^,
    );
  sprintf("reachable: %s", String.concat(" ", l));
};
let track_global = (name, var) =>
  tuck(track_global, (name, Obj.repr(var)));

let show_c_info = () => sprintf("%s. %s", show_vm_info(), show_crt_info^());

let show_all_info = () => [show_c_info(), show_gc_info(), show_lwt_info()];

let log_all_info = () => show_all_info() |> List.iter(log#info_s);

let () = new_stats(show_c_info);
let () = new_stats(show_gc_info);
let () = new_stats(show_lwt_info);
let () = new_stats(show_global_reachable);
