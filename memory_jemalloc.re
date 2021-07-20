/** Memory reporting for jemalloc, call [setup] in every binary linked with jemalloc */;

open Devkit_core;
open Jemalloc;

let show_crt_info = () => {
  let b = Action.bytes_string;
  try({
    let memory = get_memory_stats();
    Printf.sprintf(
      "MALLOC: size %s, used %s, heap %s, free %s",
      b(memory.mapped),
      b(memory.active),
      b(memory.allocated),
      b(memory.mapped - memory.active),
    );
  }) {
  | exn => Printf.sprintf("MALLOC:? (error %s)", Exn.to_string(exn))
  };
};

let setup = () => {
  Memory.show_crt_info := show_crt_info;
  Memory.malloc_release := release_free_memory;
  ();
};
