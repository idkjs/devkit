open Prelude;
open Control;

let enum_dir = d =>
  Enum.from(() =>
    try(Unix.readdir(d)) {
    | End_of_file => raise(Enum.No_more_elements)
    }
  );
let with_readdir = dirname => bracket(Unix.opendir(dirname), Unix.closedir);

let iter_names = (dirname, f) => {
  let rec loop = (path, rel) =>
    with_readdir(path, d =>
      enum_dir(d)
      |> Enum.iter(
           fun
           | "."
           | ".." => ()
           | name => {
               let path = Filename.concat(path, name);
               switch (
                 try(Some(Unix.openfile(path, [Unix.O_RDONLY], 0))) {
                 | _ => None
                 }
               ) {
               | None => ()
               | Some(fd) =>
                 bracket(
                   fd,
                   Exn.suppress(Unix.close),
                   fd => {
                     let rel = Filename.concat(rel, name);
                     switch (Unix.fstat(fd).Unix.st_kind) {
                     | Unix.S_REG => f(fd, path, rel)
                     | Unix.S_DIR => loop(path, rel)
                     | _ => ()
                     };
                   },
                 )
               };
             },
         )
    );
  loop(dirname, "");
};

let iter_names_q = (dirname, f) => {
  let rec loop = (path, rel) =>
    with_readdir(path, d =>
      enum_dir(d)
      |> Enum.iter(
           fun
           | "."
           | ".." => ()
           | name => {
               let path = Filename.concat(path, name);
               let rel = Filename.concat(rel, name);
               switch (
                 try(Some(Unix.stat(path).Unix.st_kind)) {
                 | _ => None
                 }
               ) {
               | Some(Unix.S_REG) => f(path, rel)
               | Some(Unix.S_DIR) => loop(path, rel)
               | _ => ()
               };
             },
         )
    );
  loop(dirname, "");
};

let iter_files = (dirname, f) =>
  iter_names(dirname, (fd, path, _) =>
    bracket(Unix.in_channel_of_descr(fd), close_in_noerr, ch => f(path, ch))
  );

let open_out_append_text =
  open_out_gen([Open_wronly, Open_append, Open_creat, Open_text], 0o644);
let open_out_append_bin =
  open_out_gen([Open_wronly, Open_append, Open_creat, Open_binary], 0o644);

/*
 let () =
   iter_files "/etc" (fun s _ -> print_endline s)
 */

let save_as = (name, ~mode=0o644, f) => {
  /* not using make_temp_file cause same dir is needed for atomic rename */
  let temp = Printf.sprintf("%s.save.%d.tmp", name, U.gettid());
  bracket(
    Unix.openfile(temp, [Unix.O_WRONLY, Unix.O_CREAT], mode), Unix.close, fd =>
    try({
      let ch = Unix.out_channel_of_descr(fd);
      /*       Unix.fchmod fd mode; */
      f(ch);
      flush(ch);
      U.fsync(fd);
      Unix.rename(temp, name);
    }) {
    | exn =>
      Exn.suppress(Unix.unlink, temp);
      raise(exn);
    }
  );
};
