/** Safe marshalling */;

open Control;
open ExtLib;

module type Value = {
  type value;
  let tag: string;
};

exception Error;

module Marshal = (V: Value) => {
  type t = V.value;

  let to_channel = (ch, ~flags=[], x) => {
    output_string(ch, V.tag);
    Marshal.to_channel(ch, x: t, flags);
  };

  let from_channel = ch => {
    let s = Bytes.create(String.length(V.tag));
    really_input(ch, s, 0, String.length(V.tag));
    if (Bytes.unsafe_to_string(s) != V.tag) {
      raise(Error);
    };
    (Marshal.from_channel(ch): t);
  };

  let to_string = (~flags=[], x) => V.tag ++ Marshal.to_string(x: t, flags);

  /** @param also - additional tags allowed (for backward compatibility) */

  let from_string_ext = (also, s) => {
    let tag = String.slice(s, ~last=String.length(V.tag));
    if (tag != V.tag && List.for_all((!=)(tag), also)) {
      raise(Error);
    };
    (Marshal.from_string(s, String.length(V.tag)): t);
  };

  let from_string = s => from_string_ext([], s);

  let to_file_exn = (name, ~mode=?, ~flags=[], x) =>
    Files.save_as(name, ~mode?, ch => to_channel(ch, ~flags, x));

  let from_file = name => with_open_in_bin(name, from_channel);
};

module type Value_ext = {
  type value;
  let tag: string;
  let also: list(string);
};

module Marshal_ext = (V: Value_ext) => {
  let () =
    List.iter(
      tag => assert(String.(length(tag) == length(V.tag))),
      V.also,
    );

  include Marshal(V);

  let from_string = s => from_string_ext(V.also, s);
};
