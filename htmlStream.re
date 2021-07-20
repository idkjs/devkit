/** Stream of html elements */;

open Printf;
open ExtLib;

include HtmlStream_ragel;

let show_attrs_quote = (c, a) =>
  List.map(((k, v)) => sprintf(" %s=%c%s%c", k, c, Raw.project(v), c), a)
  |> String.concat("");

let show_raw_quote = (c, elem) =>
  switch (elem) {
  | Tag((name, attrs)) =>
    sprintf("<%s%s>", name, show_attrs_quote(c, attrs))
  | Text(t) => Raw.project(t)
  | Close(name) => Printf.sprintf("</%s>", name)
  | Script((attrs, s)) =>
    sprintf("<script%s>%s</script>", show_attrs_quote(c, attrs), s)
  | Style((attrs, s)) =>
    sprintf("<style%s>%s</style>", show_attrs_quote(c, attrs), s)
  };

let show_raw' = show_raw_quote('\'');
let show_raw = show_raw_quote('"');

let attrs_include = (attrs, a) => {
  let attrs =
    lazy(
      List.map(
        ((k, v)) => (k, String.nsplit(Raw.project(v), " ")),
        attrs,
      )
    );
  try(
    List.for_all(
      ((k, v)) => {
        assert((!) @@ String.contains(v, ' '));
        List.mem(v, List.assoc(k, Lazy.force(attrs)));
      },
      a,
    )
  ) {
  | Not_found => false
  };
};

let tag = (name, ~a=[]) =>
  fun
  | Tag((name', attrs)) when name == name' => attrs_include(attrs, a)
  | _ => false;

let close = name =>
  fun
  | Close(name') when name == name' => true
  | _ => false;

let to_text = (~br=false, ~strip=false) =>
  fun
  | Tag(("br", _)) when br => Some(Raw.inject("\n"))
  | Tag(_) => None
  | Text(x) =>
    Some(
      if (strip) {
        Raw.inject(String.strip(Raw.project(x)));
      } else {
        x;
      },
    )
  | Script(_)
  | Style(_)
  | Close(_) => None;

/* let make_text l = wrapped_outs (fun out -> List.iter (Option.may (IO.nwrite out) $ Option.map Raw.project $ to_text) l) */
let make_text = (~br=?, l) => {
  let fold = e => {
    let b = Buffer.create(10);
    let _: bool =
      Enum.fold(
        (s, bos) => {
          if (!bos && s != "\n") {
            Buffer.add_char(b, ' ');
          };
          Buffer.add_string(b, s);
          s == "\n";
        },
        true,
        e,
      );
    Buffer.contents(b);
  };

  List.enum(l)
  |> Enum.filter_map(to_text(~br?, ~strip=true))
  |> Enum.map(Raw.project)
  |> fold
  |> Raw.inject;
};
