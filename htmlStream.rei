/** HTML scanner */;

module Raw = HtmlStream_ragel.Raw;

type elem =
  | Tag((string, list((string, Raw.t))))
  | /** attributes and contents. TODO investigate script contents encoding */
    Script(
      (list((string, Raw.t)), string),
    )
  | Style((list((string, Raw.t)), string))
  | Text(Raw.t)
  | Close(string);

type ctx;

let init: unit => ctx;
let get_lnum: ctx => int;

/**
  Scan string for html tags.
  NB
  1. self-closing tags (e.g. [<x/>]) will result in two tags generated [<x></x>] (except for [<a/>])
  2. unfinished tags at the end of input are ignored
*/

let parse: (~ctx: ctx=?, elem => unit, string) => unit;

/** @return html string for [elem] */

let show_raw: elem => string;

/** @return html string for [elem] using single quote for attributes */

let show_raw': elem => string;

let attrs_include: (list((string, Raw.t)), list((string, string))) => bool;
let tag: (string, ~a: list((string, string))=?, elem) => bool;
let close: (string, elem) => bool;

/** extract text from the list elements */

let make_text: (~br: bool=?, list(elem)) => Raw.t;
