(** More string functions *)

open Prelude
open ExtLib

let by_words = Pcre.regexp ~flags:[`UTF8] "[^\\pL\\pN.]+"
let by_space = Pcre.regexp "\\s+"
let by_lines = Pcre.regexp "\\r?\\n"
let split rex str = match Pcre.split ~rex str with ""::l -> l | l -> l

let nsplitc_rev str sep =
  if str = "" then []
  else
    let rec nsplit acc p =
      match try Some (String.index_from str p sep) with Not_found -> None with
      | Some p2 -> nsplit (String.sub str p (p2 - p) :: acc) (p2 + 1)
      | None -> String.sub str p (String.length str - p) :: acc
    in
    nsplit [] 0

let nsplitc str sep = List.rev (nsplitc_rev str sep)

let countc s c = String.fold_left (fun acc c' -> if c = c' then acc+1 else acc) 0 s

(** split by delimiter
  @raise Not_found if [sep] is not found in [str] *)
let splitc str sep =
  let p = String.index str sep in
  String.sub str 0 p, String.sub str (p+1) (String.length str - p - 1)

(** split by delimiter from the right
  @raise Not_found if [sep] is not found in [str] *)
let rsplitc str sep =
  let p = String.rindex str sep in
  String.sub str 0 p, String.sub str (p+1) (String.length str - p - 1)

(** [before s sep]
  @return substring from the beginning of [s] to the delimiter [sep] or the original string if delimiter is not present.
  NB [before s "" -> ""]
  e.g. [before "a.b.c" "." -> "a"]
*)
let before s sep = try String.sub s 0 (String.find s sep) with ExtString.Invalid_string -> s

(** @return substring from the delimiter [sep] to the end of string [s] or the empty string if delimiter is not present.
  e.g. [after "a.b.c" "." -> "b.c"]
  NB [after s "" -> s]
  invariant:
    if [s] contains [sep] then [before s sep ^ sep ^ after s sep = s] else [before s sep ^ after s sep = s]
  or put it another way:
    say [a = before s sep] and [b = after s sep] then
    [before (a ^ sep ^ b) sep = a] and [after (a ^ sep ^ b) sep = b]
  *)
let after s sep = try String.(let i = find s sep + length sep in sub s i (length s - i)) with ExtString.Invalid_string -> ""

(** @return [(before s sep, after s sep)]
  [divide s sep] is equal to [String.split] but doesn't throw if [sep] is not a substring of [s]
*)
let divide s sep = try String.split s sep with Invalid_string -> s, ""

(** remove prefix from string if present *)
let drop_prefix s pre = if String.starts_with s pre then String.slice s ~first:(String.length pre) else s
let drop_suffix s suf = if String.ends_with s suf then String.slice ~last:(String.length s - String.length suf) s else s

let replace_all ~str ~sub ~by =
  Str.global_substitute (Str.regexp_string sub) (fun _ -> by) str

let qreplace str sub by =
  Pcre.qreplace ~rex:(Pcre.regexp @@ Pcre.quote sub) ~templ:by str

(** contents of the first submatch *)
let extract rex str =
  try
    Some (Pcre.extract ~rex ~full_match:false str).(0)
  with
    _ -> None

let istarts_with s ?(pos=0) prefix =
  if pos < 0 || pos > String.length s then invalid_arg "Stre.istarts_with";
  if String.length s < pos + String.length prefix then false else
  try
  for i = 0 to String.length prefix - 1 do
    let c1 = s.[pos + i] and c2 = prefix.[i] in
    if c1 <> c2 && Char.lowercase c1 <> Char.lowercase c2 then raise Not_found
  done; true
  with Not_found -> false

(** ascii case-insensitive equality *)
let iequal s1 s2 = String.length s1 = String.length s2 && istarts_with s1 s2

let equal (s1:string) s2 = s1 = s2

(** ascii case-insensitive substring *)
let iexists s sub =
  if String.length s < String.length sub then false else
  if sub = "" then true else
  try
  for i = 0 to String.length s - String.length sub do
    if istarts_with s ~pos:i sub then raise Exit;
  done; false
  with Exit -> true

(** sequence of matches *)
let enum_matches rex s =
  try
    Pcre.exec_all ~rex s >> Array.enum
  with
    _ -> Enum.empty ()

let enum_extract rex s = enum_matches rex s >> Enum.map (flip Pcre.get_substring 1)

let erase_dots s =
  let rec erase = parser
  | [< ''\''; t >] -> erase t
  | [< ''0'..'9' as d; t >] -> after_digit d t
  | [< ''.'; t=dots >] -> [< '' '; erase t >]
  | [< 'c; t >] -> [< 'c; erase t >]
  | [< >] -> [< >]
  and after_digit d1 = parser
  | [< ''.'; t >] ->
    begin match Stream.peek t with
    | Some ('0'..'9') -> [< 'd1; ''.'; erase t >]
    | _ -> [< 'd1; '' '; erase t >]
    end
  | [< t >] -> [< 'd1; erase t >]
  and dots = parser
  | [< ''.'; t >] -> dots t
  | [< t >] -> t
  in
  erase (Stream.of_string s)

let string_of_stream s =
  let b = Buffer.create 32 in Stream.iter (Buffer.add_char b) s; Buffer.contents b

let split_words = split by_words $ string_of_stream $ erase_dots
let split_words s = try split_words s with exn -> Log.self #warn ~exn "split_words \"%s\"" s; [s]

module ASCII = struct
let is_alpha = function
| 'a'..'z' | 'A'..'Z' -> true
| _ -> false
let is_digit = function
| '0'..'9' -> true
| _ -> false
let is_alnum c = is_alpha c || is_digit c
end

let unescaped s =
  try Scanf.sscanf ("\""^s^"\"") "%S%!" (fun a -> a)  with  _ -> (Exn.fail "Stre.unescaped : Wry input %s" s)

let rev = String.implode $ List.rev $ String.explode

(** deprecated in favor of [common_prefix] *)
let find_prefix s1 s2 =
  let i = ref 0 in
  let min_len = min (String.length s1) (String.length s2) in
  while !i < min_len && s1.[!i] = s2.[!i] do incr i done;
  !i

let common_prefix = find_prefix

let shorten limit s =
  let limit = max limit 24 in
  if String.length s <= limit then s
  else
    let limit = limit - 20 in
    Printf.sprintf "%s..[+%d bytes]..%s" (String.slice ~last:limit s) (String.length s - limit - 4) (String.slice ~first:(-4) s)
