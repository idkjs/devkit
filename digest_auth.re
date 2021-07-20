open Printf;
open Prelude;
open Httpev_common;

type t = {
  mutable stamp: Time.t,
  mutable index: int,
  realm: string,
  user: string,
  password: string,
};

type digest_request = {
  name: string,
  crealm: string,
  nonce: string,
  uri: string,
  qop: [ | `Auth | `Authi | `Unknown],
  nc: string,
  cnonce: string,
  response: string,
  opaque: string,
};

module Parse = {
  /* awful */

  let appendlst = (lst, elem) => lst := List.append(lst^, [elem]);

  let appendstr = (str, elem) => str := str^ ++ elem;

  let lowparse = (elem, curstr, curlist) =>
    if (elem == ',') {
      if (String.length(curstr^) > 0) {
        appendlst(curlist, curstr^);
      };
      curstr := "";
    } else if (elem != ' ' && elem != '"' && elem != '\n' && elem != '\r') {
      appendstr(curstr, Char.escaped(elem));
    };

  let make_tuple = (a, b) => (a, b);

  let highparse = (str, curlist) => {
    let first_equal =
      try(String.index(str, '=')) {
      | Not_found => Exn.fail("symbol = not found in %s", str)
      };
    appendlst(
      curlist,
      make_tuple(
        String.sub(str, 0, first_equal),
        String.sub(
          str,
          first_equal + 1,
          String.length(str) - 1 - first_equal,
        ),
      ),
    );
  };

  let digest_request_from_string = s => {
    if (String.length(s) < 6) {
      Exn.fail("Digest string too short");
    };
    let s1 = String.sub(s, 0, 6);
    if (String.lowercase_ascii(s1) != "digest") {
      Exn.fail("Authorization fail - non-digest trying to connect");
    };
    let str = String.sub(s, 6, String.length(s) - 6);
    let tmpstr = ref("");
    let a = str ++ ",";
    let tmplist = ref([]);
    String.iter(a => lowparse(a, tmpstr, tmplist), a);
    let resultlist = ref([]);
    List.iter(a => highparse(a, resultlist), tmplist^);
    let get = k =>
      try(List.assoc(k, resultlist^)) {
      | Not_found => ""
      };
    {
      name: get("username"),
      crealm: get("realm"),
      nonce: get("nonce"),
      uri: get("uri"),
      qop:
        switch (get("qop")) {
        | "auth" => `Auth
        | "auth-int" => `Authi
        | _ => `Unknown
        },
      nc: get("nc"),
      cnonce: get("cnonce"),
      response: get("response"),
      opaque: get("opaque"),
    };
  };

  let _string_from_digest_request = p => {
    let s =
      "Digest username=\""
      ++ p.name
      ++ "\", realm=\""
      ++ p.crealm
      ++ "\", nonce=\""
      ++ p.nonce
      ++ "\", uri=\""
      ++ p.uri
      ++ "\", qop=";
    let a =
      switch (p.qop) {
      | `Auth => s ++ "auth"
      | `Authi => s ++ "auth-int"
      | `Unknown => s ++ "unknown"
      };
    let a2 =
      a
      ++ ", nc="
      ++ p.nc
      ++ ", cnonce=\""
      ++ p.cnonce
      ++ "\", response=\""
      ++ p.response
      ++ "\", opaque=\""
      ++ p.opaque
      ++ "\"";
    a2;
  };
}; /* Parse */

let md5_hex_string = Digest.(to_hex $ string);
let hash = l => md5_hex_string @@ String.concat(":", l);

let digest_opaque = md5_hex_string @@ Action.random_bytes(64);

let init = (~realm, ~user, ~password, ()) => {
  realm,
  user,
  password,
  stamp: Time.now(),
  index: 1,
};

let check = (t, req) => {
  if (Time.now() -. t.stamp > 300.) {
    t.stamp = Time.now();
    t.index = t.index + 1;
  };
  let nonce =
    hash([
      Unix.string_of_inet_addr @@ client_ip(req),
      string_of_float(t.stamp),
      string_of_int(t.index),
    ]);
  try({
    let dig =
      List.assoc("authorization", req.headers)
      |> Parse.digest_request_from_string;
    switch (dig.nonce == nonce) {
    | false => raise(Not_found)
    | true =>
      /* Nonce is ok, checking another params */
      let ha1 = hash([t.user, t.realm, t.password]);
      let ha2 = hash([show_method(req.meth), dig.uri]);
      let response =
        switch (dig.qop) {
        | `Authi
        | `Auth => hash([ha1, dig.nonce, dig.nc, dig.cnonce, "auth", ha2])
        | `Unknown => hash([ha1, dig.nonce, ha2])
        };

      if (dig.opaque != digest_opaque || dig.response != response) {
        raise(Not_found);
      };
      `Ok;
    };
  }) {
  | _ =>
    let v =
      sprintf(
        "Digest realm=\"%s\", qop=\"auth\", nonce=\"%s\", opaque=\"%s\"",
        t.realm,
        nonce,
        digest_opaque,
      );
    `Unauthorized(("WWW-Authenticate", v));
  };
};
