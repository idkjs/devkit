/** web utilities */;

open ExtLib;
open Printf;

open Prelude;
open Control;

let log = Log.self;

/** percent-encode (convert space into %20) */

let rawurlencode = Netencoding.Url.encode(~plus=false);

/** percent-encode, but convert space into plus, not %20 */

let urlencode = Netencoding.Url.encode(~plus=true);

/** percent-decode (leave plus as is) */

let rawurldecode = s =>
  try(Netencoding.Url.decode(~plus=false, s)) {
  | _ => s
  };

/** percent-decode and convert plus into space */

let urldecode = s =>
  try(Netencoding.Url.decode(~plus=true, s)) {
  | _ => s
  };

let htmlencode =
  Netencoding.Html.encode(~in_enc=`Enc_utf8, ~out_enc=`Enc_utf8, ());
let htmldecode_exn =
  Netencoding.Html.decode(~in_enc=`Enc_utf8, ~out_enc=`Enc_utf8, ());
let htmldecode = {
  /* U+FFFD REPLACEMENT CHARACTER */
  let u_FFFD = "�";
  let subst = _ => u_FFFD;
  let lookup = _ => u_FFFD;
  Netencoding.Html.decode(
    ~subst,
    ~lookup,
    ~in_enc=`Enc_utf8,
    ~out_enc=`Enc_utf8,
    (),
  );
};

/* TODO uncomment when httpev becomes less strict everywhere */
let make_url_args =
  String.concat("&")
  $ List.map(
      fun
      | (
          /* (k, "") -> urlencode k | */
          k,
          v,
        ) =>
        urlencode(k) ++ "=" ++ urlencode(v),
    );

/** Minimum strictness, Neturl will fail on malformed parameters in url */

let parse_url_args = args =>
  Stre.nsplitc_rev(args, '&')
  |> List.rev_map(s => Stre.dividec(s, '=') |> apply2(urldecode));

let url_get_args = url =>
  try(String.split(url, "?") |> snd |> parse_url_args) {
  | _ => []
  };

let () = Curl.global_init(Curl.CURLINIT_GLOBALALL);

let curl_times = h =>
  Curl.[
    ("Curl.dns", get_namelookuptime(h)),
    ("Curl.conn", get_connecttime(h)),
    /*       "Curl.app", get_appconnecttime; */
    ("Curl.pre", get_pretransfertime(h)),
    ("Curl.start", get_starttransfertime(h)),
    ("Curl.total", get_totaltime(h)),
  ];

module CurlCache =
  Cache.Reuse({
    type t = Curl.t;
    let create = Curl.init;
    let reset = Curl.reset;
  });

let curl_default_setup = h => {
  Curl.set_nosignal(h, true);
  Curl.set_connecttimeout(h, 30);
  Curl.set_timeout(h, 60);
  Curl.set_followlocation(h, false);
  Curl.set_encoding(h, Curl.CURL_ENCODING_ANY);
  ();
};

type http_action_old = [
  | `GET
  | `POST_FORM(list((string, string)))
  | `POST(string, string) /* content-type and body */
  | `PUT(string, string)
  | `DELETE
  | `CUSTOM(string, string, string) /* request, content-type and body */
];

type http_body = [
  | `Raw(string, string) /* content-type and body */
  | `Form(list((string, string))) /* key value */
  | `Chunked(string, unit => string) /* content-type and body readfunction */
];

type http_action = [
  | `GET
  | `POST
  | `PUT
  | `PATCH
  | `DELETE
  | `CUSTOM(string)
];

let string_of_http_action: http_action => string = (
  fun
  | `GET => "GET"
  | `POST => "POST"
  | `PUT => "PUT"
  | `PATCH => "PATCH"
  | `DELETE => "DELETE"
  | `CUSTOM(s) => s:
    http_action => string
);

let http_action_of_string: string => http_action = (
  fun
  | "GET" => `GET
  | "POST" => `POST
  | "PUT" => `PUT
  | "PATCH" => `PATCH
  | "DELETE" => `DELETE
  | s => Exn.fail("http_action_of_string %S", s):
    string => http_action
);

module type IO_TYPE = {
  type t('a);
  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
  let bracket: (t('a), 'a => t(unit), 'a => t('b)) => t('b);
  let sleep: Time.t => t(unit);
  let fail: (~exn: exn=?, format4('a, unit, string, t('b))) => 'a;
  let raise: exn => t('a);
  let map_s: ('a => t('b), list('a)) => t(list('b));
  let catch: (unit => t('a), exn => t('a)) => t('a);
};

module type CURL = {
  type t('a);
  let perform: Curl.t => t(Curl.curlCode);
};

type http_request_('body, 'ret) =
  (
    ~ua: string=?,
    ~timeout: int=?,
    ~verbose: bool=?,
    ~setup: Curl.t => unit=?,
    ~timer: Action.timer=?,
    ~max_size: int=?,
    ~http_1_0: bool=?,
    ~headers: list(string)=?,
    ~body: 'body=?,
    http_action,
    string
  ) =>
  'ret;

type http_request('ret) =
  http_request_(
    [ | `Form(list((string, string))) | `Raw(string, string)],
    'ret,
  );

module type HTTP = {
  module IO: IO_TYPE;
  let with_curl: (Curl.t => IO.t('a)) => IO.t('a);
  let with_curl_cache: (Curl.t => IO.t('a)) => IO.t('a);

  type request_('body, 'ret) = http_request_('body, IO.t('ret));
  type request('ret) = http_request(IO.t('ret));

  let http_request':
    request([> | `Error(Curl.curlCode) | `Ok(int, string)]);
  let http_request: request([> | `Error(string) | `Ok(string)]);
  let http_request_exn: request(string);
  let http_query:
    request_((string, string), [> | `Error(string) | `Ok(string)]);
  let http_submit:
    (
      ~ua: string=?,
      ~timeout: int=?,
      ~verbose: bool=?,
      ~setup: Curl.t => unit=?,
      ~timer: Action.timer=?,
      ~http_1_0: bool=?,
      ~headers: list(string)=?,
      ~action: http_action=?,
      string,
      list((string, string))
    ) =>
    IO.t([> | `Error(string) | `Ok(string)]);
};

let show_result = (~verbose=false) =>
  fun
  | `Error(code) =>
    sprintf("(%d) %s", Curl.errno(code), Curl.strerror(code))
  | `Ok(n, content) =>
    sprintf(
      "http %d%s",
      n,
      if (verbose) {
        ": " ++ content;
      } else {
        "";
      },
    );

let simple_result = (~is_ok=code => code / 100 == 2, ~verbose=?) =>
  fun
  | `Ok(code, s) when is_ok(code) => `Ok(s)
  | r => `Error(show_result(~verbose?, r));

let nr_http = ref(0);

module Http =
       (IO: IO_TYPE, Curl_IO: CURL with type t('a) = IO.t('a))
       : (HTTP with type IO.t('a) = IO.t('a)) => {
  module IO = IO;

  type request_('body, 'ret) = http_request_('body, IO.t('ret));
  type request('ret) = http_request(IO.t('ret));

  open IO;

  let return_unit = return();

  let with_curl = f =>
    bracket(
      return @@ Curl.init(),
      h => {
        Curl.cleanup(h);
        return_unit;
      },
      f,
    );
  let with_curl_cache = f =>
    bracket(
      return @@ CurlCache.get(),
      h => {
        CurlCache.release(h);
        return_unit;
      },
      f,
    );

  let update_timer = (h, timer) =>
    switch (timer) {
    | None => ()
    | Some(t) =>
      let total = Curl.get_totaltime(h);
      let now = Time.now();
      t#record("Curl.start", now -. total);
      curl_times(h)
      |> List.iter(((name, time)) => t#record(name, now -. total +. time));
      ();
    };

  /* deprecated */
  let http_gets =
      (
        ~setup=ignore,
        ~timer=?,
        ~max_size=?,
        ~check=_ => true,
        ~result=(_, _) => return_unit,
        url,
      ) =>
    with_curl_cache(h => {
      Curl.set_url(h, url);
      curl_default_setup(h);
      let () = setup(h);
      let b = Buffer.create(10);
      let read_size = ref(0);
      Curl.set_writefunction(h, s =>
        switch (check(h)) {
        | false => 0
        | true =>
          Buffer.add_string(b, s);
          let l = String.length(s);
          read_size += l;
          switch (max_size) {
          | Some(max_size) when read_size^ > max_size =>
            Exn.fail(
              "received too much data (%db) when max is %db",
              read_size^,
              max_size,
            )
          | _ => l
          };
        }
      );
      timer |> Option.may(t => t#mark("Web.http"));
      catch(
        () => Curl_IO.perform(h),
        exn => {
          update_timer(h, timer);
          IO.raise(exn);
        },
      )
      >>= (
        code =>
          {
            update_timer(h, timer);
            result(h, code);
          }
          >>= (
            () =>
              return @@
              (
                switch (code) {
                | Curl.CURLE_OK =>
                  `Ok((Curl.get_httpcode(h), Buffer.contents(b)))
                | code => `Error(code)
                }
              )
          )
      );
    });

  let verbose_curl_result = (nr_http, action, t, h, code) => {
    open Curl;
    let b = Buffer.create(10);
    bprintf(
      b,
      "%s #%d %s ⇓%s ⇑%s %s ",
      string_of_http_action(action),
      nr_http,
      Time.compact_duration(t#get),
      Action.bytes_string_f @@ get_sizedownload(h),
      Action.bytes_string_f @@ get_sizeupload(h),
      get_primaryip(h),
    );

    switch (code) {
    | CURLE_OK =>
      bprintf(b, "HTTP %d %s", get_httpcode(h), get_effectiveurl(h));
      switch (get_redirecturl(h)) {
      | "" => ()
      | s => bprintf(b, " -> %s", s)
      };
      switch (get_redirectcount(h)) {
      | 0 => ()
      | n => bprintf(b, " after %d redirects", n)
      };
    | _ =>
      bprintf(
        b,
        "error (%d) %s (errno %d)",
        errno(code),
        strerror(code),
        Curl.get_oserrno(h),
      )
    };
    log#info_s(Buffer.contents(b));
    return();
  };

  /* NOTE don't forget to set http_1_0=true when sending requests to a Httpev-based server */
  /* Don't use curl_setheaders when using ?headers option */
  let http_request' =
      (
        ~ua=?,
        ~timeout=?,
        ~verbose=false,
        ~setup=ignore,
        ~timer=?,
        ~max_size=?,
        ~http_1_0=false,
        ~headers=?,
        ~body=?,
        action: http_action,
        url,
      ) => {
    open Curl;
    let set_body_and_headers = (h, ct, body) => {
      set_httpheader(
        h,
        ["Content-Type: " ++ ct, ...Option.default([], headers)],
      );
      set_postfields(h, body);
      set_postfieldsize(h, String.length(body));
    };

    let setup = h => {
      switch (body) {
      | Some(`Form(args)) =>
        set_body_and_headers(
          h,
          "application/x-www-form-urlencoded",
          make_url_args(args),
        )
      | Some(`Raw(ct, body)) => set_body_and_headers(h, ct, body)
      | Some(`Chunked(ct, f)) =>
        set_httpheader(
          h,
          [
            "Content-Type: " ++ ct,
            "Transfer-Encoding: chunked",
            ...Option.default([], headers),
          ],
        );
        set_readfunction(h, f);
      | None =>
        Option.may(set_httpheader(h), headers);
        /* prevent reading from stdin with POST without body */
        set_readfunction(h, _ => "");
        /* prevent libcurl 7.66.0+ from sending Transfer-Encoding: chunked for POST without body.
           See https://github.com/curl/curl/pull/4138. */
        set_postfieldsize(h, 0);
      };
      switch (action) {
      | `GET
      | `DELETE
      | `CUSTOM(_) => ()
      | `POST
      | `PUT
      | `PATCH => set_post(h, true)
      };
      set_customrequest(h, string_of_http_action(action));
      if (http_1_0) {
        set_httpversion(h, HTTP_VERSION_1_0);
      };
      Option.may(set_timeout(h), timeout);
      Option.may(set_useragent(h), ua);
      let () = setup(h);
      ();
    };

    let nr_http = {
      incr(nr_http);
      nr_http^;
    }; /* XXX atomic wrt ocaml threads */
    if (verbose) {
      let action = string_of_http_action(action);
      let body =
        switch (body) {
        | None => ""
        | Some(`Form(args)) =>
          String.concat(" ") @@
          List.map(
            ((k, v)) =>
              sprintf("%s=\"%s\"", k, Stre.shorten(~escape=true, 64, v)),
            args,
          )
        | Some(`Raw(ct, body)) =>
          sprintf("%s \"%s\"", ct, Stre.shorten(~escape=true, 64, body))
        | Some(`Chunked(ct, _f)) => sprintf("%s chunked", ct)
        };

      log#info("%s #%d %s %s", action, nr_http, url, body);
    };
    let t = new Action.timer;
    let result =
      if (verbose) {
        Some(verbose_curl_result(nr_http, action, t));
      } else {
        None;
      };
    http_gets(~setup, ~timer?, ~result?, ~max_size?, url);
  };

  let http_request =
      (
        ~ua=?,
        ~timeout=?,
        ~verbose=?,
        ~setup=?,
        ~timer=?,
        ~max_size=?,
        ~http_1_0=?,
        ~headers=?,
        ~body=?,
        action: http_action,
        url,
      ) =>
    http_request'(
      ~ua?,
      ~timeout?,
      ~verbose?,
      ~setup?,
      ~timer?,
      ~max_size?,
      ~http_1_0?,
      ~headers?,
      ~body?,
      action,
      url,
    )
    >>= (res => return @@ simple_result(~verbose?, res));

  let http_request_exn =
      (
        ~ua=?,
        ~timeout=?,
        ~verbose=?,
        ~setup=?,
        ~timer=?,
        ~max_size=?,
        ~http_1_0=?,
        ~headers=?,
        ~body=?,
        action: http_action,
        url,
      ) =>
    http_request(
      ~ua?,
      ~timeout?,
      ~verbose?,
      ~setup?,
      ~timer?,
      ~max_size?,
      ~http_1_0?,
      ~headers?,
      ~body?,
      action,
      url,
    )
    >>= (
      fun
      | `Ok(s) => return(s)
      | `Error(error) => fail("%s", error)
    );

  let http_query =
      (
        ~ua=?,
        ~timeout=?,
        ~verbose=?,
        ~setup=?,
        ~timer=?,
        ~max_size=?,
        ~http_1_0=?,
        ~headers=?,
        ~body=?,
        action: http_action,
        url,
      ) => {
    let body =
      switch (body) {
      | Some((ct, s)) => Some(`Raw((ct, s)))
      | None => None
      };
    http_request(
      ~ua?,
      ~timeout?,
      ~verbose?,
      ~setup?,
      ~timer?,
      ~max_size?,
      ~http_1_0?,
      ~headers?,
      ~body?,
      action,
      url,
    );
  };

  let http_submit =
      (
        ~ua=?,
        ~timeout=?,
        ~verbose=?,
        ~setup=?,
        ~timer=?,
        ~http_1_0=?,
        ~headers=?,
        ~action=`POST,
        url,
        args,
      ) =>
    http_request(
      ~ua?,
      ~timeout?,
      ~verbose?,
      ~setup?,
      ~timer?,
      ~http_1_0?,
      ~headers?,
      ~body=`Form(args),
      action,
      url,
    );
};

module IO_blocking = {
  type t('a) = 'a;
  let return = identity;
  let (>>=) = (m, f) => f(m);
  let bracket = bracket;
  let fail = Exn.fail;
  let raise = raise;
  let sleep = Nix.sleep;
  let map_s = List.map;
  let catch = (f, e) =>
    try(f()) {
    | exn => e(exn)
    };
};

module IO_lwt = {
  type t('a) = Lwt.t('a);
  let return = Lwt.return;
  let (>>=) = Lwt.(>>=);
  let bracket = (mresource, destroy, k) => {
    let%lwt resource = mresource;
    (k(resource))([%finally destroy(resource)]);
  };
  let fail = Exn_lwt.fail;
  let raise = Lwt.fail;
  let sleep = Lwt_unix.sleep;
  let map_s = Lwt_list.map_s;
  let catch = Lwt.catch;
};

module Curl_blocking = {
  type t('a) = 'a;
  let perform = h =>
    try(
      {
        Curl.perform(h);
        Curl.CURLE_OK;
      }
    ) {
    | Curl.CurlException((code, _, _)) => code
    };
};

module Curl_lwt_for_http = {
  type t('a) = Lwt.t('a);
  include Curl_lwt;
};

module Http_blocking = Http(IO_blocking, Curl_blocking);
module Http_lwt = Http(IO_lwt, Curl_lwt_for_http);

let with_curl = Http_blocking.with_curl;
let with_curl_cache = Http_blocking.with_curl_cache;
let http_request' = Http_blocking.http_request';
let http_request = Http_blocking.http_request;
let http_request_exn = Http_blocking.http_request_exn;
let http_query = Http_blocking.http_query;
let http_submit = Http_blocking.http_submit;

let http_request_lwt' = Http_lwt.http_request';
let http_request_lwt = Http_lwt.http_request;
let http_request_lwt_exn = Http_lwt.http_request_exn;
let http_query_lwt = Http_lwt.http_query;
let http_submit_lwt = Http_lwt.http_submit;

/* http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html */
let string_of_http_code =
  fun
  | 100 => "Continue"
  | 101 => "Switching protocols"

  | 200 => "OK"
  | 201 => "Created"
  | 202 => "Accepted"
  | 203 => "Non-Authoritative Information"
  | 204 => "No Content"
  | 205 => "Reset Content"
  | 206 => "Partial Content"

  | 300 => "Multiple Choices"
  | 301 => "Moved Permanently"
  | 302 => "Found"
  | 303 => "See Other"
  | 304 => "Not Modified"
  | 305 => "Use Proxy"
  | 306 => "(Unused)"
  | 307 => "Temporary Redirect"

  | 400 => "Bad Request"
  | 401 => "Unauthorized"
  | 402 => "Payment Required"
  | 403 => "Forbidden"
  | 404 => "Not Found"
  | 405 => "Method Not Allowed"
  | 406 => "Not Acceptable"
  | 407 => "Proxy Authentication Required"
  | 408 => "Request Timeout"
  | 409 => "Conflict"
  | 410 => "Gone"
  | 411 => "Length Required"
  | 412 => "Precondition Failed"
  | 413 => "Request Entity Too Large"
  | 414 => "Request-URI Too Long"
  | 415 => "Unsupported Media Type"
  | 416 => "Requested Range Not Satisfiable"
  | 417 => "Expectation Failed"

  | 500 => "Internal Server Error"
  | 501 => "Not Implemented"
  | 502 => "Bad Gateway"
  | 503 => "Service Unavailable"
  | 504 => "Gateway Timeout"
  | 505 => "HTTP Version Not Supported"

  | _ => "(Unknown)";

let class_of_http_code = code =>
  switch (code / 100) {
  | 1 => "Informational"
  | 2 => "Successful"
  | 3 => "Redirection"
  | 4 => "Client Error"
  | 5 => "Server Error"
  | n => sprintf("%dxx", n)
  };
