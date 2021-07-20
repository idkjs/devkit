/** Very simple and incomplete HTTP server */;

open Printf;
open ExtLib;
module Ev = Libevent;

open Prelude;

let buffer_size = 4096;

/* hide log */
module Hidden = {
  let log = Log.from("httpev");
};
open Hidden;

/** {2 Server} */;

type max_time = {
  headers: Time.t,
  body: Time.t,
  send: Time.t,
};

/** server configuration */

type config = {
  connection: Unix.sockaddr,
  backlog: int,
  log_epipe: bool,
  /** more logging */
  mutable debug: bool,
  events: Ev.event_base,
  access_log: ref(out_channel),
  access_log_enabled: bool,
  name: string,
  max_request_size: int,
  auth: option((string, string, string)),
  /** limit on total number of requests in processing at any point of time */
  max_clients: int,
  max_data_childs: int,
  max_data_waiting: int,
  /** do [Lwt_unix.yield ()] after accepting connection to give other lwt threads chance to run (set to [true] when http requests
          processing causes other threads to stuck) */
  yield: bool,
  /** only process one request at a time (intended for preforked workers) */
  single: bool,
  /** if set, stop accepting connections as soon as exit_thread terminates (defaults to [Daemon.should_exit_lwt]) */
  exit_thread: option(Lwt.t(unit)),
  reuseport: bool,
  nodelay: bool,
  /** default false, if enabled - will in particular fail on "/path?arg1&arg2", why would anyone want that? */
  strict_args: bool,
  max_time,
  cors_allow_all: bool, /* default false, if enabled - automatically set `Access-Control-Allow-Origin: *` for simple requests */
  /** what to do on receive timeout, [`Reply_bad_request] by default */
  on_recv_timeout: [ | `Reply_bad_request | `Drop_connection],
};

let default_max_time = {
  headers: Time.seconds(30),
  body: Time.minutes(1),
  send: Time.minutes(2),
};

let default = {
  connection:  ADDR_INET(Unix.inet_addr_loopback, 8080),
  backlog: 100,
  log_epipe: false,
  debug: false,
  events: Ev.Global.base,
  name: "HTTP server",
  max_request_size: 256 * 1024,
  auth: None,
  max_clients: 10_000,
  access_log: ref(stdout),
  access_log_enabled: true,
  max_data_childs: 50,
  max_data_waiting: 200,
  yield: true,
  single: false,
  exit_thread: Some(Daemon.should_exit_lwt),
  reuseport: false,
  nodelay: false,
  strict_args: false,
  max_time: default_max_time,
  cors_allow_all: false,
  on_recv_timeout: `Reply_bad_request,
};

include Httpev_common;

type partial_body = {
  line1: string,
  content_length: option(int),
  parsed_headers: list((string, string)),
  buf: Buffer.t,
};

type request_state =
  | Headers(Buffer.t)
  | Body(partial_body)
  | Body_lwt(int)
  | Ready(request);

/** client state */

type client = {
  fd: Unix.file_descr,
  req_id: int,
  /** time when client connected */
  time_conn: Time.t,
  sockaddr: Unix.sockaddr,
  mutable req: request_state,
  server,
}

/** server state */

and server = {
  listen_socket: Unix.file_descr,
  mutable total: int,
  mutable active: int,
  mutable errors: int,
  mutable reject: int,
  reqs: Hashtbl.t(int, request),
  clients: Hashtbl.t(int, client),
  config,
  digest_auth: option(Digest_auth.t),
  /** currently running forked childs */
  h_childs: Hashtbl.t(int, unit),
  /** the stack of requests to fork */
  q_wait: Stack.t(unit => unit),
};

let incr_active = s => s.active = s.active + 1;
let decr_active = s => s.active = s.active - 1;
let incr_total = s => s.total = s.total + 1;
let incr_errors = s => s.errors = s.errors + 1;
let incr_reject = s => s.reject = s.reject + 1;

let make_server_state = (fd, config) => {
  let digest_auth =
    switch (config.auth) {
    | Some((realm, user, password)) =>
      Some(Digest_auth.init(~realm, ~user, ~password, ()))
    | None => None
    };

  {
    total: 0,
    active: 0,
    errors: 0,
    reject: 0,
    reqs: Hashtbl.create(10),
    clients: Hashtbl.create(10),
    config,
    digest_auth,
    listen_socket: fd,
    h_childs: Hashtbl.create(16),
    q_wait: Stack.create(),
  };
};

let show_socket_error = fd =>
  try(
    switch (Unix.getsockopt_int(fd, Unix.SO_ERROR)) {
    | 0 => ""
    | n => sprintf(", socket error %d", n)
    }
  ) {
  | _ => ""
  };

let show_peer = c =>
  sprintf(
    "%s (%s%s)",
    Nix.show_addr(c.sockaddr),
    Time.duration_str(Time.now() -. c.time_conn),
    show_socket_error(c.fd),
  );

let show_client = c =>
  switch (c.req) {
  | Headers(b) =>
    sprintf(
      "%s headers %s",
      show_peer(c),
      Action.bytes_string @@ Buffer.length(b),
    )
  | Body(b) =>
    sprintf(
      "%s body %s",
      show_peer(c),
      Action.bytes_string @@ Buffer.length(b.buf),
    )
  | Body_lwt(n) =>
    sprintf("%s body %s", show_peer(c), Action.bytes_string(n))
  | Ready(req) => show_request(req)
  };

type result('a, 'b) = [ | `Ok('a) | `Error('b)];

let space = Pcre.regexp("[ \t]+");

type reason =
  | Url
  | Version
  | Method
  | Header
  | RequestLine
  | Split
  | Extra
  | NotAcceptable;
exception Parse(reason, string);
let failed = (reason, s) => {
  let name =
    switch (reason) {
    | Url => "url"
    | Version => "version"
    | Method => "method"
    | RequestLine => "RequestLine"
    | Split => "split"
    | Header => "header"
    | Extra => "Extra"
    | NotAcceptable => "Not Acceptable"
    };

  let s = Stre.shorten(1024, s);
  raise( Parse(reason, sprintf("%s : %S", name, s)));
};

let get_content_length = headers =>
  switch (Exn.catch(List.assoc("content-length"), headers)) {
  | None => None
  | Some(s) =>
    try(Some(int_of_string(s))) {
    | _ => failed(Header, sprintf("content-length %S", s))
    }
  };

let decode_args = s =>
  try(Netencoding.Url.dest_url_encoded_parameters(s)) {
  | exn => Exn.fail(~exn, "decode_args : %S", s)
  };

let acceptable_encoding = headers => {
  let split = (s, c) =>
    List.map(String.strip(~chars=" \t\r\n")) @@ Stre.nsplitc(s, c);
  switch (Exn.catch(List.assoc("accept-encoding"), headers)) {
  | Some(str) =>
    let encodings =
      split(str, ',')
      |> List.filter_map(s =>
           switch (split(String.lowercase(s), ';')) {
           | [enc] => Some((enc, None))
           | [enc, q] => Some((enc, Some(q)))
           | _ =>
             log#warn("bad accept-encoding record, ignoring : %S", s);
             None;
           }
         );
    let test_available = s =>
      if (List.mem((s, Some("q=0")), encodings)) {
        false;
      } else if (List.exists(((enc, _)) => enc == s, encodings)) {
        true;
      } else if (List.mem(("*", Some("q=0")), encodings)) {
        false;
      } else if (List.exists(((enc, _)) => enc == "*", encodings)) {
        true;
      } else {
        s == "identity";
      }; /* identity is always accepted, unless prohibited */

    if (test_available("gzip")) {
      Gzip;
    } else if (test_available("identity")) {
      Identity;
    } else {
      Exn.fail("not acceptable : %S", str);
    };
  | None => Identity
  };
};

let make_request_exn = (~line1, ~headers, ~body, c) =>
  switch (Pcre.split(~rex=space, line1)) {
  | [meth, url, version] =>
    if (url.[0] != '/') {
      /* abs_path */
      failed(Url, url);
    };
    let version =
      try(
        switch (String.split(version, "/")) {
        | ("HTTP", version) =>
          apply2(int_of_string, String.split(version, "."))
        | _ => raise(Not_found)
        }
      ) {
      | _ => failed(Version, version)
      };

    let meth =
      try(method_of_string(meth)) {
      | Failure(_) => failed(Method, meth)
      };
    let (path, args) =
      try(String.split(url, "?")) {
      | _ => (url, "")
      };
    if (version == (1, 1) && !List.mem_assoc("host", headers)) {
      failed(Header, "Host is required for HTTP/1.1");
    };
    let decode_args =
      if (c.server.config.strict_args) {
        decode_args;
      } else {
        Web.parse_url_args $ String.strip;
      };
    let args = decode_args(args);
    let cont_type =
      try(List.assoc("content-type", headers)) {
      | _ => ""
      };
    let args =
      if (cont_type == "application/x-www-form-urlencoded") {
        List.append(args) @@ decode_args(body);
      } else {
        args;
      };
    let encoding =
      try(acceptable_encoding(headers)) {
      | Failure(s) => failed(NotAcceptable, s)
      };
    {
      url,
      path,
      args,
      headers,
      body,
      meth,
      id: c.req_id,
      addr: c.sockaddr,
      conn: c.time_conn,
      recv: Time.get(),
      version,
      blocking: None,
      line: line1,
      socket: c.fd,
      encoding,
    };
  | _ => failed(RequestLine, line1)
  };

let extract_header = s =>
  String.(
    try({
      let (n, v) = split(s, ":");
      (lowercase(strip(n)), strip(v));
    }) {
    | _ => failed(Header, s)
    }
  );

let extract_headers = data =>
  switch (String.nsplit(data, "\r\n")) {
  | [] => failed(Split, data)
  | [line1, ...xs] => (line1, List.map(extract_header, xs))
  };

let is_body_ready = ({line1, content_length, parsed_headers: _, buf}, final) =>
  switch (content_length, Buffer.contents(buf)) {
  | (None, "") => true
  | (None, body) => failed(Extra, body)
  | (Some(length), body) =>
    let body_len = String.length(body);
    switch (body_len - length) {
    | 0 => true
    /* workaround MSIE 6 */
    | 2
        when
          String.starts_with(line1, "POST")
          && body.[body_len - 2] == '\r'
          && body.[body_len - 1] == '\n' =>
      Buffer.clear(buf);
      Buffer.add_string(buf, Stre.slice(~last=-2, body));
      true;
    | n when final || n > 0 =>
      Exn.fail("wrong content-length : %d <> %d", length, body_len)
    | _ => false
    };
  };

/* let int_of_fd : Unix.file_descr -> int = Obj.magic */

let teardown = fd => {
  /*   Log.info "close %u" (int_of_fd fd); */
  Exn.suppress(Unix.shutdown(fd), Unix.SHUTDOWN_ALL);
  Unix.close(fd);
};

let finish = (~shutdown=true, c) => {
  decr_active(c.server);
  Hashtbl.remove(c.server.clients, c.req_id);
  if (shutdown) {
    teardown(c.fd);
  } else {
    Unix.close(c.fd);
  };
  switch (c.req) {
  | Headers(_)
  | Body(_)
  | Body_lwt(_) => ()
  | Ready(req) =>
    Hashtbl.remove(c.server.reqs, req.id);
    if (c.server.config.debug) {
      log#info("finished %s", show_request(req));
    };
  };
};

let write_f = (c, (data, ack), ev, fd, _flags) => {
  let finish = () => {
    finish(c);
    Ev.del(ev);
  };
  let rec loop = (l, ack) =>
    switch (l) {
    | [] =>
      finish();
      ([], 0);
    | [s, ...xs] when String.length(s) == 0 => loop(xs, 0) /* skip empty strings */
    | [s, ...xs] =>
      try({
        let len =
          Unix.single_write_substring(fd, s, ack, String.length(s) - ack);
        let ack = ack + len;
        if (ack == String.length(s)) {
          loop(xs, 0);
        } else {
          loop(l, ack);
        };
      }) {
      |  Unix.Unix_error(Unix.EAGAIN, _, _) => (l, ack)
      }
    };

  try({
    let (l, a) = loop(data^, ack^);
    data := l;
    ack := a;
  }) {
  | exn =>
    incr_errors(c.server);
    finish();
    switch (c.server.config.log_epipe, exn) {
    | (false,  Unix.Unix_error(Unix.EPIPE, _, _)) => ()
    | _ => log#warn(~exn, "write_f %s", show_client(c))
    };
  };
};

let log_access_apache = (ch, code, size, ~background=false, req) =>
  try({
    let now = Time.now();
    fprintf(
      ch,
      "%s - - [%s] %S %d %dB . %S %S %.3f %s %S%s\n%!",
      show_client_addr(req),
      Time.to_string(now),
      req.line,
      code,
      size,
      header_referer(req),
      header_safe(req, "user-agent"),
      now -. req.conn,
      header_safe(req, "host"),
      header_safe(req, "x-request-id"),
      if (background) {" (BG)"} else {""},
    );
  }) {
  | exn => log#warn(~exn, "access log : %s") @@ show_request(req)
  };

let log_status_apache = (ch, status, size, req) =>
  switch (status) {
  | `No_reply => () /* ignore */
  | #reply_status as code =>
    log_access_apache(ch, status_code(code), size, req)
  };

/** Wait until [fd] becomes readable and close it (for eventfd-backed notifications) */

let wait = (base, fd, k) =>
  Async.simple_event(
    base,
    fd,
    [Ev.READ],
    (ev, fd, _) => {
      Ev.del(ev);
      Exn.suppress(Unix.close, fd);
      k();
    },
  );

let write_some = (fd, s) => {
  let slen = String.length(s);
  if (slen == 0) {
    `Done;
  } else {
    try({
      let len = Unix.write_substring(fd, s, 0, String.length(s));
      if (len == slen) {
        `Done;
      } else {
        `Some(len);
      };
    }) {
    |  Unix.Unix_error(Unix.EAGAIN, _, _) => `Some(0)
    };
  };
};

/** close transport connection, count as error */

let abort = (c, exn, msg) => {
  incr_errors(c.server);
  finish(c);
  switch (c.server.config.log_epipe, exn) {
  | (false,  Unix.Unix_error(Unix.EPIPE, _, _)) => ()
  | _ => log#warn(~exn, "abort %s %s", msg, show_client(c))
  };
};

let write_reply = (c, l) => {
  let rec loop = l =>
    switch (l) {
    | [] => finish(c)
    | [s, ...tl] =>
      switch (write_some(c.fd, s)) {
      | `Some(n) =>
        Async.setup_simple_event(
          c.server.config.events,
          c.fd,
          [Ev.WRITE],
          write_f(c, (ref(l), ref(n))),
        )
      | `Done => loop(tl)
      }
    };

  try(loop(l)) {
  | exn => abort(c, exn, "write_reply")
  };
};

let write_reply_blocking = (c, s) =>
  try({
    let n = Unix.write_substring(c.fd, s, 0, String.length(s));
    assert(n == String.length(s));
  }) {
  | exn => abort(c, exn, "write_reply_blocking")
  };

let set_blocking = req => {
  let ch = Unix.out_channel_of_descr(req.socket);
  let encode =
    switch (req.encoding) {
    | Identity => id
    | Gzip => Gzip_io.output
    };
  let io = encode @@ IO.output_channel(ch);
  req.blocking = Some(io);
  io;
};

let make_request_headers = (code, hdrs) => {
  let b = Buffer.create(1024);
  let put = s => {
    Buffer.add_string(b, s);
    Buffer.add_string(b, "\r\n");
  };
  put(show_http_reply(code));
  List.iter(((n, v)) => bprintf(b, "%s: %s\r\n", n, v), hdrs);
  put("Connection: close");
  put("");
  Buffer.contents(b);
};

let send_reply_async = (c, encoding, (code, hdrs, body)) =>
  /* possibly apply encoding */
  try({
    let (hdrs, body) =
      /* TODO do not apply encoding to application/gzip */
      switch (encoding) {
      | Gzip when String.length(body) > 128 => (
          [("Content-Encoding", "gzip"), ...hdrs],
          Gzip_io.string(body),
        )
      | _ => (hdrs, body)
      };

    let hdrs = [
      ("Content-Length", string_of_int(String.length(body))),
      ...hdrs,
    ];
    /* do not transfer body for HEAD requests */
    let body =
      switch (c.req) {
      | Ready({meth: `HEAD, _}) => ""
      | _ => body
      };
    let headers = make_request_headers(code, hdrs);
    if (c.server.config.debug) {
      log#info(
        "will answer to %s with %d+%d bytes",
        show_peer(c),
        String.length(headers),
        String.length(body),
      );
    };
    write_reply(c, [headers, body]);
  }) {
  | exn => abort(c, exn, "send_reply_async")
  };

let send_reply_blocking = (c, (code, hdrs)) =>
  try(write_reply_blocking(c) @@ make_request_headers(code, hdrs)) {
  | exn =>
    abort(c, exn, "send_reply_blocking");
    raise(exn);
  };

let maybe_allow_cors = (c, h) =>
  switch (c.req) {
  | Ready(req) =>
    switch (
      c.server.config.cors_allow_all,
      req.meth,
      List.mem_assoc("origin", req.headers),
      List.mem_assoc("access-control-allow-origin", h),
    ) {
    | (true, `GET | `HEAD | `POST, true, false) => [
        ("Access-Control-Allow-Origin", "*"),
        ...h,
      ]
    | _ => h
    }
  | _ => h
  };

/* this function is called back by user to actually send data */
let send_reply_user = (c, req, (code, hdrs, body)) =>
  switch (code) {
  | `No_reply => finish(~shutdown=false, c)
  | #reply_status as code =>
    let hdrs =
      switch (hdrs) {
      /* hack for answer_forked, which logs on its own */
      | [("X-Disable-Log", "true"), ...hs] => hs
      | _ =>
        if (c.server.config.access_log_enabled) {
          log_status_apache(
            c.server.config.access_log^,
            code,
            String.length(body),
            req,
          );
        };
        hdrs;
      };

    let hdrs = maybe_allow_cors(c, hdrs);
    let blocking = Option.is_some(req.blocking);
    /* filter headers */
    open Stre;
    let hdrs =
      hdrs
      |> List.filter(((k, _)) =>{
          //  open Stre;
           let forbidden =
             iequal(k, "content-length")
             && !blocking  /* httpev will calculate */
             || iequal(k, "connection")
             || iequal(k, "content-encoding"); /* none of the user's business */

           !forbidden;
         });
    blocking
      /* this is forked child, events are gone, so write to socket with blocking */
      ? {
        Unix.clear_nonblock(c.fd);
        let hdrs =
          switch (req.encoding) {
          | Identity => hdrs
          | Gzip => [("Content-Encoding", "gzip"), ...hdrs]
          };
        send_reply_blocking(c, (code, hdrs));
      }
      : send_reply_async(c, req.encoding, (code, hdrs, body));
  };

let make_error =
  fun
  |  Parse(what, msg) => {
      let error =
        switch (what) {
        | Url
        | RequestLine
        | Header
        | Split
        | Version
        | Extra => `Bad_request
        | Method => `Not_implemented
        | NotAcceptable => `Not_acceptable
        };

      (error, msg);
    }
  | Failure(s) => (`Bad_request, s)
  | exn => (`Bad_request, Exn.str(exn));

let send_reply_limit = (c, n) => {
  log#info(
    "request too large from %s : %s",
    show_client(c),
    Action.bytes_string(n),
  );
  send_reply_async(
    c,
    Identity,
    (`Request_too_large, [], "request entity too large"),
  );
};

let handle_request = (c, body, answer) => {
  let req = {
    let {line1, parsed_headers: headers, buf, _} = body;
    make_request_exn(c, ~line1, ~headers, ~body=Buffer.contents(buf));
  };

  Hashtbl.replace(c.server.reqs, req.id, req);
  c.req = Ready(req);
  try(
    switch (req.version) {
    | (1, _) =>
      let auth =
        switch (c.server.digest_auth) {
        | Some(auth) => Digest_auth.check(auth, req)
        | None => `Ok
        };

      let k = send_reply_user(c, req);
      switch (auth) {
      | `Unauthorized(header) =>
        k((`Unauthorized, [header], "Unauthorized"))
      | `Ok => answer(c.server, req, k)
      };
    | _ =>
      log#info(
        "version %u.%u not supported from %s",
        fst(req.version),
        snd(req.version),
        show_request(req),
      );
      send_reply_async(
        c,
        Identity,
        (`Version_not_supported, [], "HTTP/1.0 is supported"),
      );
    }
  ) {
  | exn =>
    log#error(~exn, "answer %s") @@ show_request(req);
    switch (req.blocking) {
    | None => send_reply_async(c, Identity, (`Not_found, [], "Not found"))
    | Some(_) => Exn.suppress(teardown, c.fd)
    };
  };
};

let rec process_chunk = (c, ev, answer, data, final) =>
  switch (c.req) {
  | Body_lwt(_) => assert(false)
  | Headers(buf)
  | Body({buf, _})
      when
        String.length(data)
        + Buffer.length(buf) > c.server.config.max_request_size =>
    Ev.del(ev);
    send_reply_limit(c, String.length(data) + Buffer.length(buf));
  | Headers(buf) =>
    Buffer.add_string(buf, data);
    let input = Buffer.contents(buf);
    switch (
      try(Some(String.split(input, "\r\n\r\n"))) {
      | _ => None
      }
    ) {
    | None =>
      if (final) {
        failed(Split, input);
      } /* continue reading headers */
    | Some((headers, part)) =>
      let (line1, headers) = extract_headers(headers);
      let content_length = get_content_length(headers);
      /* TODO transfer-encoding */
      if (List.mem_assoc("transfer-encoding", headers)) {
        Exn.fail("Transfer-Encoding not supported");
      };
      Buffer.clear(buf);
      let body = {line1, parsed_headers: headers, content_length, buf};
      c.req = Body(body);
      process_chunk(c, ev, answer, part, final);
    };
  | Body(body) =>
    Buffer.add_string(body.buf, data);
    if (is_body_ready(body, final)) {
      Ev.del(ev);
      handle_request(c, body, answer);
    } else if (final) {
      failed(Split, Buffer.contents(body.buf));
    };
  | Ready(req) =>
    if (data == "" && final == true) {
      log#warn("STRANGE %s %B", show_request(req), final);
      failed(Split, "");
    } else {
      failed(Extra, data);
    }
  };

let handle_client = (c, answer) =>
  Async.setup_simple_event(
    c.server.config.events, c.fd, [Ev.READ], (ev, fd, _) =>
    try(
      switch (
        Async.read_available(~limit=c.server.config.max_request_size, fd)
      ) {
      | `Limit(s) =>
        Ev.del(ev);
        send_reply_limit(c, String.length(s));
      | `Chunk(data, final) => process_chunk(c, ev, answer, data, final)
      }
    ) {
    | exn =>
      Ev.del(ev);
      let (http_error, msg) = make_error(exn);
      log#warn("error for %s : %s", show_client(c), msg);
      send_reply_async(c, Identity, (http_error, [], ""));
    }
  );

module Tcp = {
  open Unix;

  let listen = (~name, ~backlog=100, ~reuseport=false, addr) => {
    let domain = domain_of_sockaddr(addr);
    let fd = socket(~cloexec=true, domain, SOCK_STREAM, 0);
    try(
      {
        setsockopt(fd, SO_REUSEADDR, true);
        if (reuseport) {
          U.setsockopt(fd, SO_REUSEPORT, true);
        };
        bind(fd, addr);
        listen(fd, backlog);
        log#info("%s listen TCP %s", name, Nix.show_addr(addr));
        fd;
      }
    ) {
    | exn =>
      log#warn(~exn, "%s listen TCP %s failed", name, Nix.show_addr(addr));
      close(fd);
      raise(exn);
    };
  };

  let handle = (events, fd, k) => {
    set_nonblock(fd);
    let rec setup = () =>
      Async.setup_simple_event(events, fd, [Ev.READ], (ev, fd, _) =>
        try(
          while (true) {
            /* accept as much as possible, socket is nonblocking */
            let peer = accept(~cloexec=true, fd);
            try(k(peer)) {
            | exn =>
              log#error(~exn, "accepted (%s)", Nix.show_addr(snd(peer)))
            };
          }
        ) {
        |  Unix_error(EAGAIN, _, _) => ()
        | exn =>
          /*
                 log #error ~exn "accept (total requests %d)" (Hashtbl.length status.reqs);
                 Hashtbl.iter (fun _ req -> log #error "%s" (show_request req)) status.reqs;
           */
          switch (exn) {
          |  Unix_error(EMFILE, _, _) =>
            let tm = 2.;
            log#error(
              "disable listening socket for %s ",
              Time.duration_str(tm),
            );
            Ev.del(ev);
            let timer = Ev.create();
            Ev.set_timer(
              events,
              timer,
              ~persist=false,
              () => {
                Ev.del(timer);
                log#info("reenabling listening socket");
                setup();
              },
            );
            Ev.add(timer, Some(tm));
          | _ => ()
          }
        }
      );

    setup();
  };

  let handle_lwt = (~single=false, fd, k) =>
    switch%lwt (Exn_lwt.map(Lwt_unix.accept, fd)) {
    | `Exn( Unix.Unix_error(Unix.EMFILE, _, _)) =>
      let pause = 2.;
      log#error(
        "too many open files, disabling accept for %s",
        Time.duration_str(pause),
      );
      Lwt_unix.sleep(pause);
    | `Exn(Lwt.Canceled) =>
      log#info("canceling accept loop");
      Lwt.fail(Lwt.Canceled);
    | `Exn(exn) =>
      log#warn(~exn, "accept");
      Lwt.return_unit;
    | `Ok((fd, addr) as peer) =>
      let task =
        (
          try%lwt(
            {
              Unix.set_close_on_exec(Lwt_unix.unix_file_descr(fd));
              k(peer);
            }
          ) {
          | exn =>
            log#warn(~exn, "accepted (%s)", Nix.show_addr(addr));
            Lwt.return_unit;
          }
        )(
          [%lwt.finally
            {
              Lwt_unix.(Exn.suppress(shutdown(fd), SHUTDOWN_ALL));
              Lwt_unix.close(fd);
            }
          ],
        );

      if (single) {
        task;
      } else {
        Lwt.ignore_result(task); /* "fork" processing */
        Lwt.return_unit;
      };
    };
};

let check_hung_requests = server => {
  let now = Time.now();
  server.reqs
  |> Hashtbl.iter((_, req) =>
       if (req.recv -. now > Time.minutes(30)) {
         log#warn(
           "request takes too much time to process : %s",
           show_request(req),
         );
       }
     );
};

let check_waiting_requests = srv =>
  while (!Stack.is_empty(srv.q_wait)
         && Hashtbl.length(srv.h_childs) < srv.config.max_data_childs) {
    let f = Stack.pop(srv.q_wait);
    try({
      let () = f();
      ();
    }) {
    | exn => log#warn(~exn, "q_wait")
    };
  };

let finish_child = (srv, pid) =>
  /*     log #info "child %d reaped" pid; */
  switch (Hashtbl.find_option(srv.h_childs, pid)) {
  | Some () =>
    Hashtbl.remove(srv.h_childs, pid);
    check_waiting_requests(srv);
  | None => log#warn("no handler for child %d", pid)
  };

let reap_orphans = srv => {
  let rec loop = () =>
    switch (Exn.catch(Unix.waitpid([Unix.WNOHANG]), 0)) {
    | None
    | Some((0, _)) => ()
    | Some((pid, st)) =>
      log#info("reaped orphan %d %S", pid, Std.dump(st));
      finish_child(srv, pid);
      loop();
    };
  loop();
};

let start_listen = config =>
  Tcp.listen(
    ~name=config.name,
    ~backlog=config.backlog,
    ~reuseport=config.reuseport,
    config.connection,
  );

let setup_server_fd = (fd, config, answer) => {
  let server = make_server_state(fd, config);
  Async.setup_periodic_timer_wait(config.events, Time.minutes(1), () =>
    check_hung_requests(server)
  );
  Async.setup_periodic_timer_now(config.events, 10., () =>
    reap_orphans(server)
  );
  Tcp.handle(
    config.events,
    fd,
    ((fd, sockaddr)) => {
      incr_total(server);
      let req_id = server.total;
      server.active >= config.max_clients
        ? {
          incr_reject(server);
          if (config.debug) {
            log#info("rejected #%d %s", req_id, Nix.show_addr(sockaddr));
          };
          teardown(fd);
        }
        : {
          incr_active(server);
          let client = {
            fd,
            req_id,
            sockaddr,
            time_conn: Time.get(),
            server,
            req: Headers(Buffer.create(1024)),
          };
          Hashtbl.replace(server.clients, req_id, client);
          Unix.set_nonblock(fd);
          if (config.debug) {
            log#info("accepted #%d %s", req_id, Nix.show_addr(sockaddr));
          };
          handle_client(client, answer);
        };
    },
  );
  server;
};

let setup_server = (config, answer) => {
  let fd = start_listen(config);
  setup_server_fd(fd, config, answer);
};

let setup_fd = (fd, config, answer) => {
  let _: server = setup_server_fd(fd, config, answer);
  ();
};
let setup = (config, answer) => {
  let _: server = setup_server(config, answer);
  ();
};

let server = (config, answer) => {
  setup(config, answer);
  Ev.dispatch(config.events);
};

let header = (n, v) => (n, v);
let forbidden = (`Forbidden, [], "forbidden");
let not_found = (`Not_found, [], "not found");
let found = url => (`Found, [header("Location", url)], "found");
let moved = url => (`Moved, [header("Location", url)], "moved permanently");
let cache_no = [
  header("Pragma", "no-cache"),
  header("Cache-Control", "max-age=0"),
];
let cache_yes = t => [header("Last-Modified", Time.to_rfc2822(t))];

/*
 let answer st url =
   match url with
   | "/test" ->
     let body = sprintf "answer %s\n%s" url (String.create 102400) in
     `Ok,[],answer
   | _ -> not_found

 let () =
   server (Unix.ADDR_INET (Unix.inet_addr_any, 8081)) answer
 */

/** {2 Utilities} */;

/** get request params */
module Param = {
  let get_exn = (req, name) => List.assoc(name, req.args);
  exception Bad(string);
  let get = req => Exn.catch(get_exn(req));
  let get_int = req => Exn.catch(int_of_string $ get_exn(req));
  let make = (f, req, ~default=?, name) =>
    switch (get(req, name), default) {
    | (None, None) => raise(Bad(name))
    | (None, Some(s)) => s
    | (Some(s), _) =>
      try(f(s)) {
      | _ => raise(Bad(name))
      }
    };
  let str = make(id);
  let int64 = make(Int64.of_string);
  let int = make(int_of_string);
  let float = make(float_of_string);
  let bool = make(bool_of_string);
  let array = (req, name) => {
    let name = name ++ "[]";
    req.args |> List.filter(((name', _)) => name == name') |> List.map(snd);
  };
};

module type Args = {
  let req: request;

  exception Bad(string);

  /** Get optional parameter. @return None if parameter is missing */

  let get: string => option(string);

  /** Get required parameter. @raise Bad if parameter is missing and no [default] provided */

  let str: (~default: string=?, string) => string;

  /** Get optional integer parameter */

  let get_int: string => option(int);

  /** Get integer parameter. @raise Bad if parameter is missing and no [default] provided */

  let int: (~default: int=?, string) => int;

  /** Get boolean parameter. @return [true] when parameter value is ["true"], [false] otherwise. @raise Bad if missing and no [default] provided. */

  let bool: (~default: bool=?, string) => bool;

  let float: (~default: float=?, string) => float;
  let int64: (~default: int64=?, string) => int64;
  /** @param name array name without brackets e.g. [array "x"] to extract [x] from /request?x[]=1&x[]=2 */

  let array: string => list(string);
};

/** functor version of {!Param} because somebody thought it is good idea */
module Args = (T: {let req: request;}) : Args => {
  include Param;
  let req = T.req;
  let get = get(req);
  let get_int = get_int(req);
  let str = str(req);
  let int64 = int64(req);
  let int = int(req);
  let float = float(req);
  let bool = bool(req);
  let array = array(req);
};

let noclose_io = io =>
  IO.create_out(
    ~write=IO.write(io),
    ~output=IO.output(io),
    ~flush=() => IO.flush(io),
    ~close=() => (),
  );

/** Buffers all output */

let output = (f: IO.output('a) => unit) => {
  let out = IO.output_string();
  f @@ noclose_io(out);
  IO.close_out(out);
};

let serve = (_req: request, ~status=?, ~extra=[], ctype, data) => (
  Option.default(`Ok, status),
  [("Content-Type", ctype), ...extra],
  data,
);

let serve_io =
    (req: request, ~status=?, ~extra=?, ctype, f: IO.output('a) => unit) =>
  serve(req, ~status?, ~extra?, ctype, output(f));

let serve_text_io = (req, ~status=?) =>
  serve_io(req, ~status?, "text/plain");

let serve_gzip_io = (req, ~status=?, f) =>
  serve_io(req, ~status?, "application/gzip", io =>
    Control.with_output(Gzip_io.output(io), f)
  );

let serve_text = (req, ~status=?, text) =>
  serve(req, ~status?, "text/plain", text);

let run = (~ip=Unix.inet_addr_loopback, port, answer) =>
  server(
    {...default, connection:  ADDR_INET(ip, port)},
    answer,
  );

let run_unix = (path, answer) =>
  server({...default, connection: ADDR_UNIX(path)}, answer);

/** {2 Forked workers} */;

let check_req = req =>
  switch (Unix.getsockopt_int(req.socket, Unix.SO_ERROR)) {
  | 0 => `Ok
  | n => `Error(n)
  };
let check_req_exn = req =>
  switch (check_req(req)) {
  | `Ok => ()
  | `Error(n) => Exn.fail("socket error %d", n)
  };

exception Continue(unit => unit);

let answer_blocking = (~debug=false, srv, req, answer, k) => {
  let count = ref(0L);
  let (code, continue) =
    try(
      {
        if (debug) {
          Printexc.record_backtrace(true);
        };
        Control.with_output(
          set_blocking(req),
          io => {
            /* TODO EPIPE closes fd, EBADF afterwards */
            let io = Action.count_bytes_to(count, io);
            let pre = h => {
              k((`Ok, [("X-Disable-Log", "true"), ...h], ""));
              check_req_exn(req);
            };

            check_req_exn(req);
            let () = answer(pre, io);
            (200, None);
          },
        );
      }
    ) {
    | Continue(continue) => (200, Some(continue))
    | exn =>
      let saved_backtrace = Exn.get_backtrace();
      log#warn(
        ~exn,
        ~backtrace=debug,
        ~saved_backtrace,
        "answer forked %s",
        show_request(req),
      );
      ((-1), None);
    };

  if (srv.config.access_log_enabled) {
    log_access_apache(
      srv.config.access_log^,
      code,
      Int64.to_int(count^),
      ~background=continue != None,
      req,
    );
  };
  call_me_maybe(continue, ());
};

let stats = (new Var.typ)("httpev.forks", "k");

let nr_forked = stats#count("forked");
let nr_queued = stats#count("queued");
let nr_rejected = stats#count("rejected");

let answer_forked = (~debug=?, srv, req, answer, k) => {
  let do_fork = () =>
    switch (check_req(req)) {
    | `Error(n) =>
      Exn.fail("pre fork %s : socket error %d", show_request(req), n)
    | `Ok =>
      switch (Lwt_unix.fork()) {
      | 0 =>
        Exn.suppress(Unix.close, srv.listen_socket);
        try(answer_blocking(~debug?, srv, req, answer, k)) {
        | exn =>
          log#error(~exn, "unhandled exception in continuation callback")
        };
        U.sys_exit(0);
      | (-1) => Exn.fail("fork failed : %s", show_request(req))
      | pid =>
        log#info("forked %d : %s", pid, show_request(req));
        k((`No_reply, [], "")); /* close socket in parent immediately */
        Hashtbl.add(srv.h_childs, pid, ());
      }
    };

  let do_fork = () =>
    try(do_fork()) {
    | exn =>
      log#warn(~exn, "answer fork failed %s", show_request(req));
      k((`Internal_server_error, [], ""));
    };

  if (Hashtbl.length(srv.h_childs) < srv.config.max_data_childs) {
    incr(nr_forked);
    do_fork();
  } else if (Stack.length(srv.q_wait) < srv.config.max_data_waiting) {
    incr(nr_queued);
    Stack.push(do_fork, srv.q_wait);
  } else {
    incr(nr_rejected);
    log#info("rejecting, overloaded : %s", show_request(req));
    k((
      `Service_unavailable,
      [("Content-Type", "text/plain")],
      "overloaded",
    ));
  };
};

/** {2 Lwt support} */;

let timeout = (tm, thread) => Lwt_unix.with_timeout(tm, () => thread);

let send_reply = (c, cout, reply) => {
  /* repack */
  let (code, hdrs, body) =
    switch (reply) {
    | `Body(code, hdrs, s) => (code, hdrs, `Body(s))
    | `Chunks(code, hdrs, gen) => (code, hdrs, `Chunks(gen))
    };

  let hdrs = maybe_allow_cors(c, hdrs);
  switch (c.req) {
  | Ready(req) =>
    let size =
      switch (body) {
      | `Body(s) => String.length(s)
      | `Chunks(_) => 0
      };
    if (c.server.config.access_log_enabled) {
      log_status_apache(c.server.config.access_log^, code, size, req);
    };
  | _ => () /* this can happen when sending back error reply on malformed HTTP input */
  };
  /* filter headers */
  // open Stre;
  let hdrs =
    hdrs
    |> List.filter(((k, _)) =>{
         open Stre;
         let forbidden =
           iequal(k, "content-length")  /* httpev will calculate */
           || iequal(k, "connection")
           || iequal(k, "transfer-encoding")
           || iequal(k, "content-encoding"); /* none of the user's business */

         !forbidden;
       });

  /* possibly apply encoding */
  let (hdrs, body) =
    /* TODO do not apply encoding to application/gzip */
    /* TODO gzip + chunked? */
    switch (body, code, c.req) {
    | (`Body(s), `Ok, Ready({encoding: Gzip, _}))
        when String.length(s) > 128 => (
        [("Content-Encoding", "gzip"), ...hdrs],
        `Body(Gzip_io.string(s)),
      )
    | _ => (hdrs, body)
    };

  let hdrs =
    switch (body) {
    | `Body(s) => [
        ("Content-Length", string_of_int(String.length(s))),
        ...hdrs,
      ]
    | `Chunks(_) => [("Transfer-Encoding", "chunked"), ...hdrs]
    };

  /* do not transfer body for HEAD requests */
  let body =
    switch (c.req) {
    | Ready({meth: `HEAD, _}) => `Body("")
    | _ => body
    };
  let headers = make_request_headers(code, hdrs);
  if (c.server.config.debug) {
    log#info(
      "will answer to %s with %d+%s bytes",
      show_peer(c),
      String.length(headers),
      switch (body) {
      | `Body(s) => sprintf("%d", String.length(s))
      | `Chunks(_) => "..."
      },
    );
  };
  let%lwt () = Lwt_io.write(cout, headers);
  let%lwt () =
    switch (body) {
    | `Body(s) => Lwt_io.write(cout, s)
    | `Chunks(gen) =>
      let push = (
        fun
        | "" => Lwt.return_unit
        | s => {
            let%lwt () =
              Lwt_io.write(cout, sprintf("%x\r\n", String.length(s)));
            let%lwt () = Lwt_io.write(cout, s);
            Lwt_io.write(cout, "\r\n");
          }
      );

      try%lwt(
        {
          let%lwt () = gen(push);
          Lwt_io.write(cout, "0\r\n\r\n");
        }
      ) {
      | exn =>
        /* do not write trailer on error - let the peer notice the breakage */
        log#warn(~exn, "generate failed");
        Lwt.return_unit;
      };
    };

  Lwt_io.flush(cout);
};

let handle_request_lwt = (c, req, answer) => {
  let return = x => Lwt.return @@ `Body(x);
  switch (req.version) {
  | (1, _) =>
    let auth =
      switch (c.server.digest_auth) {
      | Some(auth) => Digest_auth.check(auth, req)
      | None => `Ok
      };

    switch (auth) {
    | `Unauthorized(header) =>
      return((`Unauthorized, [header], "Unauthorized"))
    | `Ok =>
      try%lwt(answer(c.server, req)) {
      | exn =>
        log#error(~exn, "answer %s") @@ show_request(req);
        return((`Not_found, [], "Not found"));
      }
    };
  | _ =>
    log#info(
      "version %u.%u not supported from %s",
      fst(req.version),
      snd(req.version),
      show_request(req),
    );
    return((`Version_not_supported, [], "HTTP/1.0 is supported"));
  };
};

let read_buf = (ic, buf) => {
  let len = Bytes.length(buf);
  let%lwt real_len = Lwt_io.read_into(ic, buf, 0, len);
  if (real_len < len) {
    Lwt.return(Bytes.(sub(buf, 0, real_len) |> unsafe_to_string));
  } else {
    Lwt.return(Bytes.unsafe_to_string(buf));
  };
};

module ReqBuffersCache =
  Cache.Reuse({
    type t = Bytes.t;
    let create = () => Bytes.create(2048);
    let reset = ignore;
  });

let read_headers = (cin, limit) => {
  let rec loop = (~acc=?, temp) =>
    switch (acc) {
    | Some(acc) when String.length(acc) >= limit =>
      Exn_lwt.fail(
        "request larger than max_request_size %s",
        Action.bytes_string(limit),
      )
    | _ =>
      switch%lwt (read_buf(cin, temp)) {
      | "" =>
        Exn_lwt.fail("client disconnected prior to sending full request")
      | buf =>
        let acc =
          switch (acc) {
          | None => buf
          | Some(acc) => acc ++ buf
          };
        switch (String.split(acc, "\r\n\r\n")) {
        | exception _ =>
          loop(
            ~acc,
            if (acc === Bytes.unsafe_to_string(temp)) {
              Bytes.create(2048);
            } else {
              temp;
            },
          )
        | (headers, body) =>
          Lwt.return((String.nsplit(headers, "\r\n"), body))
        };
      }
    };

  let temp = ReqBuffersCache.get();
  (loop(temp))(
    [%lwt.finally
      {
        ReqBuffersCache.release(temp);
        Lwt.return_unit;
      }
    ],
  );
};

let handle_client_lwt = (client, cin, answer) => {
  let cfg = client.server.config;
  let%lwt (headers, data) =
    timeout(cfg.max_time.headers, read_headers(cin, cfg.max_request_size));
  switch (headers) {
  | [] => failed(RequestLine, "missing")
  | [line1, ...headers] =>
    let headers = List.map(extract_header, headers);
    let content_length = get_content_length(headers);
    /* TODO transfer-encoding */
    if (List.mem_assoc("transfer-encoding", headers)) {
      Exn.fail("Transfer-Encoding not supported");
    };
    let%lwt data =
      switch (content_length) {
      | None when data == "" => Lwt.return("")
      | None => failed(Extra, data)
      | Some(n) when String.length(data) > n =>
        failed(Extra, Stre.slice(~first=n, data))
      | Some(n) when String.length(data) == n => Lwt.return(data)
      | Some(n) =>
        let s = Bytes.create(n - String.length(data));
        let%lwt () =
          timeout(
            cfg.max_time.body,
            Lwt_io.read_into_exactly(cin, s, 0, n - String.length(data)),
          );
        Lwt.return(data ++ Bytes.unsafe_to_string(s));
      };

    /* TODO check that no extra bytes arrive */
    client.req = Body_lwt(String.length(data));
    let req = make_request_exn(client, ~line1, ~headers, ~body=data);
    client.req = Ready(req);
    Hashtbl.replace(client.server.reqs, req.id, req);
    (handle_request_lwt(client, req, answer))(
      [%lwt.finally
        {
          Hashtbl.remove(client.server.reqs, req.id);
          Lwt.return_unit;
        }
      ],
    );
  };
};

let accept_hook = ref(() => ());

let handle_lwt = (config, fd, k) => {
  let rec loop = () => {
    accept_hook^();
    let%lwt () = Tcp.handle_lwt(~single=config.single, fd, k);
    let%lwt () =
      if (config.yield) {
        Lwt_unix.yield();
      } else {
        Lwt.return_unit;
      };
    loop();
  };

  let%lwt () =
    switch (config.exit_thread) {
    | Some(exit) => Lwt.pick([exit, loop()])
    | None => loop()
    };

  log#info("%s %s exit", config.name, Nix.show_addr(config.connection));
  Lwt.return_unit;
};

module BuffersCache =
  Cache.Reuse({
    type t = Lwt_bytes.t;
    let create = () => Lwt_bytes.create(buffer_size);
    let reset = ignore;
  });

let setup_fd_lwt = (fd, config, answer) => {
  let server =
    make_server_state(
      Lwt_unix.unix_file_descr(fd /* will not be used */),
      config,
    );
  Lwt.ignore_result(
    while%lwt (true) {
      let%lwt () = Lwt_unix.sleep @@ Time.minutes(1);
      Lwt.wrap1(check_hung_requests, server);
    },
  );
  handle_lwt(
    config,
    fd,
    ((fd, sockaddr)) => {
      incr_total(server);
      let req_id = server.total;
      server.active >= config.max_clients
        ? {
          incr_reject(server);
          if (config.debug) {
            log#info("rejected #%d %s", req_id, Nix.show_addr(sockaddr));
          };
          Lwt.return_unit;
        }
        : {
          let error = lazy(incr_errors(server));
          let client =
            /* used only in show_socket_error */
            {
              fd: Lwt_unix.unix_file_descr(fd),
              req_id,
              sockaddr,
              time_conn: Time.get(),
              server,
              req: Headers(Buffer.create(0)),
            };

          if (config.debug) {
            log#info("accepted #%d %s", req_id, Nix.show_addr(sockaddr));
          };
          incr_active(server);
          Hashtbl.replace(server.clients, req_id, client);
          let buffer = BuffersCache.get();
          {
            let cin =
              Lwt_io.(of_fd(~buffer, ~close=Lwt.return, ~mode=input, fd));
            let%lwt reply =
              try%lwt(handle_client_lwt(client, cin, answer)) {
              | exn =>
                !!error;
                switch (exn, config.on_recv_timeout) {
                | (Lwt_unix.Timeout, `Drop_connection) =>
                  log#info(
                    ~exn,
                    "dropped #%d %s",
                    req_id,
                    show_client(client),
                  );
                  Lwt.return(`No_reply);
                | (exn, _) =>
                  let (http_error, msg) = make_error(exn);
                  log#warn("error for %s : %s", show_client(client), msg);
                  Lwt.return @@ `Body((http_error, [], ""));
                };
              };

            switch (reply) {
            | `No_reply => Lwt.return_unit
            | (`Body(_) | `Chunks(_)) as reply =>
              if (config.nodelay) {
                Lwt_unix.setsockopt(fd, TCP_NODELAY, true);
              };
              /* reusing same buffer! */
              let cout =
                Lwt_io.(of_fd(~buffer, ~close=Lwt.return, ~mode=output, fd));
              try%lwt(
                switch (reply) {
                | `Chunks(_) => send_reply(client, cout, reply)
                | _ =>
                  timeout(
                    config.max_time.send,
                    send_reply(client, cout, reply),
                  )
                }
              ) {
              | exn =>
                !!error;
                log#warn(~exn, "send_reply %s", show_client(client));
                Lwt.return_unit;
              };
            };
          }(
            [%lwt.finally
              {
                decr_active(server);
                Hashtbl.remove(server.clients, req_id);
                BuffersCache.release(buffer);
                Lwt.return_unit;
              }
            ],
          );
        };
    },
  );
};

let setup_lwt = (config, answer) => {
  let fd =
    Lwt_unix.of_unix_file_descr(~blocking=false) @@ start_listen(config);
  (setup_fd_lwt(fd, config, answer))([%lwt.finally Lwt_unix.close(fd)]);
};

let server_lwt = (config, answer) =>
  Lwt_main.run @@ setup_lwt(config, answer);

module Answer = {
  let return =
      (~status=`Ok, ~extra=[], ~typ, data)
      : Lwt.t([> | `Body(reply'(reply_status))]) =>
    Lwt.return @@ `Body((status, [("Content-Type", typ), ...extra], data));

  let text = return(~typ="text/plain");
  let binary = return(~typ="application/octet-stream");
  let printf = (~status=?, ~extra=?, fmt) =>
    ksprintf(s => text(~status?, ~extra?, s), fmt);
  let json = return(~typ="application/json");
  let yojson = (~status=?, ~extra=?, x) =>
    json(~status?, ~extra?, Yojson.Safe.to_string(x));

  let error = (status, s) => text(~status, s);
  let not_found = error(`Not_found);
  let bad_request = error(`Bad_request);
  let internal_error = error(`Internal_server_error);

  exception No_path;

  let random_ref = () => Random.int64(Int64.max_int);

  let rest = (~show_exn, req, answer) =>
    try%lwt(
      {
        module Arg =
          Args({
            let req = req;
          });
        answer((module Arg): (module Args), (req.meth, req.path));
      }
    ) {
    | No_path =>
      bad_request @@
      sprintf("unrecognized path %s %s", show_method(req.meth), req.url)
    | Arg.Bad(s) =>
      bad_request @@ sprintf("bad parameter %s in %s", s, req.url)
    | exn =>
      let ref = random_ref();
      log#warn(~exn, "failed ref:%Ld %s", ref, show_request(req));
      if (show_exn) {
        internal_error @@
        sprintf(
          "internal error ref:%Ld : %s",
          ref,
          switch (exn) {
          | Failure(s) => s
          | _ => Exn.str(exn)
          },
        );
      } else {
        internal_error @@ sprintf("undisclosed internal error ref:%Ld", ref);
      };
    };
};
