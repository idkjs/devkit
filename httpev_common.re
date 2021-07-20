open Printf;

type encoding =
  | Gzip
  | Identity;

type meth = [ | `GET | `POST | `PUT | `PATCH | `DELETE | `HEAD | `OPTIONS];

type request = {
  addr: Unix.sockaddr,
  url: string, /* path and arguments */
  path: string,
  args: list((string, string)),
  conn: Time.t, /* time when client connected */
  recv: Time.t, /* time when client request was fully read */
  meth,
  headers: list((string, string)),
  body: string,
  version: (int, int), /* client HTTP version */
  id: int, /* request id */
  socket: Unix.file_descr,
  /** request line */
  line: string,
  mutable blocking: option(IO.output(unit)), /* hack for forked childs */
  encoding,
};

type reply_status = [
  | `Ok
  | `Created
  | `No_content
  | `Found
  | `Moved
  | `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Conflict
  | `Length_required
  | `Request_too_large
  | `I'm_a_teapot
  | `Internal_server_error
  | `Not_implemented
  | `Service_unavailable
  | `Version_not_supported
  | `Custom(string)
];

type extended_reply_status = [ reply_status | `No_reply];

type reply'('status) = ('status, list((string, string)), string);
type reply = reply'(extended_reply_status);

let show_method =
  fun
  | `GET => "GET"
  | `POST => "POST"
  | `PUT => "PUT"
  | `PATCH => "PATCH"
  | `DELETE => "DELETE"
  | `HEAD => "HEAD"
  | `OPTIONS => "OPTIONS";

let method_of_string =
  fun
  | "GET" => `GET
  | "POST" => `POST
  | "PUT" => `PUT
  | "PATCH" => `PATCH
  | "DELETE" => `DELETE
  | "HEAD" => `HEAD
  | "OPTIONS" => `OPTIONS
  | s => Exn.fail("method_of_string %s", s);

let show_client_addr = (~via=[Unix.inet_addr_loopback], req) => {
  let header_or = default =>
    try(List.assoc("x-real-ip", req.headers)) {
    | Not_found => default
    };
  switch (req.addr) {
  | Unix.ADDR_UNIX(_) => header_or @@ Nix.show_addr(req.addr)
  | [@implicit_arity] ADDR_INET(addr, _) when List.mem(addr, via) =>
    header_or @@ Unix.string_of_inet_addr(addr)
  | [@implicit_arity] ADDR_INET(addr, _) => Unix.string_of_inet_addr(addr)
  };
};

let client_addr = req =>
  switch (req.addr) {
  | [@implicit_arity] Unix.ADDR_INET(addr, port) => (addr, port)
  | _ => assert(false)
  };
let client_ip = req => fst @@ client_addr(req);

let find_header = (req, name) =>
  List.assoc(String.lowercase_ascii(name), req.headers);
let header_exn = (req, name) =>
  try(find_header(req, name)) {
  | _ => Exn.fail("header %S", name)
  };
let header_safe = (req, name) =>
  try(find_header(req, name)) {
  | _ => ""
  };
let header_referer = req =>
  try(find_header(req, "Referer")) {
  | _ =>
    try(find_header(req, "Referrer")) {
    | _ => ""
    }
  };

let show_request = req =>
  sprintf(
    "#%d %s time %.4f (recv %.4f) %s %s%s %S %S",
    req.id,
    show_client_addr(req),
    Time.get() -. req.conn,
    req.recv -. req.conn,
    show_method(req.meth),
    header_safe(req, "host"),
    req.url,
    header_safe(req, "user-agent"),
    header_safe(req, "x-request-id"),
  );

let status_code: reply_status => int = (
  fun
  | `Ok => 200
  | `Created => 201
  | `No_content => 204

  | `Moved => 301
  | `Found => 302

  | `Bad_request => 400
  | `Unauthorized => 401
  | `Payment_required => 402
  | `Forbidden => 403
  | `Not_found => 404
  | `Method_not_allowed => 405
  | `Not_acceptable => 406
  | `Conflict => 409
  | `Length_required => 411
  | `Request_too_large => 413
  | `I'm_a_teapot => 418

  | `Internal_server_error => 500
  | `Not_implemented => 501
  | `Service_unavailable => 503
  | `Version_not_supported => 505

  | `Custom(_) => 999:
    reply_status => int
);

let show_http_reply: reply_status => string = (
  fun
  | `Ok => "HTTP/1.0 200 OK"
  | `Created => "HTTP/1.0 201 Created"
  | `No_content => "HTTP/1.0 204 No Content"

  | `Moved => "HTTP/1.0 301 Moved Permanently"
  | `Found => "HTTP/1.0 302 Found"

  | `Bad_request => "HTTP/1.0 400 Bad Request"
  | `Unauthorized => "HTTP/1.0 401 Unauthorized"
  | `Payment_required => "HTTP/1.0 402 Payment Required"
  | `Forbidden => "HTTP/1.0 403 Forbidden"
  | `Not_found => "HTTP/1.0 404 Not Found"
  | `Method_not_allowed => "HTTP/1.0 405 Method Not Allowed"
  | `Not_acceptable => "HTTP/1.0 406 Not Acceptable"
  | `Conflict => "HTTP/1.0 409 Conflict"
  | `Length_required => "HTTP/1.0 411 Length Required"
  | `Request_too_large => "HTTP/1.0 413 Request Entity Too Large"
  | `I'm_a_teapot => "HTTP/1.0 418 I'm a teapot"

  | `Internal_server_error => "HTTP/1.0 500 Internal Server Error"
  | `Not_implemented => "HTTP/1.0 501 Not Implemented"
  | `Service_unavailable => "HTTP/1.0 503 Service Unavailable"
  | `Version_not_supported => "HTTP/1.0 505 HTTP Version Not Supported"

  | `Custom(s) => s:
    reply_status => string
);

/* basically allow all */
let cors_preflight_allow_all = (
  `No_content,
  [
    ("Access-Control-Allow-Origin", "*"),
    (
      "Access-Control-Allow-Methods",
      "GET, POST, OPTIONS, PUT, PATCH, DELETE, HEAD",
    ),
    ("Access-Control-Max-Age", "600"),
  ],
  "",
);
