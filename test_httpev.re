/** Bare-bones httpev server example */;

open Printf;
open Devkit;

let log = Httpev.Hidden.log;

let http_handle = (_st, req, k_http) => {
  module Arg =
    Httpev.Args({
      let req = req;
    });
  switch (req.Httpev.path) {
  | "/hello" =>
    let name = Option.default("world", Arg.get("name"));
    k_http @@ Httpev.serve_text(req, sprintf("Hello, %s!", name));
  | _ =>
    log#warn("not found : %s", Httpev.show_request(req));
    k_http @@ Httpev.not_found;
  };
};

let run = http_port => {
  let main = () => {
    let http_config = {
      ...Httpev.default,
      Httpev.events: Async.Ev.init(),
      connection: [@implicit_arity] ADDR_INET(Unix.inet_addr_any, http_port),
      max_request_size: 128 * 1024,
    };
    Httpev.server(http_config, http_handle);
  };

  Action.log(main, ());
};
