type t;
let init: (~realm: string, ~user: string, ~password: string, unit) => t;
let check:
  (t, Httpev_common.request) => [ | `Ok | `Unauthorized(string, string)];
