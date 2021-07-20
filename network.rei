/** Manipulating network addresses */;

type ipv4;
type ipv4_cidr;

exception Parse_ipv4(string);

let ipv4_null: ipv4;
let bytes_of_ipv4: ipv4 => (int, int, int, int);
let string_of_ipv4: ipv4 => string;

/** @raise Parse_ipv4 if input is not an IP */

let ipv4_of_string_exn: string => ipv4;

/** @return ip 0.0.0.0 when input is not an IP */

let ipv4_of_string_null: string => ipv4;

let ipv4_of_int32: int32 => ipv4;
let int32_of_ipv4: ipv4 => int32;
let is_ipv4_slow: string => bool;
let is_ipv4: string => bool;
let ipv4_of_int: int => ipv4;
let int_of_ipv4: ipv4 => int;
let class_c: ipv4 => ipv4;
let ipv4_to_yojson: ipv4 => Yojson.Safe.t;
let ipv4_of_yojson: Yojson.Safe.t => result(ipv4, string);

module IPv4: {
  type t = ipv4;
  let equal: (t, t) => bool;
  let compare: (t, t) => int;
  let null: t;
  let to_bytes: t => (int, int, int, int);
  let to_string: t => string;
  let of_string_exn: string => t;
  let of_string_null: string => t;
  let of_int32: int32 => t;
  let to_int32: t => int32;
  let of_int: int => t;
  let to_int: t => int;
  let class_c: t => t;
};

/** accepts addr/n notation or single ip */

let cidr_of_string_exn: string => ipv4_cidr;
let range_of_cidr: ipv4_cidr => (ipv4, ipv4);
let prefix_of_cidr: ipv4_cidr => ipv4;
let ipv4_matches: (ipv4, ipv4_cidr) => bool;
let is_ipv4_special: ipv4 => bool;
let special_cidr: list(ipv4_cidr);

/** @return ip address of this machine on private network, with 127.0.0.1 as a fallback, NB ipv4 only */

let private_ipv4_network_ip: unit => Unix.inet_addr;

let public_ipv4_network_ip: unit => option(Unix.inet_addr);
let public_ipv4_network_ip_exn: unit => Unix.inet_addr;

/** @return interfaces and associated ip addresses of this machine on public network. NB ipv4 only */

let public_ipv4_network_ips: unit => list((string, Unix.inet_addr));

/** @return interfaces and associated ip addresses of this machine on private network. NB ipv4 only */

let private_ipv4_network_ips: unit => list((string, Unix.inet_addr));

[@ocaml.deprecated "use private_ipv4_network_ip instead"]
let private_network_ip: unit => Unix.inet_addr;
[@ocaml.deprecated "use public_ipv4_network_ips instead"]
let public_network_ips: unit => list((string, Unix.inet_addr));
[@ocaml.deprecated "use private_ipv4_network_ips instead"]
let private_network_ips: unit => list((string, Unix.inet_addr));
