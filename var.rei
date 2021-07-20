/**
  Global register for various types of counters.
  {!Logstash} module will send all [Var] counters to logstash automatically.
  Counters must be mononotonically increasing for logstash to send correct deltas to Elasticsearch.
*/;

type attributes = list((string, string));
type t =
  | Time(Time.t)
  | Count(int)
  | Bytes(int);

let show_a: list((string, string)) => string;
let is_in_families: string => bool;

/** [new typ type ?attr key] registers new [type] of counters with designated [attr]ibutes and [key] name */

class typ:
  (string, ~attr: attributes=?, string) =>
  {
    pub ref: 'a. ('a, 'a => t, string) => ref('a);
    pub get_count: (string, unit => option(int)) => unit;
    pub get_bytes: (string, unit => option(int)) => unit;
    pub get_time: (string, unit => option(Time.t)) => unit;
    pub count: string => ref(int);
    pub bytes: string => ref(int);
    pub time: string => ref(float);
    pub unregister: unit => unit;
    pub get: list((string, t));
    pub show: string;
  };

/** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name

Logstash events will have attributes as follows :
 * all of [attr] key value pairs (if given)
 * class=[type]
 * [key]=X where X is value inserted into [CC]

Guidelines for picking names :
  keep number of different [key] names low (makes ES happy),
  uniqueness of events is primarily provided by [class].

Bad example :
  let pages = new Var.cc "tool.pages" "pages"
  let index = new Var.cc "tool.index" "index"
  let count = new Var.cc "tool.count" "count"

Better :
  let pages = new Var.cc "tool.pages" "kind"
  let pages = new Var.cc "tool.index" "kind"
  let pages = new Var.cc "tool.count" "kind"
*/

let cc:
  ('a => string, string, ~attr: attributes=?, string) => Cache.Count.t('a);

/** [cc pp type ?attr key] new set of counters with designated [type], [attr]ibutes and [key] name, treated as milliseconds */

let cc_ms:
  ('a => string, string, ~attr: attributes=?, string) => Cache.Count.t('a);

/* val show : unit -> string */

/** callback takes attributes and value */

let iter: ((attributes, t) => unit) => unit;

/** [list_stats filter]

    @return a list containing a printed line for each counter whose type is in [filter].
*/

let list_stats: list(string) => list(string);
