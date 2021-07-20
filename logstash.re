open Printf;
open ExtLib;
open Prelude;

let log = Log.from("logstash");

let state = Hashtbl.create(100);

let dynamic = Hashtbl.create(100);

let escape = k =>
  if (String.contains(k, ' ')) {
    String.map(
      fun
      | ' ' => '_'
      | c => c,
      k,
    );
  } else {
    k;
  };

let zero =
  Var.(
    fun
    | Count(_) => Count(0)
    | Time(_) => Time(0.)
    | Bytes(_) => Bytes(0)
  );

module J = Yojson.Safe;

type json = [ | `Float(float) | `Int(int) | `String(string)];

module Dyn = {
  open Var;

  type t = list((string, json));

  let show_a = Stre.list(((c, _)) => sprintf("%S:''", c));

  let make_family = family => {
    let f = List.unique(~cmp=((a, _), (b, _)) => a == b, family);
    if (List.length(f) != List.length(family)) {
      log#warn("duplicate attributes : %s", show_a(family));
    };
    List.sort(~cmp=((a, _), (b, _)) => String.compare(a, b), family);
  };

  let make = (~attrs=[], name) =>
    is_in_families(name)
      ? Exn.fail("static class with this name already exists: %s", name)
      : make_family([("class", `String(name)), ...attrs]);

  let extend = (dyn, attrs) =>
    switch (attrs) {
    | [] => dyn
    | attrs => make_family(dyn) @ attrs
    };

  let set = (dyn, ~attrs=[], v) => {
    let family = extend(dyn, attrs);
    Hashtbl.replace(dynamic, family, v);
  };

  let add = (dyn, ~attrs=[], v) => {
    let family = extend(dyn, attrs);
    switch (Hashtbl.find_default(dynamic, family, zero(v)), v) {
    | (Count(x), Count(x')) =>
      let n = x + x';
      Hashtbl.replace(dynamic, family, Count(n));
    | (Bytes(x), Bytes(x')) =>
      let n = x + x';
      Hashtbl.replace(dynamic, family, Bytes(n));
    | (Time(x), Time(x')) =>
      let n = x +. x';
      Hashtbl.replace(dynamic, family, Time(n));
    | (Count(_), Bytes(_))
    | (Count(_), Time(_))
    | (Bytes(_), Count(_))
    | (Bytes(_), Time(_))
    | (Time(_), Count(_))
    | (Time(_), Bytes(_)) =>
      log#warn("mismatched value type for %s", show_a(family))
    };
  };

  let set_count = (dyn, attrs, v) => set(dyn, ~attrs, Count(v));
  let set_bytes = (dyn, attrs, v) => set(dyn, ~attrs, Bytes(v));
  let set_time = (dyn, attrs, v) => set(dyn, ~attrs, Time(v));
  let add_count = (dyn, attrs, v) => add(dyn, ~attrs, Count(v));
  let add_bytes = (dyn, attrs, v) => add(dyn, ~attrs, Bytes(v));
  let add_time = (dyn, attrs, v) => add(dyn, ~attrs, Time(v));
};

let timestamp_field = () => ("timestamp_ms", `Int(Time.(to_ms @@ now())));
let common_fields = () => [
  timestamp_field(),
  ("pid", `String(Pid.show_self())),
  ("from", `String(Pid.self().Pid.name)),
];

let get = () => {
  open Var;
  let l = ref([]);
  Var.iter((attr', v) =>{
    let (previous, attr) =
      try(Hashtbl.find(state, attr')) {
      | Not_found =>
        let a = List.map(((k, s)) => (escape(k), `String(s)), attr');
        let x = (ref(zero(v)), a);
        Hashtbl.add(state, attr', x);
        x;
      };

    let this = (
      common_fields() @ attr: list((string, json)) :>
        list((string, [> json]))
    );
    switch (v, previous^) {
    | (Count(x), Count(x')) =>
      let delta = x - x';
      if (delta != 0) {
        previous := v;
        tuck(l) @@ `Assoc([("count", `Int(delta)), ...this]);
      };
    | (Bytes(x), Bytes(x')) =>
      let delta = x - x';
      if (delta != 0) {
        previous := v;
        tuck(l) @@ `Assoc([("bytes", `Int(delta)), ...this]);
      };
    | (Time(x), Time(x')) =>
      let delta = x -. x';
      if (delta > epsilon_float) {
        previous := v;
        tuck(l) @@ `Assoc([("seconds", `Float(delta)), ...this]);
      };
    | (Count(_), Bytes(_))
    | (Count(_), Time(_))
    | (Bytes(_), Count(_))
    | (Bytes(_), Time(_))
    | (Time(_), Count(_))
    | (Time(_), Bytes(_)) =>
      log#warn(
        "the impossible happened : mismatched type for %S",
        show_a(attr'),
      )
    };
  });
  dynamic
  |> Hashtbl.iter((attr, v) =>{
       let attr = List.map(((k, s)) => (escape(k), s), attr);
       let this = (
         common_fields() @ attr: list((string, json)) :>
           list((string, [> json]))
       );
       let add = c => tuck(l) @@ `Assoc([c, ...this]);
       switch (v) {
       | Count(x) => add(("count", `Int(x)))
       | Bytes(x) => add(("bytes", `Int(x)))
       | Time(x) => add(("seconds", `Float(x)))
       };
     });
  Hashtbl.clear(dynamic);
  l^;
};

let get_basename = () =>
  switch (Daemon.logfile^) {
  | None =>
    log#warn("no logfile, disabling logstash stats too");
    None;
  | Some(logfile) =>
    let f =
      try(Filename.chop_extension(logfile)) {
      | _ => logfile
      };
    log#info("will output logstash stats to %s.YYYYMMDD.json", f);
    Some(f);
  };

let open_logstash_exn = basename => {
  let filename =
    sprintf(
      "%s.%s.json",
      basename,
      Time.format_date8 @@ Unix.gmtime @@ Time.now(),
    );
  try(Files.open_out_append_text(filename)) {
  | exn => Exn.fail(~exn, "failed to open stats file %s", filename)
  };
};

let write_json = (activity, out, nr, json) => {
  let bytes = J.to_string(~std=true, json) ++ "\n";
  /* try not to step on the other forks toes, page writes are atomic */
  try(
    {
      if (String.length(bytes) > 4096 - nr^) {
        activity := true;
        flush(out);
        nr := 0;
      };
      output_string(out, bytes);
      nr := nr^ + String.length(bytes);
    }
  ) {
  | exn => log#warn(~exn, "failed to write event %S", bytes)
  };
};

let line_writer = out => {
  let nr = ref(0);
  write_json(ref(false), out, nr);
};

let setup_ = setup =>
  switch (get_basename()) {
  | None => ()
  | Some(stat_basename) =>
    setup(() =>
      switch (open_logstash_exn(stat_basename)) {
      | exception exn => log#warn(~exn, "disabling output")
      | ch =>
        Control.bracket(
          ch,
          close_out_noerr,
          ch => {
            let write = line_writer(ch);
            get() |> List.iter(write);
            flush(ch);
          },
        )
      }
    )
  };

let default_period = Time.seconds(60);
let setup = (~pause=default_period, events) =>
  setup_(f => {
    at_exit(f);
    Async.setup_periodic_timer_wait(events, pause, f);
  });
let setup_lwt = (~pause=default_period, ()) =>
  setup_(f => {
    at_exit(f);
    let rec loop = () =>
      Daemon.should_exit()
        ? Lwt.return_unit
        : Lwt.bind(
            Lwt_unix.sleep(pause),
            () => {
              f();
              loop();
            },
          );

    Lwt.async(loop);
  });

let round_to_midnight = timestamp => {
  let ms = Time.to_ms(timestamp);
  let diff = ms mod (Time.days(1) |> Time.to_ms);
  timestamp -. Time.msec(diff);
};

let is_same_day = timestamp => Time.now() -. Time.days(1) < timestamp;

type logger = {
  .
  event: list((string, Yojson.Safe.t)) => unit,
  write: unit => unit,
  reload: unit => unit,
  flush: unit => unit,
};

let null = {
  as _;
  pub event = _j => ();
  pub write = () => ();
  pub reload = () => ();
  pub flush = () => ()
};

let log =
    (~autoflush=?, ~verbose=false, ~add_timestamp_only=false, ~name=?, ()) => {
  let add_fields =
    if (add_timestamp_only) {
      l => [timestamp_field(), ...l];
    } else {
      l => common_fields() @ l;
    };
  let name =
    switch (name) {
    | None => get_basename()
    | Some(_) => name
    };
  switch (name) {
  | None => null
  | Some(stat_basename) =>
    switch (open_logstash_exn(stat_basename)) {
    | exception exn =>
      log#warn(~exn, "disabling output");
      null;
    | out =>
      let out = ref(out);
      let nr = ref(0);
      let activity = ref(false);
      switch (autoflush) {
      | None => ()
      | Some(delay) =>
        let rec l = () =>
          Daemon.should_exit()
            ? Lwt.return_unit
            : {
              activity := false;
              let%lwt () = Lwt_unix.sleep(delay);
              if (nr^ > 0 && ! activity^) {
                flush(out^);
                nr := 0;
              };
              l();
            };

        Lwt.async(l);
      };
      {
        as self;
        val mutable timestamp = round_to_midnight @@ Time.now();
        pub reload = () =>
          try(
            {
              if (verbose) {
                log#info("rotate log");
              };
              let new_out = open_logstash_exn(stat_basename);
              let prev = out^;
              out := new_out;
              nr := 0;
              timestamp = round_to_midnight @@ Time.now();
              flush(prev);
              close_out_noerr(prev);
            }
          ) {
          | exn => log#warn(~exn, "failed to rotate log")
          };
        pri try_rotate = () =>
          switch (timestamp) {
          | t when (!) @@ is_same_day(t) => self#reload()
          | _ => ()
          };
        pub write = () => {
          self#try_rotate();
          get() |> List.iter(write_json(activity, out^, nr));
        };
        pub event = j => {
          self#try_rotate();
          write_json(activity, out^, nr, `Assoc(add_fields(j)));
        };
        pub flush = () =>
          if (nr^ > 0 && ! activity^) {
            flush(out^);
            nr := 0;
          }
      };
    }
  };
};

let logstash_err = Lazy.from_fun @@ log(~name="log/errors");

let setup_error_log = () => {
  Signal.set_reload((!!logstash_err)#reload);
  let chain_hook = Log.State.hook^;
  Log.State.hook :=
    (
      (level, facil, s) => {
        if (level == `Error) {
          (!!logstash_err)#event([
            ("facility", `String(facil.Logger.name)),
            ("message", `String(s)),
          ]);
          (!!logstash_err)#flush();
        };
        chain_hook(level, facil, s);
      }
    );
};

let lifetime = (~extra="", ~events, ~version, ()) => {
  let text = sprintf("%s start version %s", Pid.self_name(), version);
  events#event([
    ("event", `String("start")),
    ("text", `String(text)),
    ("extra", `String(extra)),
    ("cmdline", `String(Nix.cmdline)),
    ("cwd", `String(Sys.getcwd())),
    (
      "user",
      `String(
        try(Unix.(getpwuid @@ geteuid()).pw_name) {
        | _ => ""
        },
      ),
    ),
    ("version", `String(version)),
  ]);
  events#flush();
  Signal.set_exit(() => {
    events#event([
      ("event", `String("signal.stop")),
      (
        "text",
        `String(sprintf("%s received stop signal", Pid.self_name())),
      ),
      ("version", `String(version)),
    ]);
    events#flush();
  });
  let pid = Unix.getpid();
  at_exit(() =>
    switch (pid == Unix.getpid()) {
    | false => () /* forked child */
    | true =>
      events#event([
        ("event", `String("exit")),
        ("text", `String(sprintf("%s exit", Pid.self_name()))),
        ("version", `String(version)),
      ]);
      events#flush();
    }
  );
};
