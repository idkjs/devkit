open ExtLib;
open Prelude;

let last_logs_max = 10;

let enabled = ref(false);

let is_enabled = () => enabled^;

/***/

module LastN = {
  type t('a) = {
    queue: Queue.t('a),
    mutable avail: int,
  };

  let create = n =>
    if (n < 0) {
      invalid_arg("LastN.create: n < 0");
    } else {
      {queue: Queue.create(), avail: n};
    };

  let add = (x, t) => {
    Queue.push(x, t.queue);
    if (t.avail == 0) {
      ignore(Queue.pop(t.queue));
    } else {
      t.avail = t.avail - 1;
    };
  };

  let to_list = t =>
    List.rev @@ Queue.fold((acc, x) => [x, ...acc], [], t.queue);
};

/***/

type id = int;

type kind =
  | Normal
  | Background
  | Status;

type lazy_string = Lazy.t(string);

type mark = {
  id,
  kind,
  name: lazy_string,
  parent_name: lazy_string,
  /** [id] is stored to find parent thread in !marks, but there are no direct links to parent's mark.
          [parent_{name,id}] don't reflect Lwt scheduling (so background thread's parent is not set to main/unnamed/toplevel); they are
          used to trace places where threads were born (control flow). */
  parent_id: id,
  logs: LastN.t(lazy_string),
};

/***/

let string_of_kind =
  fun
  | Normal => "normal"
  | Background => "background"
  | Status => "status";

/** [0] is a special value, not used by threads. */

let next_mark_id = ref(1);

let marks: Hashtbl.t(int, mark) = (Hashtbl.create(7): Hashtbl.t(int, mark));

let create = (~name, ~parent_id, ~parent_name, ~kind) => {
  id: {
    let id = next_mark_id^;
    next_mark_id := id + 1;
    id;
  },
  name,
  parent_id,
  parent_name,
  logs: LastN.create(last_logs_max),
  kind,
};

let register_mark = m =>
  switch (Hashtbl.find(marks, m.id)) {
  | exception Not_found => Hashtbl.add(marks, m.id, m)
  | _ => assert(false)
  };

let unregister_mark = m =>
  switch (Hashtbl.find(marks, m.id)) {
  | _ => Hashtbl.remove(marks, m.id)
  | exception Not_found => assert(false)
  };

let special = name => {
  let m =
    create(
      ~name=Lazy.from_val(name),
      ~parent_id=0,
      ~parent_name=Lazy.from_val(""),
      ~kind=Normal,
    );
  register_mark(m);
  m;
};

/** dummy parent of threads created by parents without mark */

let top_mark = special("<top>");

/** dummy parent of threads/statuses which parent has terminated */

let orphan_mark = special("<orphan>");

/***/

let log_add_line = (mark, msg) => {
  let msg =
    lazy({
      let msg = !!msg;
      if (String.ends_with(msg, "\n")) {
        msg;
      } else {
        msg ++ "\n";
      };
    });

  LastN.add(msg, mark.logs);
};

let log_to = (mark, msg) =>
  if (! enabled^) {
    ();
  } else {
    log_add_line(mark, msg);
  };

let key = Lwt.new_key();

let with_mark = (v, f) => Lwt.with_value(key, v, f);

let run_thread = (on_success, on_failure, func) =>
  switch (func()) {
  | thr =>
    Lwt.on_any(thr, on_success, on_failure);
    thr;
  | exception exn =>
    on_failure(exn);
    Lwt.fail(exn);
  };

let mark_or_orphan = id =>
  try(Hashtbl.find(marks, id)) {
  | Not_found => orphan_mark
  };

let log_exit = (mark, msg) => {
  let parent = mark_or_orphan(mark.parent_id);
  log_to(
    parent,
    {
      let {name, id, kind, parent_name, parent_id, logs: _} = mark;
      lazy(
        Printf.sprintf(
          "thread %S (#%i, %s%s) exit %s\n",
          !!name,
          id,
          string_of_kind(kind),
          if (parent === orphan_mark) {
            Printf.sprintf(", parent was %s#%i", !!parent_name, parent_id);
          } else {
            "";
          },
          !!msg,
        )
      );
    },
  );
};

/** separate function to ease reasoning about which values are kept in closures (here: only arguments and top-level values, no local
    bindings from [with_new_mark]) */

let run_with_mark = (~dump=?, ~log: option(Log.logger)=?, ~mark, cont, ()) => {
  register_mark(mark);
  let on_success = v => {
    unregister_mark(mark);
    log_exit(mark) @@
    (
      lazy(
        "ok"
        ++ (
          switch (dump) {
          | None => ""
          | Some(dump) => ", res: " ++ dump(v)
          }
        )
      )
    );
  };

  let on_failure = exn => {
    unregister_mark(mark);
    log_exit(mark) @@ (lazy("exn: " ++ Printexc.to_string(exn)));
    switch (log) {
    | None => ()
    | Some(log) => log#warn("thread %S failed", !!mark.name, ~exn)
    };
  };

  run_thread(on_success, on_failure, cont);
};

let with_new_mark = (~dump=?, ~log=?, ~name, ~kind, cont) =>
  if (! enabled^) {
    cont();
  } else {
    let new_mark = {
      let (parent_name, parent_id) = {
        let parent = Option.default(top_mark, Lwt.get(key));
        (parent.name, parent.id);
      };

      create(~name, ~kind, ~parent_name, ~parent_id);
    };

    with_mark(Some(new_mark)) @@
    run_with_mark(~dump?, ~log?, ~mark=new_mark, cont);
  };

/***/

let name = (name, cont) =>
  with_new_mark(~name=Lazy.from_val(name), ~kind=Normal, cont);

let status = (name, ~dump=?, cont) =>
  with_new_mark(~name, ~dump?, ~kind=Status, cont);

let status_s = (name, ~dump=?, cont) =>
  status(Lazy.from_val(name), ~dump?, cont);

let async = (~log=?, name, run_thread) =>
  Lwt.async @@
  (
    () =>
      with_new_mark(~log?, ~name=Lazy.from_val(name), ~kind=Background) @@
      run_thread
  );

let log_do = msg => {
  let mark = Option.default(top_mark, Lwt.get(key));
  log_add_line(mark, msg);
};

let log_l = msg =>
  if (! enabled^) {
    ();
  } else {
    log_do(msg);
  };

let log_do_strict = msg => log_do(Lazy.from_val(msg));

let log = msg =>
  if (! enabled^) {
    ();
  } else {
    log_do_strict(msg);
  };

let log_f = fmt =>
  if (! enabled^) {
    Printf.ikfprintf(ignore, (), fmt);
  } else {
    Printf.ksprintf(log_do_strict, fmt);
  };

/***/

let rec parent_of_status = parent_id => {
  let parent = mark_or_orphan(parent_id);
  switch (parent.kind) {
  | Normal
  | Background => parent
  | Status => parent_of_status(parent.parent_id)
  };
};

let summary = () => {
  let b = Buffer.create(100);
  open Printf;
  Buffer.add_string(b, "Lwt_mark status (running threads):\n");
  if (enabled^) {
    let statuses = Hashtbl.create(7);
    Hashtbl.iter(
      (_id, mark) =>
        switch (mark.kind) {
        | Normal
        | Background => ()
        | Status =>
          let {id: parent_id, _} = parent_of_status(mark.parent_id);
          let sts =
            try(Hashtbl.find(statuses, parent_id)) {
            | Not_found =>
              let s = ref([]);
              Hashtbl.add(statuses, parent_id, s);
              s;
            };

          tuck(sts, mark);
        },
      marks,
    );
    Hashtbl.iter(
      (_id, {id, name, parent_id, parent_name, logs, kind}) => {
        bprintf(
          b,
          "%s (#%i, %s%s)\n",
          !!name,
          id,
          string_of_kind(kind),
          if (parent_id == 0) {
            "";
          } else {
            sprintf(", parent: %s#%i", !!parent_name, parent_id);
          },
        );
        let logs = LastN.to_list(logs);
        List.iter(
          line => {
            Buffer.add_string(b, " L ");
            Buffer.add_string(b, !!line);
          },
          logs,
        );
        switch (kind) {
        | Status => ()
        | Normal
        | Background =>
          let sts =
            switch (Hashtbl.find(statuses, id)) {
            | sts_acc => List.rev(sts_acc^)
            | exception Not_found => []
            };

          List.iter(
            status => bprintf(b, " S %s#%i\n", !!status.name, status.id),
            sts,
          );
        };
        Buffer.add_char(b, '\n');
      },
      marks,
    );
  } else {
    bprintf(b, "<not initialized>\n");
  };
  Buffer.contents(b);
};

/***/

let init = () => {
  enabled := true;
  let old_hook = Log.State.hook^;
  Log.State.hook :=
    (
      (level, facil, msg) => {
        log(msg);
        old_hook(level, facil, msg);
      }
    );
};
