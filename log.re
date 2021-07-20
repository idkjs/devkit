/**
  Global ready-to-use logger

  TODO interface to manage State
*/;

/**
{2 Example usage}

Create logging facility (messages origin)
{[let http = Log.facility "http"]}

Log from http subsystem at debug level
{[Log.debug http "received %u bytes"]}

Create and use object for http logging
{[let log = Log.from "http" (* new Log.logger http *);;
log#info "sent %u bytes" 1024
log#warn ~exn "failed here"
]}

Output only messages of warning level or higher for the http facility
{[http#allow `Warn]}
or
{[Logger.set_filter http `Warn]}
or
{[Log.set_filter ~name:"http" `Warn]}
or
{[Log.set_filter ~name:"http*" `Warn]} to set for all facilities starting with "http"

Output only messages of warning level or higher for all facilities
{[Log.set_filter `Warn]}

{2 API}
*/;

open Printf;
open ExtLib;
open Prelude;

/** Global logger state */
module State = {
  let all = Hashtbl.create(10);
  let default_level = ref(`Info: Logger.level);

  let utc_timezone = ref(false);

  let facility = name =>
    try(Hashtbl.find(all, name)) {
    | Not_found =>
      let x = {Logger.name, show: Logger.int_level(default_level^)};
      Hashtbl.add(all, name, x);
      x;
    };

  let set_filter = (~name=?, level) =>
    switch (name) {
    | None =>
      default_level := level;
      Hashtbl.iter((_, x) => Logger.set_filter(x, level), all);
    | Some(name) when String.ends_with(name, "*") =>
      let prefix = String.slice(~last=-1, name);
      Hashtbl.iter(
        (k, x) =>
          if (String.starts_with(k, prefix)) {
            Logger.set_filter(x, level);
          },
        all,
      );
    | Some(name) => Logger.set_filter(facility(name), level)
    };

  let set_loglevels = s =>
    Stre.nsplitc(s, ',')
    |> List.iter(spec =>
         switch (Stre.nsplitc(spec, '=')) {
         | [name, l] => set_filter(~name, Logger.level(l))
         | [l] => set_filter @@ Logger.level(l)
         | _ =>
           Exn.fail(
             "loglevel not recognized, specify either <level> or <facil>=<level> or <prefix>*=<level>",
           )
         }
       );

  let read_env_config = (~env="DEVKIT_LOG", ()) =>
    set_loglevels @@
    (
      try(Sys.getenv(env)) {
      | Not_found => ""
      }
    );

  let output_ch = (ch, str) =>
    try(
      {
        output_string(ch, str);
        flush(ch);
      }
    ) {
    | _ => ()
    }; /* logging never fails, most probably ENOSPC */

  let format_simple = (level, facil, msg) => {
    let pid = Unix.getpid();
    let tid = U.gettid();
    let pinfo =
      if (pid == tid) {
        sprintf("%5u:", pid);
      } else {
        sprintf("%5u:%u", pid, tid);
      };
    sprintf(
      "[%s] %s [%s:%s] %s\n",
      Time.to_string(~gmt=utc_timezone^, ~ms=true, Unix.gettimeofday()),
      pinfo,
      facil.Logger.name,
      Logger.string_level(level),
      msg,
    );
  };

  let log_ch = stderr;
  let () = assert(Unix.descr_of_out_channel(stderr) == Unix.stderr);
  let base_name = ref("");

  let hook = ref((_, _, _) => ());

  module Put =
    Logger.PutSimple({
      let format = format_simple;
      let output = (level, facil, s) => {
        let () = hook^(level, facil, s);
        output_ch(log_ch, s);
      };
    });

  module M = Logger.Make(Put);

  let self = "lib";

  /*
     we open the new fd, then dup it to stderr and close afterwards
     so we are always logging to stderr
   */
  let reopen_log_ch = (~self_call=false, file) =>
    try(
      {
        if (self_call == false) {
          base_name := file;
        };
        let ch = Files.open_out_append_text(file);
        Std.finally(
          () => close_out_noerr(ch),
          () => Unix.dup2(Unix.descr_of_out_channel(ch), Unix.stderr),
          (),
        );
      }
    ) {
    | e =>
      M.warn(
        facility(self),
        "reopen_log_ch(%s) failed : %s",
        file,
        Printexc.to_string(e),
      )
    };
};

include State.M;

let facility = State.facility;
let set_filter = State.set_filter;
let set_loglevels = State.set_loglevels;
let set_utc = () => State.utc_timezone := true;

/** Update facilities configuration from the environment.

    By default, it reads the configuration in the environment variable [DEVKIT_LOG]
    which can be overwritten using the optional [process_name] parameter.

    The value of environment variable should match the following grammar: [(\[<facil|prefix*>=\]debug|info|warn|error\[,\])*]

    @raise Failure on invalid level values of wrong format
*/

let read_env_config = State.read_env_config;

/**
  param [lines]: whether to split multiline message as separate log lines (default [true])

  param [backtrace]: whether to show backtrace (default is [true] if [exn] is given and backtrace recording is enabled)

  param [saved_backtrace]: supply backtrace to show instead of using [Printexc.get_backtrace]
*/

type pr('a) =
  (
    ~exn: exn=?,
    ~lines: bool=?,
    ~backtrace: bool=?,
    ~saved_backtrace: list(string)=?,
    format4('a, unit, string, unit)
  ) =>
  'a;

class logger (facil) = {
  let make_s = output_line => {
    let output =
      fun
      | true => (
          (facil, s) =>
            if (String.contains(s, '\n')) {
              List.iter(output_line(facil)) @@ String.nsplit(s, "\n");
            } else {
              output_line(facil, s);
            }
        )
      | false => output_line;

    let print_bt = (lines, exn, bt, s) => {
      output(
        lines,
        facil,
        s
        ++ " : exn "
        ++ Exn.str(exn)
        ++ (
          if (bt == []) {
            " (no backtrace)";
          } else {
            "";
          }
        ),
      );
      List.iter(line => output_line(facil, "    " ++ line), bt);
    };

    (~exn=?, ~lines=true, ~backtrace=?, ~saved_backtrace=?, s) =>
      try(
        switch (exn) {
        | None => output(lines, facil, s)
        | Some(exn) =>
          switch (saved_backtrace) {
          | Some(bt) => print_bt(lines, exn, bt, s)
          | None =>
            Option.default(Printexc.backtrace_status(), backtrace)
              ? print_bt(lines, exn, Exn.get_backtrace(), s)
              : output(lines, facil, s ++ " : exn " ++ Exn.str(exn))
          }
        }
      ) {
      | exn =>
        output_line(
          facil,
          sprintf("LOG FAILED : %S with message %S", Exn.str(exn), s),
        )
      };
  };
  let make = (output, ~exn=?, ~lines=?, ~backtrace=?, ~saved_backtrace=?, fmt) =>
    ksprintf(
      s => output(~exn?, ~lines?, ~backtrace?, ~saved_backtrace?, s),
      fmt,
    );
  let debug_s = make_s(debug_s);
  let warn_s = make_s(warn_s);
  let info_s = make_s(info_s);
  let error_s = make_s(error_s);
  let put_s = level => make_s(put_s(level));
  as _;
  pub debug_s = debug_s;
  pub warn_s = warn_s;
  pub info_s = info_s;
  pub error_s = error_s;
  pub put_s = put_s;
  /* expecting direct inlining to be faster but it is not o_O
     method debug : 'a. 'a pr =
       fun ?exn ?lines ?backtrace ?saved_backtrace fmt ->
       ksprintf (fun s -> debug_s ?exn ?lines ?backtrace ?saved_backtrace s) fmt
     */
  pub debug: 'a. pr('a) = make(debug_s);
  pub warn: 'a. pr('a) = make(warn_s);
  pub info: 'a. pr('a) = make(info_s);
  pub error: 'a. pr('a) = make(error_s);
  pub put: 'a. Logger.level => pr('a) = level => make(put_s(level));
  pub allow = (level: Logger.level) => Logger.set_filter(facil, level);
  pub level: Logger.level = Logger.get_level(facil);
  pub name = facil.Logger.name;
  pub facility: Logger.facil = facil;
};

let from = name => (new logger)(facility(name));

/** internal logging facility */

let self = from(State.self);

/** general logging facility */

let main = from("main");

/** reopen log file */

let reopen =
  fun
  | None => ()
  | Some(name) => State.reopen_log_ch(name);

let log_start = ref(Time.now());
let cur_size = ref(0);
