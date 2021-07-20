/**
  Dealing with Lwt exceptions
*/;

open Printf;

let catch = (f, x) =>
  Lwt.try_bind(() => f(x), Lwt.return_some, _exn => Lwt.return_none);
let map = (f, x) =>
  Lwt.try_bind(
    () => f(x),
    r => Lwt.return(`Ok(r)),
    exn => Lwt.return(`Exn(exn)),
  );

let fail = (~exn=?, fmt) => {
  let fails = s =>
    Lwt.fail_with @@
    (
      switch (exn) {
      | None => s
      | Some(exn) => s ++ " : " ++ Exn.to_string(exn)
      }
    );
  ksprintf(fails, fmt);
};

let invalid_arg = fmt => ksprintf(Lwt.fail_invalid_arg, fmt);
