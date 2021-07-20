/** Various utilities for use with Lwt. */;

let with_count: (ref(int), Lwt.t('a)) => Lwt.t('a);

let timely: (Time.t, 'a => Lwt.t(unit), 'a) => Lwt.t(unit);

/** [timely_loop' ?immediate period f] run f every period seconds; run immediately if immediate is true. */

let timely_loop':
  (~immediate: bool=?, Time.t, unit => Lwt.t(unit)) => Lwt.t(unit);

/** [timely_loop' ?immediate ?wait period f] run f every period seconds; run immediately if immediate is true; stop when wait thread terminates. */

let timely_loop:
  (~immediate: bool=?, ~wait: Lwt.t(unit)=?, Time.t, unit => Lwt.t(unit)) =>
  Lwt.t(unit);

/** [ensure_order t1 t2] cancel t1 when t2 terminates. */

let ensure_order: (Lwt.t('a), Lwt.t('b)) => Lwt.t('b);

/** [suppress_exn name cleanup t] wait for t to terminate, suppress any exception, and call cleanup () afterwards. */

let suppress_exn: (string, unit => Lwt.t('a), Lwt.t(unit)) => Lwt.t('a);

let action: (string, 'a => Lwt.t('b), 'a) => Lwt.t('b);

let action_do: (string, unit => Lwt.t('a)) => Lwt.t('a);

/** same as [Lwt.async] but also cancels task on {!Daemon.ShouldExit} */

let async: (unit => Lwt.t(unit)) => unit;
