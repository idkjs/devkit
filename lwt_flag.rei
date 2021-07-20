/** Simple wrapper over Lwt_condition, starts to wait again on condvar right after current [wait] was finished, to not lose signals.
    Usable when there is one thread that waits for "flag".
    "Multiple waiters" semantics is not defined here ( <-> Lwt_condition.broadcast), don't use it.
 */;

type t('a);
let create: unit => t('a);
let signal: (t('a), 'a) => unit;
let wait: t('a) => Lwt.t('a);
