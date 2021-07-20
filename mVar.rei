/** Variable shared between threads */;

type t('a);

/** Create */

let create: unit => t('a);

/** Set the variable (overwriting previous value if any) and return immediately */

let set: (t('a), 'a) => unit;

/** Unset the variable */

let clear: t('a) => unit;

/** Get value (block until it is available) */

let get: t('a) => 'a;

/** Get value (block until it is available) and unset */

let grab: t('a) => 'a;

/** Get value immediately without blocking
    @return None if value was not set */

let try_get: t('a) => option('a);

/** Grab value immediately without blocking
    @return None if value was not set */

let try_grab: t('a) => option('a);
