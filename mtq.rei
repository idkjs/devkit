/** Queue shared between multiple threads */;

type t('a);

/** Create queue */

let create: unit => t('a);

/** Put item into the queue and return immediately */

let put: (t('a), 'a) => unit;

/** Get item from the queue (will block while queue is empty) */

let get: t('a) => 'a;

/** Peek the item (leaving it in the queue) */

let peek: t('a) => 'a;

/** Drop item from the queue if present */

let junk: t('a) => unit;

/** Get item from the queue without blocking
    @return None immediately if queue is empty */

let try_get: t('a) => option('a);

/** Get the length of the queue */

let length: t('a) => int;

/** Remove all elements from the queue */

let clear: t('a) => unit;
