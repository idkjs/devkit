/*
  This is reduced copy of ExtLib.BitSet with removed safety checks and auto-resize code to get some more speed of it.
  It is about 15% faster than the original implementation.

  Update 2: converted to bigarray
 */

type t;

/** Create an empty bitset with an initial size (in number of bits). */

let create: int => t;

/** Copy a bitset : further modifications of first one will not affect the
 copy. */

let copy: t => t;

/** [set s n] sets the nth-bit in the bitset [s] to true. */

let set: (t, int) => unit;

/** [unset s n] sets the nth-bit in the bitset [s] to false. */

let unset: (t, int) => unit;

/** [put s v n] sets the nth-bit in the bitset [s] to [v]. */

let put: (t, bool, int) => unit;

/** [toggle s n] changes the nth-bit value in the bitset [s]. */

let toggle: (t, int) => unit;

/** [is_set s n] returns true if nth-bit in the bitset [s] is set,
 or false otherwise. */

let is_set: (t, int) => bool;
