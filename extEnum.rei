/** Extensions to Enum */;

include  (module type of Enum) with type t('a) = Enum.t('a);

/** same as {!Enum.find}, but found element is peeked, not junked */

let find_peek: ('a => bool, t('a)) => 'a;

/** @return enum that indefinitely runs over given (non-empty) list */

let list_loop: list('a) => t('a);

/** @return enum over [DynArray] slice (default: whole array) */

let of_dynarray: (~start: int=?, ~n: int=?, DynArray.t('a)) => Enum.t('a);

/** [take n e] @return enum consuming first [n] elements of [e] */

let take: (int, t('a)) => t('a);

/** merge two enums of same type */

let align: (('a, 'a) => int, t('a), t('a)) => t('a);

/** @param multi repeat input value from the left enum multiple times to output as many pairs as there are matching equal consecutive values in the right enum */

let join:
  (
    ~left: bool=?,
    ~right: bool=?,
    ~multi: bool=?,
    ('a, 'b) => int,
    t('a),
    t('b)
  ) =>
  t((option('a), option('b)));
let join_assoc:
  (
    ~left: bool=?,
    ~right: bool=?,
    ~multi: bool=?,
    ('a, 'a) => int,
    t(('a, 'b)),
    t(('a, 'c))
  ) =>
  t(('a, option('b), option('c)));

include (module type of ExtEnum_merge);

/** merge two enums of different types */

let merge:
  (('a, 'b) => int, t('a), t('b)) => t((option('a), option('b)));

/** merge two enums over key-value pairs */

let merge_assoc:
  (('a, 'a) => int, t(('a, 'b)), t(('a, 'c))) =>
  t(('a, option('b), option('c)));

/** [group equal fold zero e]
  accumulates elements of [e] with [fold], first element is [fold]ed with [zero],
  at each subsequent step [equal] is checked, and new accumulator is started once it returns [false]
*/

let group: (('acc, 'a) => bool, ('acc, 'a) => 'acc, 'acc, t('a)) => t('acc);

/** [group_assoc equal fold zero e]
  accumulates (with [fold]) values in [e] with matching key as determined by comparison function [equal],
  first value is [fold]ed with [zero], e.g.:

  [List.of_enum @@ Enum.group_assoc (=) (+) 0 @@ List.enum \["a",1; "a",2; "b",3; "b",4; "a", 1; "a", 10\] = \["a", 3; "b", 7; "a", 11; \] ]
*/

let group_assoc:
  (('a, 'a) => bool, ('b, 'c) => 'b, 'b, t(('a, 'c))) => t(('a, 'b));

/** [uniq f e] replaces every consecuitive sequence of elements from [e] comparing equal
  by the given comparison function [f] with the first element from that sequence */

let uniq: (('a, 'a) => bool, t('a)) => t('a);

/** [count_unique f e] replaces every consecutive sequence of elements from [e] comparing equal
  by the given comparison function [f] with the first element from that sequence and the number of duplicates */

let count_unique: (('a, 'a) => bool, t('a)) => t(('a, int));

/** [sub e f] extracts a subenum (consecutive sequence of the elements from [e]) that map to the same value of [f] */

let sub: (~eq: ('b, 'b) => bool=?, t('a), 'a => 'b) => option(('b, t('a)));

/** [iter_while f e] calls [f] for each element of [e] until it returns [false] or [e] is exhausted */

let iter_while: ('a => bool, t('a)) => unit;
