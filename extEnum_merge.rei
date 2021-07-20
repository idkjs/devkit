let join_inner_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t(('c, 'd));
let join_inner_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) => Enum.t(('b, 'b));
let join_left_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t(('c, option('d)));
let join_left_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t(('b, option('b)));
let join_right_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t((option('c), 'd));
let join_right_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t((option('b), 'b));
let join_full_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t([> | `Both('c, 'd) | `Left('c) | `Right('d)]);
let join_full_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t([> | `Both('b, 'b) | `Left('b) | `Right('b)]);
let join_inner_multi_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t(('c, 'd));
let join_inner_multi_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) => Enum.t(('b, 'b));
let join_left_multi_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t(('c, option('d)));
let join_left_multi_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t(('b, option('b)));
let join_right_multi_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t((option('c), 'd));
let join_right_multi_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t((option('b), 'b));
let join_full_multi_by:
  (('a, 'b) => int, 'c => 'a, 'd => 'b, Enum.t('c), Enum.t('d)) =>
  Enum.t([> | `Both('c, 'd) | `Left('c) | `Right('d)]);
let join_full_multi_by_key:
  (('a, 'a) => int, 'b => 'a, Enum.t('b), Enum.t('b)) =>
  Enum.t([> | `Both('b, 'b) | `Left('b) | `Right('b)]);
let join_assoc_inner:
  (('a, 'b) => int, Enum.t(('a, 'c)), Enum.t(('b, 'd))) =>
  Enum.t(('a, 'c, 'd));
let join_assoc_left:
  (('a, 'b) => int, Enum.t(('a, 'c)), Enum.t(('b, 'd))) =>
  Enum.t(('a, 'c, option('d)));
let join_assoc_right:
  (('a, 'a) => int, Enum.t(('a, 'b)), Enum.t(('a, 'c))) =>
  Enum.t(('a, option('b), 'c));
let join_assoc_full:
  (('a, 'a) => int, Enum.t(('a, 'b)), Enum.t(('a, 'c))) =>
  Enum.t(('a, [> | `Both('b, 'c) | `Left('b) | `Right('c)]));
let join_assoc_inner_multi:
  (('a, 'b) => int, Enum.t(('a, 'c)), Enum.t(('b, 'd))) =>
  Enum.t(('a, 'c, 'd));
let join_assoc_left_multi:
  (('a, 'b) => int, Enum.t(('a, 'c)), Enum.t(('b, 'd))) =>
  Enum.t(('a, 'c, option('d)));
let join_assoc_right_multi:
  (('a, 'a) => int, Enum.t(('a, 'b)), Enum.t(('a, 'c))) =>
  Enum.t(('a, option('b), 'c));
let join_assoc_full_multi:
  (('a, 'a) => int, Enum.t(('a, 'b)), Enum.t(('a, 'c))) =>
  Enum.t(('a, [> | `Both('b, 'c) | `Left('b) | `Right('c)]));
let merge:
  (('a, 'b) => int, Enum.t('a), Enum.t('b)) =>
  Enum.t((option('a), option('b)));
let merge_assoc:
  (('a, 'a) => int, Enum.t(('a, 'b)), Enum.t(('a, 'c))) =>
  Enum.t(('a, option('b), option('c)));
