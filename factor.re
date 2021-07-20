/** */;

module Int = {
  type t = int;
  let compare = (x: int, y) => compare(x, y);
  let equal = (x: int, y) => x == y;
  external to_int: t => int = "%identity";
  external of_int: int => t = "%identity";
  let of_string = int_of_string;
  let to_string = string_of_int;
  let add = (+);
  let zero = 0;
  let mul = ( * );
  let neg = (~-);
  let min = (x: int, y) =>
    if (x < y) {
      x;
    } else {
      y;
    };
  let max = (x: int, y) =>
    if (x > y) {
      x;
    } else {
      y;
    };
};

module Float = {
  type t = float;
  let compare = (x: float, y) => compare(x, y);
  let equal = (x: float, y) => x == y;
};
