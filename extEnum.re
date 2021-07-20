/** Extensions to Enum */;

include Enum;

let rec find_peek = (f, e) =>
  switch (peek(e)) {
  | Some(x) when f(x) => x
  | None => raise(Not_found)
  | _ =>
    junk(e);
    find_peek(f, e);
  };

let list_loop = l => {
  assert(l != []);
  let r = ref(l);
  let rec next = () =>
    switch (r^) {
    | [x, ...xs] =>
      r := xs;
      x;
    | [] =>
      r := l;
      next();
    };

  from(next);
};

let of_dynarray = (~start=0, ~n=?, d) => {
  let last =
    switch (n) {
    | None => DynArray.length(d)
    | Some(n) => start + n
    };

  let rec make = start => {
    let idxref = ref(start);
    let next = () => {
      if (idxref^ >= last) {
        raise(Enum.No_more_elements);
      };
      let retval = DynArray.get(d, idxref^);
      incr(idxref);
      retval;
    }
    and count = () =>
      if (idxref^ >= last) {
        0;
      } else {
        last - idxref^;
      }
    and clone = () => make(idxref^);

    Enum.make(~next, ~count, ~clone);
  };

  make(start);
};

let take = (limit, e) => {
  let limit = ref(limit);
  from(() => {
    if (0 == limit^) {
      raise(Enum.No_more_elements);
    };
    let x = next(e);
    decr(limit);
    x;
  });
};

let align = (f, e1, e2) => {
  let next = () =>
    switch (peek(e1), peek(e2)) {
    | (None, None) => raise(No_more_elements)
    | (Some(x), None) =>
      junk(e1);
      x;
    | (None, Some(y)) =>
      junk(e2);
      y;
    | (Some(x), Some(y)) when f(x, y) < 0 =>
      junk(e1);
      x;
    | (Some(_), Some(y)) =>
      junk(e2);
      y;
    };

  from(next);
};

let join = (~left=false, ~right=false, ~multi=true, f, e1, e2) => {
  let found = ref(false);
  let rec next = () => {
    let found' = found^;
    found := false;
    switch (peek(e1), peek(e2)) {
    | (None, None) => raise(No_more_elements)
    | (Some(_), None) as res =>
      junk(e1);
      if (left && !found') {
        res;
      } else {
        next();
      };
    | (None, Some(_)) as res =>
      junk(e2);
      if (right) {
        res;
      } else {
        raise(No_more_elements);
      };
    | (Some(x), Some(y)) as res =>
      switch (f(x, y)) {
      | n when n < 0 =>
        junk(e1);
        if (left && !found') {
          (Some(x), None);
        } else {
          next();
        };
      | n when n > 0 =>
        junk(e2);
        if (right) {
          (None, Some(y));
        } else {
          next();
        };
      | _ =>
        if (!multi) {
          junk(e1);
        };
        junk(e2);
        found := multi;
        res;
      }
    };
  };

  from(next);
};

let join_assoc = (~left=false, ~right=false, ~multi=true, f, e1, e2) => {
  let found = ref(false);
  let rec next = () => {
    let found' = found^;
    found := false;
    switch (peek(e1), peek(e2)) {
    | (None, None) => raise(No_more_elements)
    | (Some((k, x)), None) =>
      junk(e1);
      if (left && !found') {
        (k, Some(x), None);
      } else {
        next();
      };
    | (None, Some((k, y))) =>
      junk(e2);
      if (right) {
        (k, None, Some(y));
      } else {
        raise(No_more_elements);
      };
    | (Some((kx, x)), Some((ky, y))) =>
      switch (f(kx, ky)) {
      | n when n < 0 =>
        junk(e1);
        if (left && !found') {
          (kx, Some(x), None);
        } else {
          next();
        };
      | n when n > 0 =>
        junk(e2);
        if (right) {
          (ky, None, Some(y));
        } else {
          next();
        };
      | _ =>
        if (!multi) {
          junk(e1);
        };
        junk(e2);
        found := multi;
        (kx, Some(x), Some(y));
      }
    };
  };

  from(next);
};

include ExtEnum_merge;

let group = (equal, fold, zero, e) => {
  let current = ref(None);
  let rec next = () =>
    switch (get(e), current^) {
    | (None, None) => raise(No_more_elements)
    | (None, Some(x)) =>
      current := None;
      x;
    | (Some(v), None) =>
      current := Some(fold(zero, v));
      next();
    | (Some(v), Some(x)) when equal(x, v) =>
      current := Some(fold(x, v));
      next();
    | (Some(v), Some(x)) =>
      current := Some(fold(zero, v));
      x;
    };

  from(next);
};

let group_assoc = (equal, fold, zero, e) => {
  let current = ref(None);
  let rec next = () =>
    switch (get(e), current^) {
    | (None, None) => raise(No_more_elements)
    | (None, Some(x)) =>
      current := None;
      x;
    | (Some((k, v)), None) =>
      current := Some((k, fold(zero, v)));
      next();
    | (Some((k, v)), Some((g, acc))) when equal(k, g) =>
      current := Some((g, fold(acc, v)));
      next();
    | (Some((k, v)), Some(cur)) =>
      current := Some((k, fold(zero, v)));
      cur;
    };

  from(next);
};

let uniq = (equal, e) => {
  let current = ref(None);
  let rec next = () =>
    switch (get(e), current^) {
    | (None, None) => raise(No_more_elements)
    | (None, Some(x)) =>
      current := None;
      x;
    | (Some(v), None) =>
      current := Some(v);
      next();
    | (Some(v), Some(x)) when equal(x, v) => next()
    | (Some(v), Some(x)) =>
      current := Some(v);
      x;
    };

  from(next);
};

let count_unique = (equal, e) => {
  let current = ref(None);
  let n = ref(0);
  let rec next = () =>
    switch (get(e), current^) {
    | (None, None) => raise(No_more_elements)
    | (None, Some(x)) =>
      current := None;
      (x, n^);
    | (Some(v), None) =>
      current := Some(v);
      n := 1;
      next();
    | (Some(v), Some(x)) when equal(x, v) =>
      incr(n);
      next();
    | (Some(v), Some(x)) =>
      let count = n^;
      current := Some(v);
      n := 1;
      (x, count);
    };

  from(next);
};

let sub = (~eq=(==), e, f) =>
  switch (peek(e)) {
  | None => None
  | Some(x) =>
    let current = f(x);
    let next = () =>
      switch (peek(e)) {
      | Some(x) when eq(f(x), current) =>
        junk(e);
        x;
      | None
      | Some(_) => raise(No_more_elements)
      };

    Some((current, from(next)));
  };

let rec iter_while = (f, e) =>
  switch (peek(e)) {
  | Some(x) when f(x) =>
    switch (peek(e)) {
    | Some(y) when x === y => junk(e) /* "support" recursive invocations */
    | _ => ()
    };
    iter_while(f, e);
  | _ => ()
  };
