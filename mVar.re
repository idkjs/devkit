open ExtThread;

type t('a) = {
  mutex: Mutex.t,
  cond: Condition.t,
  mutable v: option('a),
};

let create = () => {mutex: Mutex.create(), cond: Condition.create(), v: None};

let set = (t, x) =>
  locked(
    t.mutex,
    () => {
      t.v = Some(x);
      Condition.signal(t.cond);
    },
  );
let clear = t => locked(t.mutex, () => t.v = None);

let rec wait = t =>
  switch (t.v) {
  | None =>
    Condition.wait(t.cond, t.mutex);
    wait(t);
  | Some(x) => x
  };

let get = t => locked(t.mutex, () => wait(t));
let grab = t =>
  locked(
    t.mutex,
    () => {
      let x = wait(t);
      t.v = None;
      x;
    },
  );

let try_get = t => locked(t.mutex, () => t.v);
let try_grab = t =>
  locked(
    t.mutex,
    () => {
      let x = t.v;
      t.v = None;
      x;
    },
  );
