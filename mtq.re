/* ExtThread.locked, duplicated to break internal circular dependency in ExtThread */
let locked = (mutex, f) => {
  Mutex.lock(mutex);
  Std.finally(() => Mutex.unlock(mutex), f, ());
};

type t('a) = {
  mutex: Mutex.t,
  cond: Condition.t,
  q: Queue.t('a),
};

let create = () => {
  mutex: Mutex.create(),
  cond: Condition.create(),
  q: Queue.create(),
};

let put = (q, v) =>
  locked(
    q.mutex,
    () => {
      Queue.push(v, q.q);
      Condition.signal(q.cond);
    },
  );

let get = q =>
  locked(
    q.mutex,
    () => {
      while (Queue.is_empty(q.q)) {
        Condition.wait(q.cond, q.mutex);
      };
      Queue.pop(q.q);
    },
  );

let peek = q =>
  locked(
    q.mutex,
    () => {
      while (Queue.is_empty(q.q)) {
        Condition.wait(q.cond, q.mutex);
      };
      Queue.peek(q.q);
    },
  );

let junk = q =>
  locked(
    q.mutex,
    () => {
      let _ = Exn.catch(Queue.pop, q.q);
      ();
    },
  );

let try_get = q => locked(q.mutex, () => Exn.catch(Queue.pop, q.q));

let length = q => locked(q.mutex, () => Queue.length(q.q));

let clear = q => locked(q.mutex, () => Queue.clear(q.q));
