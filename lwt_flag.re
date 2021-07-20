module C = Lwt_condition;

type t('a) = {
  cond: C.t('a),
  mutable waiter: Lwt.t('a),
};

let create = () => {
  let cond = C.create();
  {cond, waiter: C.wait(cond)};
};

let signal = ({cond, _}, x) => C.signal(cond, x);

let wait = fl => {
  let%lwt r = fl.waiter;
  fl.waiter = C.wait(fl.cond);
  Lwt.return(r);
};
