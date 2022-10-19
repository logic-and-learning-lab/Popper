head_pred(pt, 2).
body_pred(addr, 2).
body_pred(assgn, 2).
body_pred(store, 2).
body_pred(load, 2).

enable_recursion.
allow_singletons.
max_body(3).