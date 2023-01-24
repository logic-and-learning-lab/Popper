head_pred(rsg, 2).
body_pred(up, 2).
body_pred(down, 2).
body_pred(flat, 2).

enable_recursion.

max_body(3).
max_vars(4).

%RSG(x,y) :- FLAT(x,y).
%RSG(x,y) :- UP(x, a), RSG(b, a), DOWN(b, y).