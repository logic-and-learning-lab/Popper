head_pred(path, 2).
body_pred(edge, 2).

enable_recursion.

%path(x, y) :- edge(x, y).
%path(x, z) :- path(x, y), edge(y, z).