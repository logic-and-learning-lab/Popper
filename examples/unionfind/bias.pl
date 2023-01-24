head_pred(sameset, 2).
body_pred(eq, 2).
body_pred(union, 2).
body_pred(find, 2).

max_body(2).
max_vars(3).

enable_recursion.

% solution
% sameset(x, y) :- eq(x, y).
% sameset(x, z) :- sameset(x, y), union(y, z).
% sameset(x, z) :- sameset(x, y), find(y, z).
% sameset(x, y) :- sameset(y, x).