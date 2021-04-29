%% f(A,B) :- right(A,F),right(F,C),right(C,D),right(D,E),right(E,B).
%% python3 popper.py examples/robots-linear  20.42s user 0.13s system 99% cpu 20.559 total

max_vars(6).
max_body(6).
max_clauses(3).

head_pred(f,2).
body_pred(up,2).
body_pred(down,2).
body_pred(left,2).
body_pred(right,2).

direction(f,(in,out)).
direction(up,(in,out)).
direction(down,(in,out)).
direction(left,(in,out)).
direction(right,(in,out)).