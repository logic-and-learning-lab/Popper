%% time py popper.py examples/robots/
%% f(A,B) :- right(A,E),right(E,F),right(F,C),right(C,D),right(D,B).
%% python3 popper.py examples/robots/  17.36s user 0.12s system 100% cpu 17.468 total

max_vars(3).
max_body(2).
max_clauses(3).
enable_pi.

head_pred(f,2).
body_pred(up,2).
body_pred(down,2).
body_pred(left,2).
body_pred(right,2).