%% f(A,B) :- inv1(A,C),inv2(C,B).
%% inv1(A,B) :- inv2(C,B),inv2(A,C).
%% inv2(A,B) :- right(C,B),right(A,C).
%% python3 popper.py examples/robots-pi  11.38s user 0.24s system 100% cpu 11.616 total

max_vars(3).
max_body(2).
max_clauses(3).
enable_pi.

head_pred(f,2).
body_pred(up,2).
body_pred(down,2).
body_pred(left,2).
body_pred(right,2).