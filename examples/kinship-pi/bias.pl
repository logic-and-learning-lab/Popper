%% grandparent(A,B) :- inv1(A,C),inv1(C,B).
%% inv1(A,B) :- mother(A,B).
%% inv1(A,B) :- father(A,B).
%% python3 popper.py examples/pi-kinship  0.93s user 0.05s system 100% cpu 0.972 total

max_clauses(4).
max_vars(3).
max_body(2).
enable_pi.

head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).