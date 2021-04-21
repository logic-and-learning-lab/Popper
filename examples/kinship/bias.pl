%% time py popper.py examples/kinship/
%% grandparent(A,B) :- mother(A,C),father(C,B).
%% grandparent(A,B) :- mother(C,B),father(A,C).
%% grandparent(A,B) :- mother(C,B),mother(A,C).
%% grandparent(A,B) :- father(A,C),father(C,B).
%% python3 popper.py examples/kinship/  1.03s user 0.08s system 100% cpu 1.104 total

max_clauses(4).
max_vars(4).
max_body(4).

head_pred(grandparent,2).
%% body_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).