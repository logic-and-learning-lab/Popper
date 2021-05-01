%% python3 popper.py examples/alleven/
%% f(A) :- empty(A).
%% f(A) :- head(A,C),even(C),tail(A,B),f(B).
%% 0.62s user 0.03s system 99% cpu 0.648 total

max_clauses(2).
max_vars(5).
max_body(5).
enable_recursion.

head_pred(f,1).
body_pred(tail,2).
body_pred(head,2).
body_pred(last,2).
body_pred(empty,1).
body_pred(zero,1).
body_pred(even,1).

type(f,(list,)).
type(tail,(list,list)).
type(head,(list,element)).
type(last,(list,element)).
type(empty,(list,)).
type(zero,(element,)).
type(even,(element,)).

direction(f,(in,)).
direction(tail,(in,out)).
direction(head,(in,out)).
direction(last,(in,out)).
direction(empty,(in,)).
direction(zero,(in,)).
direction(even,(in,)).