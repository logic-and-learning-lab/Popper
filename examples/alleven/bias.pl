%% time py popper.py examples/alleven/
%% f(A) :- tail(A,B),head(B,D),even(D),last(A,C),even(C).
%% python3 popper.py examples/alleven/  2.57s user 0.06s system 99% cpu 2.635 total

max_vars(7).
max_body(7).
max_clauses(2).

head_pred(f,1).
body_pred(f,1).
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