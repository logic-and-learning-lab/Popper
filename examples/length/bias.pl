%% f(A,B) :- empty(A),zero(B).
%% f(A,B) :- tail(A,D),f(D,C),succ(C,B).
%% python3 popper.py examples/length  0.28s user 0.03s system 99% cpu 0.314 total

max_vars(5).
max_body(5).
max_clauses(2).
enable_recursion.

head_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(succ,2).
body_pred(empty,1).
body_pred(zero,1).
body_pred(one,1).

type(f,(list,integer)).
type(head,(list,element)).
type(tail,(list,list)).
type(succ,(integer,integer)).
type(empty,(list,)).
type(zero,(integer,)).
type(one,(integer,)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(succ,(in,out)).
direction(empty,(in,)).
direction(zero,(out,)).
direction(one,(out,)).