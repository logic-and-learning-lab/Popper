%% python3 popper.py examples/find-dupl --eval-timeout=0.01
%% f(A,B) :- head(A,B).
%% f(A,B) :- tail(A,D),tail(D,C),element(C,B),f(D,B).
%% 30.24s user 1.40s system 100% cpu 31.431 total

max_vars(5).
max_body(5).
max_clauses(2).

head_pred(f,2).
body_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(element,2).
body_pred(increment,2).
body_pred(decrement,2).
body_pred(geq,2).
body_pred(empty,1).
body_pred(even,1).
body_pred(odd,1).
body_pred(one,1).
body_pred(zero,1).

type(f,(list,element)).
type(head,(list,element)).
type(tail,(list,list)).
type(element,(list,element)).
type(increment,(element,element)).
type(decrement,(element,element)).
type(geq,(element,element)).
type(empty,(list,)).
type(even,(element,)).
type(odd,(element,)).
type(one,(element,)).
type(zero,(element,)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(element,(in,out)).
direction(increment,(in,out)).
direction(decrement,(in,out)).
direction(geq,(in,in)).
direction(empty,(in,)).
direction(even,(in,)).
direction(odd,(in,)).
direction(one,(in,)).
direction(zero,(out,)).