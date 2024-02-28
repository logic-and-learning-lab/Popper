%% check whether every element in a list is even
%% f(V0):- empty(V0).
%% f(V0):- tail(V0,V2),head(V0,V1),even(V1),f(V2).

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
direction(empty,(out,)).
direction(zero,(out,)).
direction(even,(in,)).