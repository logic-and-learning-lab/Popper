%% drop last element of a list
%% f(V0,V1):- empty(V1),tail(V0,V1).
%% f(V0,V1):- head(V0,V3),tail(V0,V4),f(V4,V2),append_(V3,V2,V1).

enable_recursion.

head_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(empty,1).
body_pred(even,1).
body_pred(odd,1).
body_pred(one,1).
body_pred(zero,1).
body_pred(append_,3).

type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(empty,(list,)).
type(even,(element,)).
type(odd,(element,)).
type(one,(element,)).
type(zero,(element,)).
type(append_,(element,list,list)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(empty,(out,)).
direction(even,(in,)).
direction(odd,(in,)).
direction(one,(out,)).
direction(zero,(out,)).
direction(append_,(in,in,out)).