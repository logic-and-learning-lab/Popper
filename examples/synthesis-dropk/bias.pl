%% drops the first k elements from a list
%% f(V0,V1,V2):- tail(V0,V2),one(V1).
%% f(V0,V1,V2):- decrement(V1,V4),f(V0,V4,V3),tail(V3,V2).

enable_recursion.

head_pred(f,3).
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

type(f,(list,element,list)).
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

direction(f,(in,in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(element,(in,out)).
direction(increment,(in,out)).
direction(decrement,(in,out)).
direction(geq,(in,in)).
direction(empty,(out,)).
direction(even,(in,)).
direction(odd,(in,)).
direction(one,(out,)).
direction(zero,(out,)).