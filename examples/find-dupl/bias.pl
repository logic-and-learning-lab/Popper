%% find a duplicate element in a list
%% f(V0,V1):- tail(V0,V2),element(V2,V1),head(V0,V1).
%% f(V0,V1):- tail(V0,V2),f(V2,V1).

enable_recursion.
max_vars(4). % make the example a little quicker to run

head_pred(f,2).
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
direction(empty,(out,)).
direction(even,(in,)).
direction(odd,(in,)).
direction(one,(out,)).
direction(zero,(out,)).