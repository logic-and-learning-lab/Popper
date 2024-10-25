enable_recursion.

head_pred(f,3).
body_pred(decrement,2).
body_pred(empty,1).
body_pred(even,1).
body_pred(geq,2).
body_pred(head,2).
body_pred(odd,1).
body_pred(one,1).
body_pred(tail,2).
body_pred(zero,1).

direction(decrement,(in,out)).
direction(empty,(in,)).
direction(even,(in,)).
direction(f,(in,in,out)).
direction(geq,(in,in)).
direction(head,(in,out)).
direction(odd,(in,)).
direction(one,(in,)).
direction(tail,(in,out)).
direction(zero,(out,)).

type(decrement,(element,element)).
type(empty,(list,)).
type(even,(element,)).
type(f,(list,element,list)).
type(geq,(element,element)).
type(head,(list,element)).
type(odd,(element,)).
type(one,(element,)).
type(tail,(list,list)).
type(zero,(element,)).