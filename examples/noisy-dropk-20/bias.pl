head_pred(f,3).
type(f,(list,element,list)).
direction(f,(in,in,out)).

max_vars(6).
max_body(6).
max_clauses(2).

enable_recursion.

body_pred(head,2).
body_pred(tail,2).
body_pred(geq,2).
body_pred(empty,1).
body_pred(even,1).
body_pred(odd,1).
body_pred(one,1).
body_pred(zero,1).
body_pred(decrement,2).

type(head,(list,element)).
direction(head,(in,out)).

type(tail,(list,list)).
direction(tail,(in,out)).

type(geq,(element,element)).
direction(geq,(in,in)).

type(empty,(list,)).
direction(empty,(in,)).

type(even,(element,)).
direction(even,(in,)).

type(odd,(element,)).
direction(odd,(in,)).

type(one,(element,)).
direction(one,(in,)).

type(zero,(element,)).
direction(zero,(out,)).

type(decrement,(element,element)).
direction(decrement,(in,out)).