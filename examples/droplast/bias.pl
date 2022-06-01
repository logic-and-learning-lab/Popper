enable_recursion.

head_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(empty,1).
body_pred(even,1).
body_pred(odd,1).
body_pred(one,1).
body_pred(zero,1).
body_pred(my_append,3).

type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(empty,(list,)).
type(even,(element,)).
type(odd,(element,)).
type(one,(element,)).
type(zero,(element,)).
type(my_append,(element,list,list)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(empty,(in,)).
direction(even,(in,)).
direction(odd,(in,)).
direction(one,(in,)).
direction(zero,(out,)).
direction(my_append,(in,in,out)).