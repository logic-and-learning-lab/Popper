%% reverse a list
%% f(V0,V1):- empty(V1),empty(V0).
%% f(V0,V1):- head(V0,V4),tail(V0,V3),f(V3,V2),my_append(V2,V4,V1).

enable_recursion.

head_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(my_append,3).
body_pred(empty,1).

type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(my_append,(list,element,list)).
type(empty,(list,)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(my_append,(in,in,out)).
direction(empty,(out,)).