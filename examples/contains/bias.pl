max_vars(5).
max_body(5).
max_clauses(3).

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
%% body_pred(increment,2). % METAGOL CRASHES WHEN GIVEN
%% body_pred(element,2). % CANNOT USE IN EVERYTHING BECAUSE OF MEMBER PROBLEM

type(cons,(element,list,list)).
direction(cons,(in,in,out)).

type(head,(list,element)).
direction(head,(in,out)).

type(tail,(list,list)).
direction(tail,(in,out)).

type(empty,(list,)).
direction(empty,(in,)).

type(element,(list,element)).
direction(element,(in,out)).

type(increment,(element,element)).
direction(increment,(in,out)).

type(decrement,(element,element)).
direction(decrement,(in,out)).

type(geq,(element,element)).
direction(geq,(in,in)).

type(even,(element,)).
direction(even,(in,)).

type(odd,(element,)).
direction(odd,(in,)).

type(one,(element,)).
direction(one,(in,)).

type(zero,(element,)).
direction(zero,(out,)).

type(sum,(element,element,element)).
direction(sum,(in,in,out)).
head_pred(f,1).
type(f,(list,)).
direction(f,(in,)).

direction(c_0,(in,)).
direction(c_1,(in,)).
direction(c_2,(in,)).
direction(c_3,(in,)).
direction(c_4,(in,)).
direction(c_5,(in,)).
direction(c_6,(in,)).
direction(c_7,(in,)).
direction(c_8,(in,)).
direction(c_9,(in,)).
direction(c_10,(in,)).

body_pred(c_0,1).
body_pred(c_1,1).
body_pred(c_2,1).
body_pred(c_3,1).
body_pred(c_4,1).
body_pred(c_5,1).
body_pred(c_6,1).
body_pred(c_7,1).
body_pred(c_8,1).
body_pred(c_9,1).
body_pred(c_10,1).

type(c_0,(element,)).
type(c_1,(element,)).
type(c_2,(element,)).
type(c_3,(element,)).
type(c_4,(element,)).
type(c_5,(element,)).
type(c_6,(element,)).
type(c_7,(element,)).
type(c_8,(element,)).
type(c_9,(element,)).
type(c_10,(element,)).
