max_vars(3).
max_body(2).
enable_recursion.

max_clauses(10).

head_pred(f,1).
body_pred(head,2).
body_pred(tail,2).
body_pred(c0,1).
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).
%% body_pred(c6,1).
%% body_pred(c7,1).
%% body_pred(c8,1).
%% body_pred(c9,1).
%% body_pred(c10,1).

:-
    body_literal(_,tail,_,(1,0)).

direction(f,(in,)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(P,(in,)):-
    body_pred(P,1).


type(f,(list,)).
type(head,(list,element)).
type(tail,(list,list)).

type(c0,(element,)).
type(c1,(element,)).
type(c2,(element,)).
type(c3,(element,)).
type(c4,(element,)).
type(c5,(element,)).