max_vars(5).
max_body(5).
max_clauses(3).
enable_recursion.

head_pred(f,2).
body_pred(empty,1).
body_pred(odd,1).
body_pred(even,1).
body_pred(head,2).
body_pred(tail,2).
body_pred(prepend,3).

type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(empty,(list,)).
type(odd,(element,)).
type(even,(element,)).
type(prepend,(element,list,list)).

direction(f,(in,out)).
direction(empty,(out,)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(odd,(in,)).
direction(even,(in,)).
direction(prepend,(in,in,out)).

%% EXTRA BIAS TO REDUCE LEARNING TIME
only_once(head).
only_once(tail).
only_once(prepend).
:-
    only_once(P),
    clause(C),
    #count{Vars : body_literal(C,P,A,Vars)} > 1.
:-
    body_literal(0,_,2,_).
:-
    body_literal(0,_,3,_).
:-
    body_literal(1,empty,_,_).
:-
    body_literal(2,empty,_,_).