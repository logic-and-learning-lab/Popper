%% b1(A,B):-skip1(A,B),is_empty(B).
%% b1(A,B):-b1_1(A,C),b1(C,B).
%% b1_1(A,B):-skip1(A,C),copy1(C,B).

max_vars(3).
max_body(2).
max_clauses(3).
enable_recursion.
enable_pi.

:-
    invented(_,_),
    not recursive.

head_pred(f,2).
body_pred(skip1,2).
body_pred(copyskip1,2).
body_pred(skipcopy1,2).
%% body_pred(copy1,2).
body_pred(mk_uppercase,2).
body_pred(mk_lowercase,2).
body_pred(is_empty,1).
body_pred(not_is_empty,1).
body_pred(is_space,1).
body_pred(not_is_space,1).
body_pred(is_uppercase,1).
body_pred(not_is_uppercase,1).
body_pred(is_lowercase,1).
body_pred(not_is_lowercase,1).
body_pred(is_letter,1).
body_pred(not_is_letter,1).
body_pred(is_number,1).
body_pred(not_is_number,1).

direction(P,(in,out)):-
    head_pred(P,2).
direction(P,(in,out)):-
    body_aux(P,2).
direction(P,(in,)):-
    body_aux(P,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Enforce forward-chaining langauge bias %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct