%% f(A,B) :- inv1(A,C),inv2(C,B).
%% inv1(A,B) :- inv2(C,B),inv2(A,C).
%% inv2(A,B) :- right(C,B),right(A,C).
%% python3 popper.py examples/robots-pi  11.38s user 0.24s system 100% cpu 11.616 total

max_vars(5).
max_body(4).
max_clauses(3).
enable_pi.

head_pred(f,2).
%% body_pred(up,2).
%% body_pred(down,2).
%% body_pred(left,2).
body_pred(right,2).


:-
    not body_size(C,1),
    body_literal(C,_,_,(0,B)),
    B != 2.

:-
    not body_size(C,1),
    body_literal(C,_,_,(A,B)),
    A != 0,
    B != 1,
    B != A+1.

:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} != 2.

:- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct