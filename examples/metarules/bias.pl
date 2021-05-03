%% python3 popper.py examples/metarules
%% f(A,B):- inv1(C,B),inv1(A,C).
%% inv1(A,B):- inv2(A,C),right(C,B).
%% inv2(A,B):- right(A,C),right(C,B).
%% 1.39s user 0.07s system 99% cpu 1.463 total

max_vars(3).
max_body(2).
max_clauses(5).
enable_pi.

head_pred(f,2).
body_pred(up,2).
body_pred(down,2).
body_pred(left,2).
body_pred(right,2).

%% P(A,B):-Q(A,C),R(C,B).
meta_clause(C):-
    head_literal(C,P,2,(0,1)),
    body_literal(C,Q,2,(0,2)),
    body_literal(C,R,2,(2,1)),
    meta_lower(P,Q),
    meta_lower(P,R),
    body_size(C,2).
:-
    clause(C),
    not meta_clause(C).

meta_lower(P,Q):-
    lower(P,Q).
meta_lower(P,Q):-
    head_aux(P,_),
    body_pred(Q,_),
    not head_aux(Q,_).