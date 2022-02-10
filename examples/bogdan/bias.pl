max_clauses(4).
max_vars(3).
max_body(2).
enable_pi.
enable_recursion.

head_pred(f,2).
body_pred(is_empty,1).
body_pred(not_empty,1).
body_pred(is_space,1).
body_pred(not_space,1).
body_pred(is_uppercase,1).
body_pred(not_uppercase,1).
body_pred(is_lowercase,1).
body_pred(not_lowercase,1).
body_pred(is_letter,1).
body_pred(not_letter,1).
body_pred(is_number,1).
body_pred(not_number,1).
body_pred(skip1,2).
%% body_pred(copy1,2).
body_pred(copyskip1,2).
body_pred(mk_uppercase,2).
body_pred(mk_lowercase,2).

direction(f,(in,out)).
%% direction(f1,(in,out)).
%% direction(f2,(in,out)).
direction(is_empty,(in,)).
direction(not_empty,(in,)).
direction(is_space,(in,)).
direction(not_space,(in,)).
direction(is_uppercase,(in,)).
direction(not_uppercase,(in,)).
direction(is_lowercase,(in,)).
direction(not_lowercase,(in,)).
direction(is_letter,(in,)).
direction(not_letter,(in,)).
direction(is_number,(in,)).
direction(not_number,(in,)).
direction(skip1,(in,out)).
direction(copy1,(in,out)).
direction(copyskip1,(in,out)).
direction(mk_uppercase,(in,out)).
direction(mk_lowercase,(in,out)).
direction(inv1,(in,out)).

%% do not invent predicates with only one argument
:-
    invented(_,1).

:-
    num_recursive(_,2).

%% P(A,B):-Q(A,C),R(C,B).
meta_clause(C):-
    head_literal(C,P,2,(0,1)),
    body_literal(C,Q,2,(0,2)),
    body_literal(C,R,2,(2,1)),
    %% meta_lower(P,Q),
    %% meta_lower(P,R),
    body_size(C,2).

%% P(A,B):-Q(A),R(A,B).
meta_clause(C):-
    head_literal(C,P,2,(0,1)),
    body_literal(C,Q,1,(0,)),
    body_literal(C,R,2,(0,1)),
    %% meta_lower(P,Q),
    %% meta_lower(P,R),
    body_size(C,2).

%% P(A,B):-Q(A,B),R(B).
meta_clause(C):-
    head_literal(C,P,2,(0,1)),
    body_literal(C,Q,2,(0,1)),
    body_literal(C,R,1,(1,)),
    %% meta_lower(P,Q),
    %% meta_lower(P,R),
    body_size(C,2).
:-
    clause(C),
    not meta_clause(C).

%% meta_lower(P,Q):-
%%     lower(P,Q).
%% meta_lower(P,Q):-
%%     head_aux(P,_),
%%     body_pred(Q,_),
%%     not head_aux(Q,_).