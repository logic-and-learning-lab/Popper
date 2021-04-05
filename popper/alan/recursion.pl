%% ########################################
%% RECURSION
%% ########################################
non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

num_recursive(N):-
    #count{C : recursive_clause(C,_,_)} == N.

num_recursive(P,N):-
    head_literal(_,P,_,_),
    #count{C : recursive_clause(C,P,_)} == N.

separable:-
    not non_separable.

recursive:-
    recursive_clause(_,_,_).

recursive_clause(Clause,P,A):-
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,_).

base_clause(Clause,P,A):-
    head_literal(Clause,P,A,_),
    not body_literal(Clause,P,A,_).

%% NO RECURSION IN THE FIRST CLAUSE
:-
    recursive_clause(0,_,_).

%% STOP RECURSION BEFORE BASE CASES
:-
    Clause1 > 0,
    recursive_clause(Clause1,P,A),
    base_clause(Clause2,P,A),
    Clause2 > Clause1.

%% CANNOT HAVE RECURSION WITHOUT A BASECASE
:-
    recursive_clause(_,P,A),
    not base_clause(_,P,A).

%% DISALLOW TWO RECURSIVE CALLS
%% WHY DID WE ADD THIS??
:-
    Clause > 0,
    recursive_clause(Clause,P,A),
    body_literal(Clause,P,A,Vars1),
    body_literal(Clause,P,A,Vars2),
    Vars1 < Vars2.

%% PREVENT LEFT RECURSION
%% TODO: GENERALISE FOR ARITY > 3
:-
    Clause > 0,
    num_in_args(P,1),
    head_literal(Clause,P,A,Vars1),
    body_literal(Clause,P,A,Vars2),
    var_pos(Var,Vars1,Pos1),
    var_pos(Var,Vars2,Pos2),
    direction(P,Pos1,in),
    direction(P,Pos2,in).

%% TODO: REFACTOR
:-
    Clause > 0,
    num_in_args(P,2),
    %% get the head input vars
    head_literal(Clause,P,A,HeadVars),
    var_pos(HeadVar1,HeadVars,Pos1),
    var_pos(HeadVar1,HeadVars,Pos2),
    HeadPos1 != HeadPos2,
    direction(P,HeadPos1,in),
    direction(P,HeadPos2,in),
    %% get the body input vars
    body_literal(Clause,P,A,BodyVars),
    var_pos(HeadVar1,BodyVars,BodyPos1),
    var_pos(HeadVar2,BodyVars,BodyPos2),
    BodyPos1 != BodyPos2,
    direction(P,BodyPos1,in),
    direction(P,BodyPos2,in).
