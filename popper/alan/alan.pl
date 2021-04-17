%% ##################################################
%% ALAN13 IS NAMED AFTER AC'S DOG
%% ##################################################

#defined functional/2.
#defined irreflexive/2.
#defined direction/2.
#defined type/2.
#defined size/1.

#show head_literal/4.
#show body_literal/4.
#show before/2.
#show min_clause/2.
#show direction_/3.

#include "pi.pl".

%% GUESS A SINGLE HEAD LITERAL
0 {head_literal(C,P,A,Vars) : head_pred(P,A), head_vars(A,Vars)} 1:-
    C = 0..N-1,
    max_clauses(N).

%% GUESS AT LEAST 1 BUT AT MOST N BODY LITERALS PER CLAUSE
1 {body_literal(C,P,A,Vars) : body_pred(P,A), vars(A,Vars)} N:-
    clause(C),
    max_body(N).

%% THERE IS A CLAUSE IF THERE IS A HEAD LITERAL
clause(C):-
    head_literal(C,_,_,_).

%% TODO: IMPROVE AS VERY EXPENSIVE
clause_size(C,N):-
    clause(C),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(C,P,_,Vars)} = N.

num_clauses(P,N):-
    head_literal(_,P,_,_),
    #count{C : head_literal(C,P,_,_)} == N.

literal(C,P,Vars):-
    head_literal(C,P,_,Vars).
literal(C,P,Vars):-
    body_literal(C,P,_,Vars).

%% ENSURE A CLAUSE
:-
    not clause(0).

%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(C,P,_,Vars),
    body_literal(C,P,_,Vars).

%% USE CLAUSES IN ORDER
:-
    clause(C),
    C > 1,
    not clause(C-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(C,Var),
    Var > 1,
    not clause_var(C,Var-1).

%% ##################################################
%% VARS ABOUT VARS - META4LIFE
%% ##################################################
#script (python)
from itertools import permutations
def pyhead_vars(arity):
    return tuple(range(arity.number))
def pyvars(arity, max_vars):
    for x in permutations(range(max_vars.number),arity.number):
        yield x
def pyvar_pos(pos, vars):
    return vars.arguments[pos.number]
#end.

var(0..N-1):-
    max_vars(N).

clause_var(C,Var):-
    head_var(C,Var).
clause_var(C,Var):-
    body_var(C,Var).

head_var(C,Var):-
    head_literal(C,_P,_A,Vars),
    var_member(Var,Vars).
body_var(C,Var):-
    body_literal(C,_P,_A,Vars),
    var_member(Var,Vars).

var_member(Var,Vars):-
    var_pos(Var,Vars,_).

var_in_literal(C,P,Vars,Var):-
    literal(C,P,Vars),
    var_member(Var,Vars).

%% HEAD VARS ARE ALWAYS 0,1,...,A-1
head_vars(A,@pyhead_vars(A)):-
    head_pred(_,A).

need_arity(A):-
    head_pred(_,A).
need_arity(A):-
    body_pred(_,A).

%% POSSIBLE VARIABLE COMBINATIONS
vars(A,@pyvars(A,MaxVars)):-
    max_vars(MaxVars),
    need_arity(A).

var_pos(@pyvar_pos(Pos,Vars),Vars,Pos):-
    vars(A,Vars),
    Pos = 0..A-1.

mode(P,A):-
    head_pred(P,A).
mode(P,A):-
    body_pred(P,A).

%% ##################################################
%% REDUCE CONSTRAINT GROUNDING BY ORDERING CLAUSES
%% ##################################################
before(C1,C2):-
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    lower(P,Q).

before(C1,C2):-
    head_literal(C1,P,_,_),
    head_literal(C2,P,_,_),
    not recursive_clause(C1,P,A),
    recursive_clause(C2,P,A).

count_lower(P,N):-
    head_literal(_,P,_,_),
    #count{Q : lower(Q,P)} == N.

min_clause(C,N+1):-
    recursive_clause(C,P,A),
    count_lower(P,N).

min_clause(C,N):-
    head_literal(C,P,A,_),
    not recursive_clause(C,P,A),
    count_lower(P,N).

%% ##################################################
%% BIAS CONSTRAINTS
%% ##################################################

%% AC: MAKE A USER OPTION
%% DATALOG
:-
    head_var(C,Var),
    not body_var(C,Var).

%% AC: MAKE A USER OPTION
%% ELIMINATE SINGLETONS
:-
    clause_var(C,Var),
    #count{P,Vars : var_in_literal(C,P,Vars,Var)} == 1.

%% MUST BE CONNECTED
head_connected(C,Var):-
    head_var(C,Var).
head_connected(C,Var1):-
    Var1 > 0,
    head_connected(C,Var2),
    body_literal(C,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.
:-
    Var > 0,
    body_var(C,Var),
    not head_connected(C,Var).

%% IRREFLEXIVE
%% prevents: p(A):-q(A,B),q(B,A)
:-
    irreflexive(P,2),
    body_literal(C,P,2,Vars1),
    body_literal(C,P,2,Vars2),
    Vars1 = (Var1,Var2),
    Vars2 = (Var2,Var1),
    Vars1 < Vars2.

%% FUNCTIONAL
%% CAN REPLICATE RECALL
%% prevents: p(A):-q(A,B),q(A,C)
:-
    functional(P,2),
    body_literal(C,P,2,(V1,_)),
    #count{V2 : body_literal(C,P,2,(V1,V2))} > 1.

%% FUNCTIONAL FOR 3
%% AC TODO: GENERALISE AND REMOVE SYMMETRY
:-
    functional(P,3),
    direction_(P,0,in),
    direction_(P,1,in),
    direction_(P,2,out),
    literal(C,P,(Var1,Var2,Var3)),
    literal(C,P,(Var1,Var2,Var4)),
    Var3 != Var3.

%% ##################################################
%% SUBSUMPTION
%% ##################################################
:-
    C1 < C2,
    head_literal(C1,HeadPred,_,HeadVars),
    head_literal(C2,HeadPred,_,HeadVars),
    body_literal(C2,P,_,Vars): body_literal(C1,P,_,Vars).

%% ##################################################
%% SIMPLE TYPE MATCHING
%% ##################################################
#script (python)
def pytype(pos, types):
    return types.arguments[pos.number]
#end.

var_type(C,Var,@pytype(Pos,Types)):-
    var_in_literal(C,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    mode(P,A),
    type(P,Types).
:-
    clause_var(C,Var),
    #count{Type : var_type(C,Var,Type)} > 1.

%% ##################################################
%% CLAUSE ORDERING
%% ##################################################

%% ORDER BY CLAUSE SIZE
%% p(A)<-q(A),r(A). (CLAUSE1)
%% p(A)<-s(A). (CLAUSE2)
%% AC: WHY DID I ADD THIS?
:-
    C2 > C1,
    not recursive_clause(C1,P,A),
    not recursive_clause(C2,P,A),
    head_literal(C1,P,A,Vars),
    head_literal(C2,P,A,Vars),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

%% AC: WHY DID I REMOVE THIS?
:-
    C2 > C1,
    recursive_clause(C1,P,A),
    recursive_clause(C2,P,A),
    head_literal(C1,P,A,Vars),
    head_literal(C2,P,A,Vars),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
    direction_(P,Pos1,in),
    direction_(P,Pos2,in).

%% TODO: REFACTOR
:-
    Clause > 0,
    num_in_args(P,2),
    %% get the head input vars
    head_literal(Clause,P,A,HeadVars),
    var_pos(HeadVar1,HeadVars,Pos1),
    var_pos(HeadVar1,HeadVars,Pos2),
    HeadPos1 != HeadPos2,
    direction_(P,HeadPos1,in),
    direction_(P,HeadPos2,in),
    %% get the body input vars
    body_literal(Clause,P,A,BodyVars),
    var_pos(HeadVar1,BodyVars,BodyPos1),
    var_pos(HeadVar2,BodyVars,BodyPos2),
    BodyPos1 != BodyPos2,
    direction_(P,BodyPos1,in),
    direction_(P,BodyPos2,in).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ENSURES INPUT VARS ARE GROUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#script (python)
def pydirection(pos, directions):
    return directions.arguments[pos.number]
#end.

direction_(P,Pos,@pydirection(Pos,Directions)):-
    mode(P,A),
    Pos=0..A-1,
    direction(P,Directions).

num_in_args(P,N):-
    direction_(P,_,_),
    #count{Pos : direction_(P,Pos,in)} == N.

%% VAR SAFE IF HEAD INPUT VAR
safe_var(Clause,Var):-
    head_literal(Clause,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in).

%% %% VAR SAFE IF IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Clause,Var):-
    num_in_args(P,0),
    body_literal(Clause,P,_,Vars),
    var_member(Var,Vars).

%% VAR SAFE IF IN SAFE LITERAL
safe_var(Clause,Var):-
    safe_literal(Clause,P,Vars),
    var_member(Var,Vars).

%% LITERAL WITH N INPUT VARS IS SAFE IF N VARS ARE SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,N),
    N > 0,
    body_literal(Clause,P,_,Vars),
    #count{Pos :
        var_pos(Var,Vars,Pos),
        direction_(P,Pos,in),
        safe_var(Clause,Var)
    } == N.

%% SAFE VARS
:-
    direction_(_,_,_), % guard for when no direction_s are given
    clause_var(Clause,Var),
    not safe_var(Clause,Var).