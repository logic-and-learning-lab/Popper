%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR
%% IT IS CALLED ALAN
%% THIS IS ALAN VERSION 14
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

%% #include "pi.pl".

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
    #count{P,Vars : body_literal(C,P,_,Vars)} == N.

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
from clingo.symbol import Tuple_, Number
def mk_tuple(xs):
    return Tuple_([Number(x) for x in xs])
def pyhead_vars(arity):
    return mk_tuple(range(arity.number))
def pyvars(arity, max_vars):
    for x in permutations(range(max_vars.number),arity.number):
        yield mk_tuple(x)
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
%% AC: @AC - I THINK WE CAN REDUCE GROUNDING MORE HERE
before(C1,C2):-
    %% C1 != C2,
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    lower(P,Q).

before(C1,C2):-
    %% C1 != C2,
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
%% AC: could refactor these for marginal gains
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

%% A RECURSIVE CLAUSE MUST HAVE MORE THAN ONE BODY LITERAL
:-
    recursive_clause(C,_,_),
    clause_size(C,1).

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
%% @RM - what is this??
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

%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################

#defined invented/2.
#defined lower/2.

pred(P,A):-
    head_pred(P,A).
pred(P,A):-
    body_pred(P,A).
lower(A,B):-
    lower(A,C),
    lower(C,B).
head_pred(P,A):-
    invented(P,A).
body_pred(P,A):-
    invented(P,A).

multiclause(P,A):-
    invented(_,_),
    head_literal(Clause1,P,A,_),
    head_literal(Clause2,P,A,_),
    Clause1 < Clause2.

%% MUST HAVE NON-INVENTED TARGET PREDICATE
:-
    #count{P,A : head_literal(_,P,A,_), not invented(P,A)} == 0.

%% IF AN INVENTED SYMBOL IS IN THE HEAD OF A CLAUSE IT MUST ALSO APPEAR IN THE BODY OF A CLAUSE
in_lower(P,A):-
    head_literal(Clause1,Q,_,_),
    body_literal(Clause1,P,A,_),
    lower(Q,P).
:-
    invented(P,A),
    head_literal(_,P,A,_),
    not in_lower(P,A).

%% IF AN INVENTED SYMBOL IS IN THE BODY OF A CLAUSE THEN IT MUST ALSO APPEAR IN THE HEAD OF A CLAUSE
:-
    invented(P,A),
    body_literal(_,P,A,_),
    not head_literal(_,P,A,_).

%% FIRST CLAUSE CANNOT BE INVENTED
:-
    head_literal(0,P,A,_),
    invented(P,A).

%% ORDER CLAUSES BY ORDERING
%% f(A):- inv1(A)
%% inv2(A):- q(A) (clause1)
%% inv1(A):- inv2(A) (clause2)
:-
    Clause1>0,
    Clause2>0,
    head_literal(Clause2,P1,_,_),
    head_literal(Clause1,P2,_,_),
    lower(P1,P2),
    Clause2 > Clause1.

%% FORCE ORDERING
%% inv2(A):- inv1(A)
:-
    Clause > 0,
    head_literal(Clause,Inv2,_,_),
    body_literal(Clause,Inv1,_,_),
    lower(Inv1,Inv2).

%% USE INVENTED SYMBOLS IN ORDER
%% f(A):- inv2(A)
%% inv2(A):- q(A)
%% TODO: ENFORCE ONLY ON ONE DIRECTLY BELOW
:-
    invented(Inv2,_),
    invented(Inv1,_),
    head_literal(_,Inv2,_,_),
    lower(Inv1,Inv2),
    not head_literal(_,Inv1,_,_).

%% PREVENT DUPLICATE INVENTED CLAUSES
%% f(A,B):-inv1(A,C),inv2(C,B).
%% inv1(A,B):-right(A,C),right(C,B).
%% inv2(A,B):-right(A,C),right(C,B).
%% TODO: GENERALISE FOR MULTIPLE CLAUSES

%% same_size(Clause1,Clause2):-
%%     clause_size(Clause1,N),
%%     clause_size(Clause2,N),
%%     Clause1 < Clause2.

:-
    Clause1 > 0,
    Clause2 > 0,
    %% TODO: DO THESE TWO HELP?
    %% clause_size(Clause1,N),
    %% clause_size(Clause2,N),
    %% same_size(Clause1,Clause2),
    Clause1 < Clause2,
    head_literal(Clause1,HeadPred1,A1,_),
    head_literal(Clause2,HeadPred2,A2,_),
    invented(HeadPred1,A1),
    invented(HeadPred2,A2),
    %% TODO: CHECK THIS !
    %% lower(HeadPred1,HeadPred2),
    HeadPred1 != HeadPred2,
    not multiclause(HeadPred1,A1),
    not multiclause(HeadPred2,A2),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% PREVENTS THIS:
%% p(A,B):-inv1(A,B).
%% inv1(A,B):-q(A,B).
%% TODO: DOUBLE CHECK!!
:-
    invented(P,A),
    clause_size(C,1),
    body_literal(C,P,A,_).

%% NO POINT INVENTING A SYMBOL IF IT ONLY HAS ONLY BODY LITERAL AND IS NOT A DISJUNCTION
%% f(A,B):-f1(A,C),f1(C,B).
%% f1(A,B):-right(A,B).
:-
    Clause > 0,
    invented(P,A),
    head_literal(Clause,P,A,_),
    clause_size(Clause,1),
    not multiclause(P,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INHERIT TYPE FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% X and Y should inherit the types of A and B respectively
var_type(Clause1,Var1,Type):-
    invented(P,A),
    Clause1 > 0,
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(Clause2,Var2,Type).

%% INHERIT TYPE FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% A and B should inherit the types of X and Y respectively
var_type(Clause2,Var2,Type):-
    invented(P,A),
    Clause1 > 0,
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(Clause1,Var1,Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INHERIT SAFETY FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% if A is safe then X is safe
%% safe_var(Clause1,Var1):-
%%     Clause1 > 0,
%%     invented(P,A),
%%     Clause1 != Clause2,
%%     head_literal(Clause1,P,A,Vars1),
%%     body_literal(Clause2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(Clause2,Var2).

%% INHERIT SAFETY FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% if Y is safe then B is safe
%% safe_var(Clause2,Var2):-
%%     Clause1 > 0,
%%     invented(P,A),
%%     Clause1 != Clause2,
%%     head_literal(Clause1,P,A,Vars1),
%%     body_literal(Clause2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(Clause1,Var1).

%% INHERIT DIRECTION FROM BODY LITERALS
%% TODO: IMPROVE HORRIBLE HACK
%% direction(P1,Pos1,in):-
%%     invented(P1,A1),
%%     head_literal(Clause,P1,A1,Vars1),
%%     var_pos(Var,Vars1,Pos1),
%%     body_literal(Clause,P2,_,Vars2),
%%     var_pos(Var,Vars2,Pos2),
%%     direction(P2,Pos2,in),
%%     #count{P3,Vars3: body_literal(Clause,P3,_,Vars3),var_pos(Var,Vars3,Pos3),direction(P3,Pos2,out)} == 0.