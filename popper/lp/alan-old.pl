%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR, CALLED ALAN
%% ##################################################

#defined direction/2.
#defined type/2.
#defined invented/2.
#defined lower/2.

#defined enable_pi/0.
#defined enable_recursion/0.
#defined non_datalog/0.
#defined custom_max_size/1.

#show body_literal/4.

max_size(K):-
    custom_max_size(K).
max_size(K):-
    not custom_max_size(_),
    max_body(M),
    max_clauses(N),
    K = (M+1)*N.
size(N):-
    max_size(MaxSize),
    N = 2..MaxSize,
    #sum{K+1,Rule : body_size(Rule,K)} == N.
:- not size(_).

pi_or_rec:-
    recursive.
pi_or_rec:-
    pi.
pi_or_rec_enabled:-
    enable_pi.
pi_or_rec_enabled:-
    enable_recursion.

%% prohibit multi-clause programs when no recursion or PI
:-
    clause(1),
    not pi_or_rec.

%% AC: @DC, this constraint might mess up the work on negation
:-
    enable_recursion,
    not pi,
    clause(Rule),
    Rule > 0,
    not recursive_clause(Rule,_,_).

%% head pred symbol if given by user or invented
head_aux(P,A):-
    head_pred(P,A).
head_aux(P,A):-
    invented(P,A).

%% body pred symbol if given by user or invented
body_aux(P,A):-
    body_pred(P,A).
body_aux(P,A):-
    invented(P,A).
body_aux(P,A):-
    head_aux(P,A),
    enable_recursion.

%% ********** BASE CASE (RULE 0) **********
head_literal(0,P,A,Vars):-
    head_pred(P,A),
    head_vars(A,Vars).

1 {body_literal(0,P,A,Vars): body_aux(P,A), vars(A,Vars), not head_pred(P,A), not type_mismatch(P,Vars), not bad_body(P,A,Vars)} M :-
    max_body(M).

%% {body_literal(0,P,A,Vars)}:-
%%     body_aux(P,A), vars(A,Vars), not head_pred(P,A), not type_mismatch(P,Vars), not bad_body(P,A,Vars).
%%     %% max_body(M).
%% ********** RECURSIVE CASE (RULE > 0) **********
%% THE SYMBOL INV_K CANNOT APPEAR IN THE HEAD OF CLAUSE C < K
0 {head_literal(Rule,P,A,Vars): head_vars(A,Vars), head_aux(P,A), index(P,I), Rule >= I} 1:-
    Rule = 1..N-1,
    max_clauses(N),
    pi_or_rec_enabled.

1 {body_literal(Rule,P,A,Vars): body_aux(P,A), vars(A,Vars), not bad_body(P,A,Vars), not type_mismatch(P,Vars)} M :-
    clause(Rule),
    Rule > 0,
    max_body(M),
    enable_recursion,
    not enable_pi.


%% 0 {body_literal(Rule,P,A,Vars): body_aux(P,A), vars(A,Vars), not bad_body(P,A,Vars), not type_mismatch(P,Vars)} M :-
%%     %% clause(Rule),
%%     %% Rule > 0,
%%     Rule = 1..N-1,
%%     max_clauses(N),
%%     max_body(M),
%%     enable_recursion,
%%     not enable_pi.

%% :-
%%     clause(Rule),
%%     not body_literal(Rule,_,_,_).

%% :-
%%     body_literal(Rule,_,_,_),
%%     not clause rule()

%% ********** INVENTED RULES **********
1 {body_literal(Rule,P,A,Vars): body_aux(P,A), vars(A,Vars), not bad_body(P,A,Vars)} M :-
    clause(Rule),
    Rule > 0,
    max_body(M),
    enable_pi.

bad_body(P,A,Vars):-
    head_pred(P,A),
    vars(A,Vars),
    not good_vars(A,Vars).
good_vars(A,Vars1):-
    head_vars(A,Vars2),
    var_member(V,Vars1),
    not var_member(V,Vars2).
bad_body(P,A,Vars2):-
    num_in_args(P,1),
    head_pred(P,A),
    head_vars(A,Vars1),
    vars(A,Vars2),
    var_pos(Var,Vars1,Pos1),
    var_pos(Var,Vars2,Pos2),
    direction_(P,Pos1,in),
    direction_(P,Pos2,in).

type_mismatch(P,Vars):-
    var_pos(Var,Vars,Pos),
    type(P,Types),
    pred_arg_type(P,Pos,T1),
    fixed_var_type(Var,T2),
    T1 != T2.

calls_invented(Rule):-
    invented(P,A),
    body_literal(Rule,P,A,_).
:-
    pi,
    not recursive,
    head_literal(Rule,P,A,_),
    head_pred(P,A),
    not calls_invented(Rule).


%% THERE IS A CLAUSE IF THERE IS A HEAD LITERAL
clause(C):-
    head_literal(C,_,_,_).

%% NUM BODY LITERALS OF A CLAUSE
%% TODO: IMPROVE AS EXPENSIVE
%% grounding is > c * (n choose k), where n = |Herbrand base| and k = MaxN
body_size(Rule,N):-
    clause(Rule),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(Rule,P,_,Vars)} == N.

%% USE CLAUSES IN ORDER
:-
    clause(C1),
    C1 > 1,
    C2 = 0..C1-1,
    not clause(C2).

%% USE VARS IN ORDER IN A CLAUSE
%% :-
%%     clause_var(C,Var1),
%%     Var1 > 1,
%%     Var2 = Var1-1,
%%     %% Var2 = 1..Var1-1,
%%     not clause_var(C,Var2).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(C,Var1),
    Var1 > 1,
    Var2 = 1..Var1-1,
    not clause_var(C,Var2).

%% POSSIBLE VAR
var(0..N-1):-
    max_vars(N).

%% CLAUSE VAR
clause_var(C,Var):-
    head_var(C,Var).
clause_var(C,Var):-
    body_var(C,Var).

%% HEAD VAR
head_var(C,Var):-
    head_literal(C,_,_,Vars),
    var_member(Var,Vars).
%% BODY VAR
body_var(C,Var):-
    body_literal(C,_,_,Vars),
    var_member(Var,Vars).

%% VAR IN A TUPLE OF VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% NEED TO KNOW LITERAL ARITIES
seen_arity(A):-
    head_pred(_,A).
seen_arity(A):-
    body_pred(_,A).
max_arity(K):-
    #max{A : seen_arity(A)} == K.

%% ##################################################
%% BIAS CONSTRAINTS
%% ##################################################
%% DATALOG
:-
    not non_datalog,
    head_var(Rule,Var),
    not body_var(Rule,Var).

%% constraints used by bk cons
valid_var(Rule,Var):-
    obeys_datalog_check(Rule,Var).

%% if non_datalog is true, all vars are valid
obeys_datalog_check(Rule, Var):-
    non_datalog,
    Rule=0..MaxRules-1,
    max_clauses(MaxRules),
    var(Var).

%% if non_datalog is false, a body only variable is valid
obeys_datalog_check(Rule,Var):-
    not non_datalog,
    clause_var(Rule,Var),
    not head_var(Rule,Var).

%% if non_datalog is false, a head var must also appear in the body
obeys_datalog_check(Rule,Var):-
    not non_datalog,
    head_var(Rule,Var),
    body_var(Rule,Var).

%% MUST BE CONNECTED
head_connected(C,Var):-
    head_var(C,Var).
head_connected(C,Var1):-
    head_literal(C,_,A,_),
    Var1 >= A,
    head_connected(C,Var2),
    body_literal(C,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.
:-
    head_literal(C,_,A,_),
    Var >= A,
    body_var(C,Var),
    not head_connected(C,Var).


%% %% %% ##################################################
%% %% %% ROLF MOREL'S ORDERING CONSTRAINT
%% %% %% IT REDUCES THE NUMBER OF MODELS BUT DRASTICALLY INCREASES GROUNDING AND SOLVING TIME
%% %% %% ##################################################
%% bfr(C,(P,PArgs),(Q,QArgs)) :- head_literal(C,P,_,PArgs),body_literal(C,Q,_,QArgs).
%% bfr(C,(P,PArgs),(Q,QArgs)) :- body_literal(C,P,_,PArgs),body_literal(C,Q,_,QArgs),P<Q.
%% bfr(C,(P,PArgs1),(P,PArgs2)) :- body_literal(C,P,PA,PArgs1),body_literal(C,P,PA,PArgs2),PArgs1<PArgs2.

%% not_var_first_lit(C,V,(P,PArgs)) :- bfr(C,(Q,QArgs),(P,PArgs)),var_member(V,QArgs),var_member(V,PArgs).
%% var_first_lit(C,V,(P,PArgs)) :- body_literal(C,P,_,PArgs),var_member(V,PArgs),not not_var_first_lit(C,V,(P,PArgs)).
%% :-var_first_lit(C,V,(P,PArgs)),var_first_lit(C,W,(Q,QArgs)),bfr(C,(P,PArgs),(Q,QArgs)),W<V.
%% %% :-pruned.


%% ##################################################
%% SUBSUMPTION
%% ##################################################
same_head(C1,C2):-
    C1 > 0,
    C1 < C2,
    head_literal(C1,P,A,Vars),
    head_literal(C2,P,A,Vars).

not_body_subset(C1,C2):-
    C1 > 0,
    clause(C2),
    C1 < C2,
    body_literal(C1,P,A,Vars),
    not body_literal(C2,P,A,Vars).

body_subset(C1,C2):-
    C1 > 0,
    clause(C2),
    C1 < C2,
    not not_body_subset(C1,C2),
    body_literal(C1,P,A,Vars),
    body_literal(C2,P,A,Vars).
:-
    C1 > 0,
    C1 < C2,
    same_head(C1,C2),
    body_subset(C1,C2).

%% ##################################################
%% SIMPLE TYPE MATCHING
%% ##################################################
#script (python)
def pytype(pos, types):
    return types.arguments[pos.number]
#end.

fixed_var_type(Var,@pytype(Pos,Types)):-
    head_literal(_,P,A,Vars),
    var_pos(Var,Vars,Pos),
    type(P,Types),
    head_vars(A,Vars).

pred_arg_type(P,Pos,@pytype(Pos,Types)):-
    Pos = 0..A-1,
    body_pred(P,A),
    type(P,Types).

var_type(C,Var,@pytype(Pos,Types)):-
    body_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    vars(A,Vars),
    type(P,Types).

var_type(C,Var,@pytype(Pos,Types)):-
    head_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    vars(A,Vars),
    type(P,Types).
:-
    clause_var(C,Var),
    #count{Type : var_type(C,Var,Type)} > 1.

%% ##################################################
%% CLAUSE ORDERING
%% ##################################################
%% AC: WHY DID I ADD THIS? ORDER SHOULD NOT MATTER
%% ORDER BY CLAUSE SIZE
%% p(A)<-q(A),r(A). (C1)
%% p(A)<-s(A). (C2)
bigger(C1, C2):-
    C1 > 0,
    max_clauses(K),
    K > 2,
    body_size(C1,N1),
    body_size(C2,N2),
    C1 < C2,
    N1 > N2.

%% TWO NON-RECURSIVE CLAUSES
:-
    C1 > 0,
    C1 < C2,
    recursive_clause(C1,_,_),
    recursive_clause(C2,_,_),
    same_head(C1,C2),
    bigger(C1,C2).

%% ########################################
%% RECURSION
%% ########################################
num_recursive(P,N):-
    head_literal(_,P,_,_),
    #count{C : recursive_clause(C,P,_)} == N.

recursive:-
    recursive_clause(_,_,_).

recursive_clause(C,P,A):-
    head_literal(C,P,A,_),
    body_literal(C,P,A,_).

base_clause(C,P,A):-
    head_literal(C,P,A,_),
    not body_literal(C,P,A,_).

%% A RECURSIVE CLAUSE MUST HAVE MORE THAN ONE BODY LITERAL
:-
    C > 0,
    recursive_clause(C,_,_),
    body_size(C,1).

%% STOP RECURSION BEFORE BASE CASES
:-
    C1 > 0,
    C1 < C2,
    recursive_clause(C1,_,_),
    base_clause(C2,_,_),
    same_head(C1,C2).

%% CANNOT HAVE RECURSION WITHOUT A BASECASE
:-
    recursive_clause(_,P,A),
    not base_clause(_,P,A).

%% DISALLOW TWO RECURSIVE CALLS
%% WHY DID WE ADD THIS??
:-
    C > 0,
    recursive_clause(C,P,A),
    #count{Vars : body_literal(C,P,A,Vars)} > 1.

%% PREVENT LEFT RECURSION
%% TODO: GENERALISE FOR ARITY > 3
:-
    C > 0,
    num_in_args(P,1),
    head_literal(C,P,A,Vars1),
    body_literal(C,P,A,Vars2),
    var_pos(Var,Vars1,Pos1),
    var_pos(Var,Vars2,Pos2),
    direction_(P,Pos1,in),
    direction_(P,Pos2,in).

%% TODO: Dangerous rules, can cause failure commenting out for now
:-
 Rule > 0,
 head_literal(Rule,P,_,(A,_)),
 body_literal(Rule,P,_,(_,A)).

:-
 Rule > 0,
 head_literal(Rule,P,_,(_,A)),
 body_literal(Rule,P,_,(A,_)).

%% PREVENT LEFT RECURSION FOR ARITY 3
:-
    C > 0,
    num_in_args(P,2),
    head_literal(C,P,A,Vars1),
    body_literal(C,P,A,Vars2),
    Var1 != Var2,
    var_pos(Var1,Vars1,V1Pos1),
    var_pos(Var1,Vars2,V1Pos2),
    direction_(P,V1Pos1,in),
    direction_(P,V1Pos2,in),
    var_pos(Var2,Vars1,V2Pos1),
    var_pos(Var2,Vars2,V2Pos2),
    direction_(P,V2Pos1,in),
    direction_(P,V2Pos2,in).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ENSURES INPUT VARS ARE GROUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#script (python)
def pydirection(pos, directions):
    return directions.arguments[pos.number]
def pylen(directions):
    return Number(len(directions.arguments))
#end.

arity(P,A):-
    head_aux(P,A).
arity(P,A):-
    body_aux(P,A).

direction_aux(P, @pylen(Directions), Directions):-
    direction(P,Directions).

direction_(P,Pos,@pydirection(Pos,Directions)):-
    arity(P,A),
    Pos=0..A-1,
    direction_aux(P,A,Directions).

num_in_args(P,N):-
    direction_(P,_,_),
    #count{Pos : direction_(P,Pos,in)} == N.

%% VAR SAFE IF HEAD INPUT VAR
safe_bvar(Rule,Var):-
    head_literal(Rule,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in).

%% VAR SAFE IF A OUTPUT VAR
safe_bvar(Rule,Var):-
    direction_(_,_,_),
    body_literal(Rule,P,_,Vars),
    num_in_args(P,0),
    var_member(Var,Vars).

safe_bvar(Rule,Var):-
    direction_(_,_,_),
    not deffo_safe(Rule, Var),
    body_literal(Rule, P, _, Vars),
    var_member(Var, Vars),
    num_in_args(P, K),
    K>0,
    safe_bvar(Rule,Var2) : var_pos(Var2,Vars,Pos), direction_(P,Pos,in).

deffo_safe(Rule, Var):-
    head_literal(Rule,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in),
    Rule = 0.
%% %% VAR SAFE IF ALL INPUT VARS ARE SAFE
%% safe_bvar(Rule,Var):-
%%     body_literal(Rule,P,_,Vars),
%%     var_member(Var,Vars),
%%     num_in_args(P,N),
%%     N > 0,
%%     #count{Pos : var_pos(Var2,Vars,Pos), direction_(P,Pos,in), safe_bvar(Rule,Var2)} == N.

:-
    direction_(_,_,_),
    body_var(Rule,Var),
    not safe_bvar(Rule,Var).

%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################
%% IDEAS FROM THE PAPER:
%% Predicate invention by learning from failures. A. Cropper and R. Morel

#script (python)
from itertools import permutations
from clingo.symbol import Tuple_, Number, Function
def mk_tuple(xs):
    return Tuple_([Number(x) for x in xs])
def py_gen_invsym(i):
    return Function(f'inv{i}')
#end.

pi:-
    invented(_,_).

%% P IS DEFNED BY AT LEAST TWO CLAUSES
num_clauses(P,N):-
    head_literal(_,P,_,_),
    #count{C : head_literal(C,P,_,_)} == N.

multiclause(P,A):-
    head_literal(_,P,A,_),
    not num_clauses(P,1).

%% INDEX FOR INVENTED SYMBOLS
index(P,0):-
    head_pred(P,_).
index(@py_gen_invsym(I),I):-
    enable_pi,
    I=1..N-1,
    max_clauses(N).

inv_symbol(P):-
    index(P,I),
    I>0.

{invented(P,A)}:-
    enable_pi,
    inv_symbol(P),
    max_arity(MaxA),
    A = 1..MaxA.

%% CANNOT INVENT A PREDICATE WITH MULTIPLE ARITIES
:-
    invented(P,_),
    #count{A : invented(P,A)} != 1.

inv_lower(P,Q):-
    enable_pi,
    inv_symbol(P),
    inv_symbol(Q),
    I < J,
    index(P,I),
    index(Q,J).
lower(P,Q):-
    head_pred(P,_),
    invented(Q,_).
lower(P,Q):-
    inv_lower(P,Q).
lower(P,Q):-
    lower(P,R),
    lower(R,Q).

%% NO RECURSIVE INVENTED CLAUSE UNLESS RECURSION IS ENABLED
:-
    not enable_recursion,
    invented(P,A),
    recursive_clause(_,P,A).

%% MUST LEARN PROGRAMS WITH ORDERED CLAUSES
:-
    C1 < C2,
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    lower(Q,P).

%% AN INVENTED SYMBOL MUST APPEAR IN THE HEAD OF A CLAUSE
:-
    invented(P,A),
    not head_literal(_,P,A,_).

%% AN INVENTED SYMBOL MUST APPEAR IN THE BODY OF A CLAUSE DEFINED BEFORE THE INVENTED ONE
appears_before(P,A):-
    invented(P,A),
    lower(Q,P),
    head_literal(C,Q,_,_),
    body_literal(C,P,_,_).

%% AN INVENTED SYMBOL MUST APPEAR IN THE BODY OF A CLAUSE
:-
    invented(P,A),
    not appears_before(P,A).

%% MUST INVENT IN ORDER
:-
    invented(P,_),
    inv_lower(Q,P),
    not invented(Q,_).

%% FORCE ORDERING
%% inv2(A):- inv1(A)
:-
    head_literal(C,P,_,_),
    body_literal(C,Q,_,_),
    lower(Q,P).

%% USE INVENTED SYMBOLS IN ORDER
%% f(A):- inv2(A)
%% inv2(A):- q(A)
%% TODO: ENFORCE ONLY ON ONE DIRECTLY BELOW
%% :-
    %% invented(P,_),
    %% head_literal(_,P,_,_),
    %% inv_lower(Q,P),
    %% not head_literal(_,Q,_,_).

%% PREVENT DUPLICATE INVENTED CLAUSES
%% f(A,B):-inv1(A,C),inv2(C,B).
:-
    C1 > 0,
    C2 > 0,
    C1 < C2,
    lower(P,Q),
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    invented(P,_),
    invented(Q,_),
    body_subset(C1,C2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INHERIT TYPE FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% X and Y should inherit the types of A and B respectively
var_type(C1,Var1,Type):-
    invented(P,A),
    C1 > C2,
    head_literal(C1,P,A,Vars1),
    body_literal(C2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(C2,Var2,Type).

%% INHERIT TYPE FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% A and B should inherit the types of X and Y respectively
var_type(C2,Var2,Type):-
    invented(P,A),
    C1 > C2,
    head_literal(C1,P,A,Vars1),
    body_literal(C2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(C1,Var1,Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INHERIT SAFETY FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% if A is safe then X is safe
%% safe_var(C1,Var1):-
%%     C1 > 0,
%%     invented(P,A),
%%     C1 != C2,
%%     head_literal(C1,P,A,Vars1),
%%     body_literal(C2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(C2,Var2).

%% INHERIT SAFETY FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% if Y is safe then B is safe
%% safe_var(C2,Var2):-
%%     C1 > 0,
%%     invented(P,A),
%%     C1 != C2,
%%     head_literal(C1,P,A,Vars1),
%%     body_literal(C2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(C1,Var1).

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


%% PRUNES SINGLE CLAUSE/LITERAL INVENTIONS
%% inv(A,B):-right(A,B).
:-
    invented(P,A),
    head_literal(C,P,A,_),
    body_size(C,1),
    not multiclause(P,A).

%% PREVENTS SINGLE CLAUSE/LITERAL CALLS
%% f(A,B):-inv(A,B)
:-
    head_literal(C,P,Pa,_),
    invented(Q,Qa),
    body_literal(C,Q,Qa,_),
    body_size(C,1),
    not multiclause(P,Pa).

only_once(P,A):-
    invented(P,A),
    head_literal(_,P,A,_),
    #count{C,Vars : body_literal(C,P,A,Vars)} == 1.

:-
    invented(P,_),
    #count{A : invented(P,A)} != 1.

:-
    invented(P,A),
    head_literal(C1,P,A,_),
    not multiclause(P,A),
    only_once(P,A),
    C2 < C1,
    body_literal(C2,P,A,_),
    body_size(C1,N1),
    body_size(C2,N2),
    max_body(MaxN),
    N1 + N2 - 1 <= MaxN.

%% %% ==========================================================================================
%% %% BK BIAS CONSTRAINTS
%% %% ==========================================================================================
%% IDEAS FROM THE PAPER:
%% Learning logic programs by discovering where not to search. A. Cropper and C. Hocquette. AAAI23.

:- prop(ab_ba,(P,P)), body_literal(Rule,P,_,(A,B)), A>B.
:- prop(abc_acb,(P,P)), body_literal(Rule,P,_,(A,B,C)), B>C.
:- prop(abc_bac,P), body_literal(Rule,P,_,(A,B,C)), A>B.
:- prop(abc_cba,P), body_literal(Rule,P,_,(A,B,C)), A>B.