bfr((P,PArgs),(Q,QArgs)) :- head_literal(P,_,PArgs),body_literal(Q,_,QArgs).
bfr((P,PArgs),(Q,QArgs)) :- body_literal(P,_,PArgs),body_literal(Q,_,QArgs),P<Q.
bfr((P,PArgs1),(P,PArgs2)) :- body_literal(P,PA,PArgs1),body_literal(P,PA,PArgs2),PArgs1<PArgs2.

not_var_first_lit(V,(P,PArgs)) :- bfr((Q,QArgs),(P,PArgs)),var_member(V,QArgs),var_member(V,PArgs).
var_first_lit(V,(P,PArgs)) :- body_literal(P,_,PArgs),var_member(V,PArgs),not not_var_first_lit(V,(P,PArgs)).
pruned:-var_first_lit(V,(P,PArgs)),var_first_lit(W,(Q,QArgs)),bfr((P,PArgs),(Q,QArgs)),W<V.
:-pruned.


%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR
%% IT IS CALLED ALAN
%% VERSION 15
%% ##################################################

#defined functional/2.
#defined irreflexive/2.
#defined direction/2.
#defined type/2.
#defined size/1.
#defined invented/2.
#defined lower/2.

#defined enable_pi/0.
#defined enable_recursion/0.
#defined non_datalog/0.
#defined allow_singletons/0.

#show head_literal/3.
#show body_literal/3.

%% %% GUESS HEAD LITERALS
%% %% THE SYMBOL INV_K CANNOT APPEAR IN THE HEAD OF CLAUSE C < K
head_literal(P,A,Vars):-
    head_vars(A,Vars),
    head_pred(P,A).

%% GUESS BODY LITERALS
1 {body_literal(P,A,Vars): body_pred(P,A), vars(A,Vars)} N:-
    max_body(N).

%% %% NUM BODY LITERALS OF A CLAUSE
%% %% TODO: IMPROVE AS EXPENSIVE
%% %% grounding is > c * (n choose k), where n = |Herbrand base| and k = MaxN
%% body_size(N):-
%%     max_body(MaxN),
%%     N > 0,
%%     N <= MaxN,
%%     #count{P,Vars : body_literal(P,_,Vars)} == N.

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(Var1),
    Var1 > 1,
    Var2 = 0..Var1-1,
    not clause_var(Var2).

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

%% POSSIBLE VAR
var(0..N-1):-
    max_vars(N).

%% CLAUSE VAR
clause_var(Var):-
    head_var(Var).
clause_var(Var):-
    body_var(Var).

%% HEAD VAR
head_var(Var):-
    head_literal(_,_,Vars),
    var_member(Var,Vars).
%% BODY VAR
body_var(Var):-
    body_literal(_,_,Vars),
    var_member(Var,Vars).

%% VAR IN A TUPLE OF VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% VAR IN A LITERAL
var_in_literal(P,Vars,Var):-
    head_literal(P,_,Vars),
    var_member(Var,Vars).
var_in_literal(P,Vars,Var):-
    body_literal(P,_,Vars),
    var_member(Var,Vars).

%% HEAD VARS ARE ALWAYS 0,1,...,A-1
head_vars(A,@pyhead_vars(A)):-
    head_pred(_,A).
head_vars(A,@pyhead_vars(A)):-
    invented(_,A).

%% NEED TO KNOW LITERAL ARITIES
seen_arity(A):-
    head_pred(_,A).
seen_arity(A):-
    body_pred(_,A).
max_arity(K):-
    #max{A : seen_arity(A)} == K.

%% POSSIBLE VARIABLE PERMUTATIONS FROM 1..MAX_ARITY
vars(A,@pyvars(A,MaxVars)):-
    max_vars(MaxVars),
    max_arity(K),
    A=1..K.

%% POSITION OF VAR IN TUPLE
var_pos(@pyvar_pos(Pos,Vars),Vars,Pos):-
    vars(A,Vars),
    Pos = 0..A-1.

%% DATALOG
:-
    head_var(Var),
    not body_var(Var).

%% ELIMINATE SINGLETONS
:-
    clause_var(Var),
    #count{P,Vars : var_in_literal(P,Vars,Var)} == 1.

%% MUST BE CONNECTED
head_connected(Var):-
    head_var(Var).
head_connected(Var1):-
    head_literal(_,A,_),
    Var1 >= A,
    head_connected(Var2),
    body_literal(_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.
:-
    head_literal(_,A,_),
    Var >= A,
    body_var(Var),
    not head_connected(Var).

%% ##################################################
%% SIMPLE TYPE MATCHING
%% ##################################################
#script (python)
def pytype(pos, types):
    return types.arguments[pos.number]
#end.

var_type(Var,@pytype(Pos,Types)):-
    var_in_literal(P,Vars,Var),
    var_pos(Var,Vars,Pos),
    vars(A,Vars),
    type(P,Types).
:-
    clause_var(Var),
    #count{Type : var_type(Var,Type)} > 1.

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
    head_pred(P,A).
arity(P,A):-
    body_pred(P,A).

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
safe_var(Var):-
    head_literal(P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction_(P,Pos,in).

%% VAR SAFE IF IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Var):-
    num_in_args(P,0),
    body_literal(P,_,Vars),
    var_member(Var,Vars).

%% VAR SAFE IF IN SAFE LITERAL
safe_var(Var):-
    safe_literal(P,Vars),
    var_member(Var,Vars).

%% LITERAL WITH N INPUT VARS IS SAFE IF N VARS ARE SAFE
safe_literal(P,Vars):-
    num_in_args(P,N),
    N > 0,
    body_literal(P,_,Vars),
    #count{Pos :
        var_pos(Var,Vars,Pos),
        direction_(P,Pos,in),
        safe_var(Var)
    } == N.

%% SAFE VARS
:-
    direction_(_,_,_), % guard for when no direction_s are given
    clause_var(Var),
    not safe_var(Var).




%% ==========================================================================================
%% BK BIAS CONSTRAINTS
%% ==========================================================================================

#defined prop/2.
#defined prop/3.
%% #defined prop/4.

%% :-
%%     unique_pA(P),
%%     body_literal(R,P,_,_),
%%     #count{A : body_literal(R,P,_,(A,))} > 1.


%% :- prop(singleton,P), body_literal(P,1,_), #count{A : body_literal(P,A,(A,))} > 1.
:- prop(singleton,P), body_literal(P,_,_), #count{Vars : body_literal(P,A,Vars)} > 1.

:- prop(asymmetric_ab_ba,P), body_literal(P,_,(A,B)), body_literal(P,_,(B,A)).
:- prop(asymmetric_abc_acb,P), body_literal(P,_,(A,B,C)), body_literal(P,_,(A,C,B)).
:- prop(asymmetric_abc_bac,P), body_literal(P,_,(A,B,C)), body_literal(P,_,(B,A,C)).
:- prop(asymmetric_abc_bca,P), body_literal(P,_,(A,B,C)), body_literal(P,_,(B,C,A)).
:- prop(asymmetric_abc_cab,P), body_literal(P,_,(A,B,C)), body_literal(P,_,(C,A,B)).
:- prop(asymmetric_abc_cba,P), body_literal(P,_,(A,B,C)), body_literal(P,_,(C,B,A)).
:- prop(asymmetric_abcd_abdc,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(A,B,D,C)).
:- prop(asymmetric_abcd_acbd,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(A,C,B,D)).
:- prop(asymmetric_abcd_acdb,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(A,C,D,B)).
:- prop(asymmetric_abcd_adbc,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(A,D,B,C)).
:- prop(asymmetric_abcd_adcb,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(A,D,C,B)).
:- prop(asymmetric_abcd_bacd,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,A,C,D)).
:- prop(asymmetric_abcd_badc,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,A,D,C)).
:- prop(asymmetric_abcd_bcad,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,C,A,D)).
:- prop(asymmetric_abcd_bcda,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,C,D,A)).
:- prop(asymmetric_abcd_bdac,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,D,A,C)).
:- prop(asymmetric_abcd_bdca,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(B,D,C,A)).
:- prop(asymmetric_abcd_cabd,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,A,B,D)).
:- prop(asymmetric_abcd_cadb,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,A,D,B)).
:- prop(asymmetric_abcd_cbad,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,B,A,D)).
:- prop(asymmetric_abcd_cbda,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,B,D,A)).
:- prop(asymmetric_abcd_cdab,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,D,A,B)).
:- prop(asymmetric_abcd_cdba,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(C,D,B,A)).
:- prop(asymmetric_abcd_dabc,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,A,B,C)).
:- prop(asymmetric_abcd_dacb,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,A,C,B)).
:- prop(asymmetric_abcd_dbac,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,B,A,C)).
:- prop(asymmetric_abcd_dbca,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,B,C,A)).
:- prop(asymmetric_abcd_dcab,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,C,A,B)).
:- prop(asymmetric_abcd_dcba,P), body_literal(P,_,(A,B,C,D)), body_literal(P,_,(D,C,B,A)).

:- prop(unique_a_b,P), body_literal(P,_,(A,_)), #count{B : body_literal(P,_,(A,B))} > 1.
:- prop(unique_a_bc,P), body_literal(P,_,(A,_,_)), #count{B,C : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_a_bcd,P), body_literal(P,_,(A,_,_,_)), #count{B,C,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_ab_c,P), body_literal(P,_,(A,B,_)), #count{C : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_ab_cd,P), body_literal(P,_,(A,B,_,_)), #count{C,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_abc_d,P), body_literal(P,_,(A,B,C,_)), #count{D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_abd_c,P), body_literal(P,_,(A,B,_,D)), #count{C : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_ac_b,P), body_literal(P,_,(A,_,C)), #count{B : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_ac_bd,P), body_literal(P,_,(A,_,C,_)), #count{B,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_acd_b,P), body_literal(P,_,(A,_,C,D)), #count{B : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_ad_bc,P), body_literal(P,_,(A,_,_,D)), #count{B,C : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_b_a,P), body_literal(P,_,(_,B)), #count{A : body_literal(P,_,(A,B))} > 1.
:- prop(unique_b_ac,P), body_literal(P,_,(_,B,_)), #count{A,C : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_b_acd,P), body_literal(P,_,(_,B,_,_)), #count{A,C,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_bc_a,P), body_literal(P,_,(_,B,C)), #count{A : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_bc_ad,P), body_literal(P,_,(_,B,C,_)), #count{A,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_bcd_a,P), body_literal(P,_,(_,B,C,D)), #count{A : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_bd_ac,P), body_literal(P,_,(_,B,_,D)), #count{A,C : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_c_ab,P), body_literal(P,_,(_,_,C)), #count{A,B : body_literal(P,_,(A,B,C))} > 1.
:- prop(unique_c_abd,P), body_literal(P,_,(_,_,C,_)), #count{A,B,D : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_cd_ab,P), body_literal(P,_,(_,_,C,D)), #count{A,B : body_literal(P,_,(A,B,C,D))} > 1.
:- prop(unique_d_abc,P), body_literal(P,_,(_,_,_,D)), #count{A,B,C : body_literal(P,_,(A,B,C,D))} > 1.

:- prop(antitransitive,P), body_literal(P,_,(A,B)), body_literal(P,_,(B,C)), body_literal(P,_,(A,C)).

:- prop(antitriangular,P), body_literal(P,_,(A,B)), body_literal(P,_,(B,C)), body_literal(P,_,(C,A)).

:- prop(unsat_pair,P,Q), body_literal(P,_,Vars), body_literal(Q,_,Vars).

:- prop(precon,P,Q), body_literal(P,_,(A,)), body_literal(Q,_,(A,B)).
:- prop(postcon,P,Q), body_literal(P,_,(A,B)), body_literal(Q,_,(B,)).