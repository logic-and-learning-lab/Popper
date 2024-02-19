%% ##################################################
%% THIS FILE CONTAINS THE ASP PROGRAM GENERATOR, CALLED ALAN
%% ##################################################

#defined direction/2.
#defined direction_/3.
#defined type/2.

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

%% ********** BASE CASE (RULE 0) **********
head_literal(0,P,A,Vars):-
    head_pred(P,A),
    head_vars(A,Vars).

1 {body_literal(0,P,A,Vars): body_pred(P,A), vars(A,Vars), not type_mismatch(P,Vars)} M :-
    max_body(M).

type_mismatch(P,Vars):-
    var_pos(Var,Vars,Pos),
    type(P,Types),
    pred_arg_type(P,Pos,T1),
    fixed_var_type(Var,T2),
    T1 != T2.

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
obeys_datalog_check(Rule,Var):-
    non_datalog,
    clause_var(Rule,Var).

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

fixed_var_type(Var, Type):-
    head_literal(_,P, A, Vars),
    var_pos(Var, Vars, Pos),
    type(P, Types),
    head_vars(A,Vars),
    type_pos(Types, Pos, Type).

pred_arg_type(P, Pos, Type):-
    Pos = 0..A-1,
    body_pred(P,A),
    type(P, Types),
    type_pos(Types, Pos, Type).

var_type(C, Var, Type):-
    body_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    vars(A,Vars),
    type(P,Types),
    type_pos(Types, Pos, Type).

var_type(C, Var, Type):-
    head_literal(C,P,_,Vars),
    var_pos(Var,Vars,Pos),
    vars(A,Vars),
    type(P,Types),
    type_pos(Types, Pos, Type).
:-
    clause_var(C,Var),
    #count{Type : var_type(C,Var,Type)} > 1.

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
    body_literal(Rule,P,_,Vars),
    num_in_args(P,0),
    var_member(Var,Vars).

%% VAR SAFE IF ALL INPUT VARS ARE SAFE
safe_bvar(Rule,Var):-
    body_literal(Rule,P,_,Vars),
    var_member(Var,Vars),
    num_in_args(P,N),
    N > 0,
    #count{Pos : var_pos(Var2,Vars,Pos), direction_(P,Pos,in), safe_bvar(Rule,Var2)} == N.

:-
    direction_(_,_,_),
    body_var(Rule,Var),
    not safe_bvar(Rule,Var).