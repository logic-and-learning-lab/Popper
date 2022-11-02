var_pos(Var,Vars,Pos):- var_pos_(Pos,Var,Vars).
var_pos_(0,V,Vars):- vars(1,Vars), Vars = (V,).
var_pos_(0,V,Vars):- vars(2,Vars), Vars = (V,_).
var_pos_(0,V,Vars):- vars(3,Vars), Vars = (V,_,_).
var_pos_(0,V,Vars):- vars(4,Vars), Vars = (V,_,_,_).
var_pos_(1,V,Vars):- vars(2,Vars), Vars = (_,V).
var_pos_(1,V,Vars):- vars(3,Vars), Vars = (_,V,_).
var_pos_(1,V,Vars):- vars(4,Vars), Vars = (_,V,_,_).
var_pos_(2,V,Vars):- vars(3,Vars), Vars = (_,_,V).
var_pos_(2,V,Vars):- vars(4,Vars), Vars = (_,_,V,_).
var_pos_(3,V,Vars):- vars(4,Vars), Vars = (_,_,_,V).

var_member(Var,Vars):- Pos = 0..3, var_pos(Var,Vars,Pos).

%% CLAUSE VAR
rule_var(C,Var):- head_var(C,Var).
rule_var(C,Var):- body_var(C,Var).

%% HEAD VAR
head_var(C,Var):- head_literal(C,_,_,Vars), var_member(Var,Vars).

%% BODY VAR
body_var(C,Var):- body_literal(C,_,_,Vars), var_member(Var,Vars).

num_in_args(P,N):- direction(P,_,_), #count{Pos : direction(P,Pos,in)} == N.

%% VAR SAFE IF HEAD INPUT VAR
safe_var(Rule,Var):- head_literal(Rule,P,_,Vars), var_pos(Var,Vars,Pos), direction(P,Pos,in).

%% VAR SAFE IF IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Rule,Var):- num_in_args(P,0), body_literal(Rule,P,_,Vars), var_member(Var,Vars).

%% VAR SAFE IF IN SAFE LITERAL
safe_var(C,Var):- safe_literal(C,P,Vars), var_member(Var,Vars).

%% LITERAL WITH N INPUT VARS IS SAFE IF N VARS ARE SAFE
safe_literal(Rule,P,Vars):- num_in_args(P,N), body_literal(Rule,P,_,Vars), #count{Pos : var_pos(Var,Vars,Pos), direction(P,Pos,in), safe_var(Rule,Var)} == N.

%% SAFE VARS
:- body_var(C,Var), not safe_var(C,Var).
:- body_literal(Rule,P,_,Vars), not safe_literal(Rule,P,Vars).


recursive:-
    head_literal(Rule,P,A,_),
    body_literal(Rule,P,A,_).

base:-
    head_literal(Rule,P,A,_),
    not body_literal(Rule,P,A,_).
:-
    recursive,
    not base.


%% :-
%%     recursive,
%%     head_var(Rule,Var),
%%     not body_var(Rule,Var).

:-
    not head_literal(0,_,_,_).




rule(R):-
    body_literal(R,_,_,_).

%% r1 is not a subset of r2 if there is a literal in r1 that is not in r2
not_body_subset(R1,R2):-
    %% R1 > 0,
    %% R2 > 0,
    rule(R2),
    R1 != R2,
    body_literal(R1,P,A,Vars),
    not body_literal(R2,P,A,Vars).

%% r1 is a subset of r2 if every literal
body_subset(R1,R2):-
    %% R1 > 0,
    %% R2 > 0,
    rule(R1),
    rule(R2),
    R1 != R2,
    not not_body_subset(R1,R2),
    body_literal(R1,P,A,Vars),
    body_literal(R2,P,A,Vars).

:-
    %% R1 > 0,
    %% R2 > 0,
    body_subset(R1,R2),
    body_subset(R2,R1).


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