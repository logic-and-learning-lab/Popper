#defined direction/3.
#defined vars/2.
#defined var_pos/3.


with_directions:-
    direction(_,_,_).

head_var(C,Var):- head_literal(C,_,_,Vars), var_member(Var,Vars).
body_var(C,Var):- body_literal(C,_,_,Vars), var_member(Var,Vars).

num_in_args(P,N):- direction(P,_,_), #count{Pos : direction(P,Pos,in)} == N.

head_input_var(Rule,Var):- head_literal(Rule,P,_,Vars), var_pos(Var,Vars,Pos), direction(P,Pos,in).

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

%% MUST BE CONNECTED
head_connected(C,Var):- head_var(C,Var).
head_connected(C,Var1):- head_literal(C,_,A,_), Var1 >= A, head_connected(C,Var2), body_literal(C,_,_,Vars), var_member(Var1,Vars), var_member(Var2,Vars), Var1 != Var2.
:- head_literal(C,_,A,_), Var >= A, body_var(C,Var), not head_connected(C,Var).

rule(R):- body_literal(R,_,_,_).
recursive_rule(Rule):- head_pred(P,A), body_literal(Rule,P,A,_).
recursive:- recursive_rule(_).
base_rule:- rule(Rule), not recursive_rule(Rule).

%% cannot have recursion without a base case
:- recursive, not base_rule.

%% r1 is not a subset of r2 if there is a literal in r1 that is not in r2
not_body_subset(R1,R2):- rule(R2), R1 != R2, body_literal(R1,P,A,Vars), not body_literal(R2,P,A,Vars).

%% r1 is a subset of r2 if every literal
body_subset(R1,R2):- rule(R1), rule(R2), R1 != R2, not not_body_subset(R1,R2), body_literal(R1,P,A,Vars), body_literal(R2,P,A,Vars).
:- body_subset(R1,R2), body_subset(R2,R1).

%% cannot have a recursive body literal without a head one
:- recursive_rule(Rule), head_pred(P,A), not head_literal(Rule,P,_,_).

%% cannot have multiple rules without recursion
:- rule(R), R > 0, not recursive.

%% a recursive rule must have at least two body literals
:- head_pred(P,A), body_literal(Rule,P,A,_), #count{Q,Vars : body_literal(Rule,Q,_,Vars)} < 2.

%% head input arg in a recursive rule must appear in the body
:- with_directions, recursive_rule(Rule), head_input_var(Rule,Var), not body_var(Rule,Var).





