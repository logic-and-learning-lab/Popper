#defined direction/3.
#defined vars/2.
#defined var_pos/3.
#defined num_recursive/1.

head_var(C,Var):- head_literal(C,_,_,Vars), var_member(Var,Vars).
body_var(C,Var):- body_literal(C,_,_,Vars), var_member(Var,Vars).
head_input_var(Rule,Var):- head_literal(Rule,P,_,Vars), var_pos(Var,Vars,Pos), direction(P,Pos,in).

%% MUST BE CONNECTED
head_connected(C,Var):- head_var(C,Var).
head_connected(C,Var1):- head_literal(C,_,A,_), Var1 >= A, head_connected(C,Var2), body_literal(C,_,_,Vars), var_member(Var1,Vars), var_member(Var2,Vars), Var1 != Var2.
:- head_literal(C,_,A,_), Var >= A, body_var(C,Var), not head_connected(C,Var).

%% ========== DIRECTIONS ==========
with_directions:- direction(_,_,_).
num_in_args(P,N):- direction(P,_,_), #count{Pos : direction(P,Pos,in)} == N.
safe_bvar(Rule,Var):- head_literal(Rule,P,_,Vars), var_pos(Var,Vars,Pos), direction(P,Pos,in).
safe_bvar(Rule,Var):- body_literal(Rule,P,_,Vars), num_in_args(P,0), var_member(Var,Vars).
safe_bvar(Rule,Var):- body_literal(Rule,P,_,Vars), var_member(Var,Vars), num_in_args(P,N), #count{Pos : var_pos(Var2,Vars,Pos), direction(P,Pos,in), safe_bvar(Rule,Var2)} == N.
:- with_directions, body_var(Rule,Var), not safe_bvar(Rule,Var).

%% ========== MULTIPLE RULES AND RECURSION ==========
rule(Rule):- body_literal(Rule,_,_,_).
recursive_rule(Rule):- head_pred(P,A), body_literal(Rule,P,A,_).
recursive:- recursive_rule(_).
base_rule:- rule(Rule), not recursive_rule(Rule).
multiple_rules:- #count{Rule : rule(Rule)} > 1.

%% cannot have recursion without a base case
:- recursive, not base_rule.

:- recursive, num_recursive(K), K > 1, #count{Rule : recursive_rule(Rule)} == M, M != K.

%% cannot have a recursive body literal without a head one
:- recursive_rule(Rule), head_pred(P,A), not head_literal(Rule,P,_,_).

%% a recursive rule must have at least two body literals
:- recursive_rule(Rule), #count{Q,Vars : body_literal(Rule,Q,_,Vars)} < 2.

%% head input arg in a recursive rule must appear in the body
:- with_directions, recursive_rule(Rule), head_input_var(Rule,Var), not body_var(Rule,Var).

%% if there are multiple rules, each rule must have a head literal
:- multiple_rules, body_literal(Rule,_,_,_), not head_literal(Rule,_,_,_).

%% cannot have multiple rules without recursion
:- multiple_rules, not recursive.
%% %% r1 is not a subset of r2 if there is a literal in r1 that is not in r2
not_body_subset(R1,R2):-  recursive_rule(R1), recursive_rule(R2), R1 != R2,  body_literal(R1,P,A,Vars), not body_literal(R2,P,A,Vars).
%% r1 is a subset of r2 if every literal
:- recursive_rule(R1), recursive_rule(R2), R1 != R2, not not_body_subset(R1,R2), body_literal(R1,P,A,Vars), body_literal(R2,P,A,Vars).
%% :- body_subset(R1,R2).

%% %% r1 is not a subset of r2 if there is a literal in r1 that is not in r2
%% not_body_subset(R1,R2):- rule(R2), R1 != R2, body_literal(R1,P,A,Vars), not body_literal(R2,P,A,Vars).
%% %% r1 is a subset of r2 if every literal
%% body_subset(R1,R2):- rule(R1), rule(R2), R1 != R2, not not_body_subset(R1,R2), body_literal(R1,P,A,Vars), body_literal(R2,P,A,Vars).
%% a:- body_subset(R1,R2), body_subset(R2,R1).
%% b:- rule(R1), rule(R2), not recursive_rule(R1), not recursive_rule(R2), body_subset(R1,R2).
%% c:- recursive_rule(R1), recursive_rule(R2), body_subset(R1,R2).

%% x :- a.
%% x :- b.
%% x :- c.

%% :- not x.






