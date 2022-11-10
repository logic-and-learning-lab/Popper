#show selected/1.
#heuristic size(N). [1000-N,true]

size(N):- #count{ID : selected(ID)} == N.

:- size(0).

%% every head literal must have a body literal
:- head_literal(Rule,_,_,_), not body_literal(Rule,_,_,_).

num_rules(N):- #count{Rule : body_literal(Rule,_,_,_)} == N.


%% head_connected(C,Var):- head_var(C,Var).
%% head_connected(C,Var1):- head_literal(C,_,A,_), Var1 >= A, head_connected(C,Var2), body_literal(C,_,_,Vars), var_member(Var1,Vars), var_member(Var2,Vars), Var1 != Var2.
%% :- head_literal(C,_,A,_), Var >= A, body_var(C,Var), not head_connected(C,Var).