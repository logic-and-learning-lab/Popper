#defined direction/3.
#defined vars/2.
%% #show head_literal/4.
%% #show body_literal/4.
#show selected/1.
%% #show size/1.

#heuristic size(N). [1000-N,true]

size(N):- #count{ID : selected(ID)} == N.

%% need to select something
:- size(0).

%% every head literal must have a body literal
:- head_literal(Rule,_,_,_), #count{P,Vars : body_literal(Rule,P,_,Vars)} == 0.














%% %% POSSIBLE VARIABLE PERMUTATIONS FROM 1..MAX_ARITY
%% vars(A,@pyvars(A,MaxVars)):-
%%     max_vars(MaxVars),
%%     max_arity(K),
%%     A=1..K.

%% %% POSITION OF VAR IN TUPLE
%% var_pos(@pyvar_pos(Pos,Vars),Vars,Pos):-
%%     vars(A,Vars),
%%     Pos = 0..A-1.

%% %% MUST BE CONNECTED
%% head_connected(C,Var):-
%%     head_var(C,Var).
%% head_connected(C,Var1):-
%%     head_literal(C,_,A,_),
%%     Var1 >= A,
%%     head_connected(C,Var2),
%%     body_literal(C,_,_,Vars),
%%     var_member(Var1,Vars),
%%     var_member(Var2,Vars),
%%     Var1 != Var2.
%% :-
%%     head_literal(C,_,A,_),
%%     Var >= A,
%%     body_var(C,Var),
%%     not head_connected(C,Var).

%% %% HEAD VAR
%% head_var(C,Var):-
%%     head_literal(C,_,_,Vars),
%%     var_member(Var,Vars).
%% %% BODY VAR
%% body_var(C,Var):-
%%     body_literal(C,_,_,Vars),
%%     var_member(Var,Vars).

%% %% VAR IN A TUPLE OF VARS
%% var_member(Var,Vars):-
%%     var_pos(Var,Vars,_).