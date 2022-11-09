max_vars(7).
max_body(6).
max_clauses(1).
constant(agent_xplayer, agent).
constant(agent_oplayer, agent).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(int_0, int).
constant(int_50, int).
constant(int_100, int).
constant(action_noop, action).
constant(symbol_x, symbol).
constant(symbol_o, symbol).
head_pred(next_control,2).
body_pred(true_mark,6).
body_pred(true_currentboard,3).
body_pred(true_control,2).
%% body_pred(not_true_control,2).
body_pred(input,2).
body_pred(input_play,6).
body_pred(does,3).
body_pred(does_play,7).
body_pred(role,1).
body_pred(index,1).
type(true_mark,(ex,mypos,mypos,mypos,mypos,symbol)).
type(true_currentboard,(ex,mypos,mypos)).
type(true_control,(ex,agent)).
type(not_true_control,(ex,agent)).
type(next_control,(ex,agent)).
type(input,(agent,action)).
type(input_play,(agent,mypos,mypos,mypos,mypos,symbol)).
type(does,(ex,agent,action)).
type(does_play,(ex,agent,mypos,mypos,mypos,mypos,symbol)).
type(role,(agent,)).
type(index,(mypos,)).

:-
	clause(C),
	#count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).



%% moo(true_control,2).
%% moo(next_control,2).
%% moo(true_currentboard,3).
%% moo(does,3).
%% moo(true_mark,6).
%% moo(does_play,7).


%% %% bad_body(P,2,Vars):-
%% %%     vars(2,Vars),
%% %%     moo(P,2),
%% %%     Vars = (V0,_),
%% %%     V0 != 0.

%% bad_body(P,A,Vars):-
%%     vars(A,Vars),
%%     moo(P,A),
%%     var_pos(Var,Vars,0),
%%     Var != 0.

%% type(true_mark,(ex,mypos,mypos,mypos,mypos,symbol)).
%% type(true_currentboard,(ex,mypos,mypos)).
%% type(true_control,(ex,agent)).
%% type(next_control,(ex,agent)).
%% type(input,(agent,action)).
%% type(input_play,(agent,mypos,mypos,mypos,mypos,symbol)).
%% type(does,(ex,agent,action)).
%% type(does_play,(ex,agent,mypos,mypos,mypos,mypos,symbol)).


prop(asymmetric_abc_acb,true_currentboard).
prop(countk,action_noop,1).
prop(countk,agent_oplayer,1).
prop(countk,agent_xplayer,1).
prop(countk,index,3).
prop(countk,input,2).
prop(countk,int_0,1).
prop(countk,int_100,1).
prop(countk,int_50,1).
prop(countk,mypos_1,1).
prop(countk,mypos_2,1).
prop(countk,mypos_3,1).
prop(countk,role,2).
prop(countk,symbol_o,1).
prop(countk,symbol_x,1).
prop(singleton,action_noop).
prop(singleton,agent_oplayer).
prop(singleton,agent_xplayer).
prop(singleton,int_0).
prop(singleton,int_100).
prop(singleton,int_50).
prop(singleton,mypos_1).
prop(singleton,mypos_2).
prop(singleton,mypos_3).
prop(singleton,symbol_o).
prop(singleton,symbol_x).
prop(unique_a_b,input).
prop(unique_a_b,true_control).
prop(unique_a_bc,does).
prop(unique_a_bc,true_currentboard).
prop(unique_ab_c,does).
prop(unique_ab_c,true_currentboard).
prop(unique_ac_b,does).
prop(unique_ac_b,true_currentboard).
prop(unsat_pair,agent_xplayer,agent_oplayer).
prop(unsat_pair,int_100,int_0).
prop(unsat_pair,int_50,int_0).
prop(unsat_pair,int_50,int_100).
prop(unsat_pair,mypos_2,mypos_1).
prop(unsat_pair,mypos_3,mypos_1).
prop(unsat_pair,mypos_3,mypos_2).
prop(unsat_pair,symbol_x,symbol_o).


