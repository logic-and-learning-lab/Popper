max_vars(7).
max_body(6).
constant(agent_white, agent).
constant(agent_black, agent).
constant(int_0, int).
constant(int_5, int).
constant(int_10, int).
constant(int_15, int).
constant(int_20, int).
constant(int_25, int).
constant(int_30, int).
constant(int_35, int).
constant(int_40, int).
constant(int_45, int).
constant(int_50, int).
constant(int_55, int).
constant(int_60, int).
constant(int_65, int).
constant(int_70, int).
constant(int_75, int).
constant(int_80, int).
constant(int_85, int).
constant(int_90, int).
constant(int_95, int).
constant(int_100, int).
constant(action_finish, action).
constant(action_continue, action).
constant(action_noop, action).
constant(prop_gameOver, prop).
head_pred(legal,3).
body_pred(true_whitePayoff,2).
body_pred(true_blackPayoff,2).
body_pred(true_control,2).
body_pred(role,1).
body_pred(succ,2).
type(true_whitePayoff,(ex,int)).
type(true_blackPayoff,(ex,int)).
type(true_control,(ex,agent)).
type(legal,(ex,agent,action)).
type(role,(agent,)).
type(succ,(int,int)).

:-
	clause(C),
	#count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
