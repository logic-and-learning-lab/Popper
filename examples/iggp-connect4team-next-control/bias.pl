constant(agent_red, agent).
constant(agent_blue, agent).
constant(agent_orange, agent).
constant(agent_cyan, agent).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(mypos_4, mypos).
constant(mypos_5, mypos).
constant(mypos_6, mypos).
constant(mypos_7, mypos).
constant(mypos_8, mypos).
constant(score_0, score).
constant(score_50, score).
constant(score_100, score).
constant(temp_hot, temp).
constant(temp_cold, temp).
constant(action_noop, action).
head_pred(next_control,2).
body_pred(true_cell,4).
body_pred(true_control,2).
body_pred(input,2).
body_pred(input_drop,2).
body_pred(does,3).
body_pred(does_drop,3).
body_pred(role,1).
body_pred(succ,2).
body_pred(team,2).
body_pred(x,1).
body_pred(y,1).
type(true_cell,(ex,mypos,mypos,agent)).
type(true_control,(ex,agent)).
type(next_control,(ex,agent)).
type(input,(agent,action)).
type(input_drop,(agent,mypos)).
type(does,(ex,agent,action)).
type(does_drop,(ex,agent,mypos)).
type(role,(agent,)).
type(succ,(mypos,mypos)).
type(team,(temp,agent)).
type(x,(mypos,)).
type(y,(mypos,)).

:- clause(C), #count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
