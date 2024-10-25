constant(agent_red, agent).
constant(agent_black, agent).
constant(mark_blank, mark).
constant(mypos_a, mypos).
constant(mypos_b, mypos).
constant(mypos_c, mypos).
constant(mypos_d, mypos).
constant(mypos_e, mypos).
constant(int_0, int).
constant(int_1, int).
constant(int_2, int).
constant(int_3, int).
constant(int_4, int).
constant(int_5, int).
constant(int_6, int).
constant(int_7, int).
constant(int_8, int).
constant(int_9, int).
constant(int_10, int).
constant(int_11, int).
constant(int_12, int).
constant(int_13, int).
constant(int_14, int).
constant(int_15, int).
constant(int_16, int).
constant(int_17, int).
constant(int_18, int).
constant(int_19, int).
constant(int_20, int).
constant(int_100, int).
constant(action_noop, action).
head_pred(next_cell,3).
body_pred(mark,1).
body_pred(true_cell,3).
body_pred(true_control,2).
body_pred(true_step,2).
body_pred(input,2).
body_pred(input_move,3).
body_pred(does,3).
body_pred(does_move,4).
body_pred(role,1).
body_pred(adjacent,2).
body_pred(node,1).
body_pred(succ,2).
type(true_cell,(ex,mypos,mark)).
type(true_control,(ex,agent)).
type(true_step,(ex,int)).
type(next_cell,(ex,mypos,mark)).
type(input,(agent,action)).
type(input_move,(agent,mypos,mypos)).
type(does,(ex,agent,action)).
type(does_move,(ex,agent,mypos,mypos)).
type(role,(agent,)).
type(adjacent,(mypos,mypos)).
type(mark,(mark,)).
type(node,(mypos,)).
type(succ,(int,int)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
