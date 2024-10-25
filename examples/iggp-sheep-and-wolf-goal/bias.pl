%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

constant(agent_wolf, agent).
constant(agent_sheep, agent).
constant(int_0, int).
constant(int_100, int).
constant(mypos_c1, mypos).
constant(mypos_c2, mypos).
constant(mypos_c3, mypos).
constant(mypos_c4, mypos).
constant(mypos_c5, mypos).
constant(mypos_c6, mypos).
constant(mypos_c7, mypos).
constant(mypos_c8, mypos).
constant(mark_b, mark).
constant(mark_s, mark).
constant(mark_w, mark).
constant(action_noop, action).
head_pred(goal,3).
body_pred(true_cell,4).
body_pred(true_control,2).
body_pred(role,1).
body_pred(adjacent,5).
body_pred(smaller,2).
body_pred(succ,2).
type(true_cell,(ex,mypos,mypos,mark)).
type(true_control,(ex,agent)).
type(goal,(ex,agent,int)).
type(role,(agent,)).
type(adjacent,(agent,mypos,mypos,mypos,mypos)).
type(smaller,(mypos,mypos)).
type(succ,(mypos,mypos)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
