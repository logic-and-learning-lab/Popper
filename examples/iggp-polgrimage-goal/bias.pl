%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

constant(agent_red, agent).
constant(agent_blue, agent).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(mypos_4, mypos).
constant(mypos_5, mypos).
constant(mypos_6, mypos).
constant(int_0, int).
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
constant(int_21, int).
constant(int_20, int).
constant(int_30, int).
constant(int_40, int).
constant(int_50, int).
constant(int_100, int).
constant(phase_list_build_terrain, phase_list).
constant(phase_list_place_pilgrim, phase_list).
constant(phase_list_pilgrimage, phase_list).
constant(action_noop, action).
head_pred(goal,3).
body_pred(phase_list,1).
body_pred(true_cell,4).
body_pred(true_builder,4).
body_pred(true_pilgrim,4).
body_pred(true_control,2).
body_pred(true_phase,3).
body_pred(true_moves,3).
body_pred(input,2).
body_pred(input_move,5).
body_pred(input_raise,3).
body_pred(role,1).
body_pred(adjacent,4).
body_pred(board_succ,2).
body_pred(height,1).
body_pred(height_end,1).
body_pred(height_score,2).
body_pred(index,1).
body_pred(succ,2).
body_pred(height_succ,2).
type(true_cell,(ex,mypos,mypos,mypos)).
type(true_builder,(ex,agent,mypos,mypos)).
type(true_pilgrim,(ex,agent,mypos,mypos)).
type(true_control,(ex,agent)).
type(true_phase,(ex,agent,phase_list)).
type(true_moves,(ex,agent,int)).
type(input,(agent,action)).
type(input_move,(agent,mypos,mypos,mypos,mypos)).
type(input_raise,(agent,mypos,mypos)).
type(goal,(ex,agent,int)).
type(role,(agent,)).
type(adjacent,(mypos,mypos,mypos,mypos)).
type(board_succ,(mypos,mypos)).
type(height,(mypos,)).
type(height_end,(mypos,)).
type(height_score,(int,int)).
type(index,(mypos,)).
type(phase_list,(phase_list,)).
type(succ,(int,int)).
type(height_succ,(int,int)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).