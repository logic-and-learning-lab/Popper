head_pred(legal_move,4).
body_pred(true_cell,4).
body_pred(true_step,2).
body_pred(input_move,3).
body_pred(role,1).
body_pred(index,1).
body_pred(succ,2).
body_pred(scoremap,2).
body_pred(successor,2).
body_pred(tile,1).

constant(agent_robot, agent).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(cell_type_4, cell_type).
constant(cell_type_5, cell_type).
constant(cell_type_6, cell_type).
constant(cell_type_7, cell_type).
constant(cell_type_8, cell_type).
constant(cell_type_b, cell_type).
constant(time_step_0, time_step).
constant(time_step_9, time_step).
constant(time_step_10, time_step).
constant(time_step_11, time_step).
constant(time_step_12, time_step).
constant(time_step_13, time_step).
constant(time_step_14, time_step).
constant(time_step_15, time_step).
constant(time_step_16, time_step).
constant(time_step_17, time_step).
constant(time_step_18, time_step).
constant(time_step_19, time_step).
constant(time_step_20, time_step).
constant(time_step_21, time_step).
constant(time_step_22, time_step).
constant(time_step_23, time_step).
constant(time_step_24, time_step).
constant(time_step_25, time_step).
constant(time_step_26, time_step).
constant(time_step_27, time_step).
constant(time_step_28, time_step).
constant(time_step_29, time_step).
constant(time_step_30, time_step).
constant(time_step_31, time_step).
constant(time_step_32, time_step).
constant(time_step_33, time_step).
constant(time_step_34, time_step).
constant(time_step_35, time_step).
constant(time_step_36, time_step).
constant(time_step_37, time_step).
constant(time_step_38, time_step).
constant(time_step_39, time_step).
constant(time_step_40, time_step).
constant(time_step_41, time_step).
constant(time_step_42, time_step).
constant(time_step_43, time_step).
constant(time_step_44, time_step).
constant(time_step_45, time_step).
constant(time_step_46, time_step).
constant(time_step_47, time_step).
constant(time_step_48, time_step).
constant(time_step_49, time_step).
constant(time_step_50, time_step).
constant(score_int_60, score_int).
constant(score_int_62, score_int).
constant(score_int_64, score_int).
constant(score_int_66, score_int).
constant(score_int_68, score_int).
constant(score_int_70, score_int).
constant(score_int_72, score_int).
constant(score_int_74, score_int).
constant(score_int_76, score_int).
constant(score_int_78, score_int).
constant(score_int_80, score_int).
constant(score_int_82, score_int).
constant(score_int_84, score_int).
constant(score_int_86, score_int).
constant(score_int_88, score_int).
constant(score_int_90, score_int).
constant(score_int_92, score_int).
constant(score_int_94, score_int).
constant(score_int_96, score_int).
constant(score_int_98, score_int).
constant(score_int_100, score_int).

type(true_cell,(ex,mypos,mypos,cell_type)).
type(true_step,(ex,time_step)).
type(legal_move,(ex,agent,mypos,mypos)).
type(input_move,(agent,mypos,mypos)).
type(role,(agent,)).
type(index,(mypos,)).
type(succ,(mypos,mypos)).
type(scoremap,(time_step,score_int)).
type(successor,(time_step,time_step)).
type(tile,(cell_type,)).

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).

:-
	clause(C),
	#count{V : clause_var(C,V),var_type(C,V,ex)} != 1.