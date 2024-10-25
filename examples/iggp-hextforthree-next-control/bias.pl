constant(agent_red, agent).
constant(agent_green, agent).
constant(agent_blue, agent).
constant(row_a, row).
constant(row_b, row).
constant(row_c, row).
constant(row_d, row).
constant(row_e, row).
constant(row_f, row).
constant(row_g, row).
constant(row_h, row).
constant(row_i, row).
constant(col_1, col).
constant(col_2, col).
constant(col_3, col).
constant(col_4, col).
constant(col_5, col).
constant(col_6, col).
constant(col_7, col).
constant(col_8, col).
constant(col_9, col).
constant(int_0, int).
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
constant(int_22, int).
constant(int_23, int).
constant(int_24, int).
constant(int_25, int).
constant(int_26, int).
constant(int_27, int).
constant(int_28, int).
constant(int_29, int).
constant(int_30, int).
constant(int_31, int).
constant(int_32, int).
constant(int_33, int).
constant(int_34, int).
constant(int_35, int).
constant(int_36, int).
constant(int_37, int).
constant(int_38, int).
constant(int_39, int).
constant(int_40, int).
constant(int_41, int).
constant(int_42, int).
constant(int_43, int).
constant(int_44, int).
constant(int_45, int).
constant(int_46, int).
constant(int_47, int).
constant(int_48, int).
constant(int_49, int).
constant(int_50, int).
constant(int_51, int).
constant(int_52, int).
constant(int_53, int).
constant(int_54, int).
constant(int_55, int).
constant(int_56, int).
constant(int_57, int).
constant(int_58, int).
constant(int_59, int).
constant(int_60, int).
constant(int_61, int).
constant(int_62, int).
constant(int_100, int).
constant(action_noop, action).
head_pred(next_control,2).
body_pred(row,1).
body_pred(col,1).
body_pred(true_cell,4).
body_pred(true_connected,4).
body_pred(true_owner,3).
body_pred(true_step,2).
body_pred(true_control,2).
body_pred(input,2).
body_pred(input_place,3).
body_pred(does,3).
body_pred(does_place,4).
body_pred(role,1).
body_pred(adjacent,4).
body_pred(imaginary,2).
body_pred(middle,2).
body_pred(redbeg,2).
body_pred(redend,2).
body_pred(nextcol,2).
body_pred(nextrow,2).
body_pred(succ,2).
type(true_cell,(ex,row,col,agent)).
type(true_connected,(ex,int,row,col)).
type(true_owner,(ex,int,agent)).
type(true_step,(ex,int)).
type(true_control,(ex,agent)).
type(next_control,(ex,agent)).
type(input,(agent,action)).
type(input_place,(agent,row,col)).
type(does,(ex,agent,action)).
type(does_place,(ex,agent,row,col)).
type(role,(agent,)).
type(adjacent,(row,col,row,col)).
type(col,(col,)).
type(imaginary,(row,col)).
type(middle,(row,col)).
type(redbeg,(row,col)).
type(redend,(row,col)).
type(nextcol,(col,col)).
type(nextrow,(row,row)).
type(row,(row,)).
type(succ,(int,int)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
