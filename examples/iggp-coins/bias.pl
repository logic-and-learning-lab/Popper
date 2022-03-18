%% 15:23:10 next_cell(A,B,C):-role(E),my_true_cell(A,B,C),does_jump(A,E,F,D),different(B,F),different(D,B).
%% 15:23:10 next_cell(A,B,C):-does_jump(A,E,F,D),different(D,F),does_jump(A,E,F,B),c_twocoins(C).
%% 15:23:10 next_cell(A,B,C):-does_jump(A,F,B,E),does_jump(A,F,D,E),c_zerocoins(C),different(E,D).


max_vars(7).
max_body(7).
max_clauses(1).

head_pred(next_cell,2).
body_pred(does_jump,3).
body_pred(my_succ,2).
body_pred(my_true_cell,2).
body_pred(role,1).
body_pred(my_pos,1).
body_pred(different,2).
body_pred(c_zerocoins,1).
body_pred(c_onecoin,1).
body_pred(c_twocoins,1).

type(next_cell,(pos,cell_value)).
type(does_jump,(agent,pos,pos)).
type(my_succ,(pos,pos)).
type(my_true_cell,(pos,cell_value)).
type(role,(agent,)).
type(my_pos,(pos,)).
type(different,(pos,pos)).
type(c_zerocoins,(cell_value,)).
type(c_onecoin,(cell_value,)).
type(c_twocoins,(cell_value,)).

prop(antitransitive,my_succ).
prop(antitriangular,my_succ).
prop(asymmetric_ab_ba,my_succ).
prop(singleton,c_onecoin).
prop(singleton,c_twocoins).
prop(singleton,c_zerocoins).
prop(singleton,role).
prop(symmetric_ab,different).
prop(unique_a_b,my_succ).
prop(unique_a_b,my_true_cell).
prop(unique_a_bc,does_jump).
prop(unique_ab_c,does_jump).
prop(unique_ac_b,does_jump).
prop(unique_b_a,my_succ).
prop(unique_b_ac,does_jump).
prop(unique_bc_a,does_jump).
prop(unique_c_ab,does_jump).
prop(countk,c_onecoin,1).
prop(countk,c_twocoins,1).
prop(countk,c_zerocoins,1).
prop(countk,role,1).
prop(unsat_pair,c_twocoins,c_onecoin).
prop(unsat_pair,c_zerocoins,c_onecoin).
prop(unsat_pair,c_zerocoins,c_twocoins).