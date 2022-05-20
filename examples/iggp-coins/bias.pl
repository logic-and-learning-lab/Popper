%% next_cell(A,B,C):-does_jump(A,E,B,D),my_pos(F),c_zerocoins(C),does_jump(A,E,F,D).
%% next_cell(A,B,C):-role(E),does_jump(A,E,D,B),c_twocoins(C),different(D,B).
%% next_cell(A,B,C):-role(D),different(B,E),my_true_cell(A,B,C),different(B,F),does_jump(A,D,E,F).

max_vars(6).
max_body(6).
max_clauses(1).

head_pred(next_cell,3).
body_pred(does_jump,4).
body_pred(my_succ,2).
body_pred(my_true_cell,3).
body_pred(role,1).
body_pred(my_pos,1).
body_pred(different,2).
body_pred(c_zerocoins,1).
body_pred(c_onecoin,1).
body_pred(c_twocoins,1).

type(next_cell,(ex,pos,cell_value)).
type(does_jump,(ex,agent,pos,pos)).
type(my_succ,(pos,pos)).
type(my_true_cell,(ex,pos,cell_value)).
type(role,(agent,)).
type(my_pos,(pos,)).
type(different,(pos,pos)).
type(c_zerocoins,(cell_value,)).
type(c_onecoin,(cell_value,)).
type(c_twocoins,(cell_value,)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.



prop(unique_b_a,my_succ).
prop(unique_abd_c,does_jump).
prop(unique_a_b,my_succ).
prop(countk,c_twocoins,1).
prop(antitriangular,my_succ).
prop(unique_ad_bc,does_jump).
prop(asymmetric_abcd_abdc,does_jump).
prop(unsat_pair,c_zerocoins,c_onecoin).
prop(unique_acd_b,does_jump).
prop(unique_a_bcd,does_jump).
prop(unsat_pair,c_zerocoins,c_twocoins).
prop(countk,role,1).
prop(unique_ac_bd,does_jump).
prop(unique_abc_d,does_jump).
prop(asymmetric_ab_ba,my_succ).
prop(countk,c_zerocoins,1).
prop(symmetric_ab,different).
prop(singleton,c_zerocoins).
prop(unique_ab_c,my_true_cell).
prop(countk,c_onecoin,1).
prop(singleton,c_onecoin).
prop(antitransitive,my_succ).
prop(unsat_pair,c_twocoins,c_onecoin).
prop(unique_ab_cd,does_jump).
prop(singleton,role).
prop(singleton,c_twocoins).