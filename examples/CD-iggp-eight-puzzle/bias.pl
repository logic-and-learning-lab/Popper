max_clauses(1).
max_vars(6).
max_body(6).

head_pred(next_cell,3).
body_pred(my_true_cell,3).
%% body_pred(my_true_step,1).
body_pred(my_input_move,3).
body_pred(does_move,3).
%% body_pred(my_index,1).
body_pred(my_succ,2).
%% body_pred(scoremap,2).
%% body_pred(my_successor,2).
body_pred(different_cell_type,2).
body_pred(different_pos,2).

%% (<= (next (cell ?x ?y b))
%%   (does robot (move ?x ?y)))

%% (<= (next (cell ?u ?y ?z))
%%   (does robot (move ?x ?y))
%%   (true (cell ?u ?y b))
%%   (true (cell ?x ?y ?z))
%%   (distinct ?z b))

%% (<= (next (cell ?x ?v ?z))
%%   (does robot (move ?x ?y))
%%   (true (cell ?x ?v b))
%%   (true (cell ?x ?y ?z))
%%   (distinct ?z b))

%% (<= (next (cell ?u ?v ?z))
%%   (true (cell ?u ?v ?z))
%%   (does robot (move ?x ?y))
%%   (or (distinct ?x ?u) (distinct ?y ?v))
%%   (true (cell ?x1 ?y1 b))
%%   (or (distinct ?x1 ?u) (distinct ?y1 ?v)))

type(next_cell, (pos, pos, cell_type)).
type(my_true_cell, (pos, pos, cell_type)).
%% type(my_true_step, (time_step,)).
type(my_input_move, (agent, pos, pos)).
type(does_move, (agent, pos, pos)).
type(my_index, (pos,)).
type(my_succ, (pos, pos)).
%% type(scoremap, (time_step, score_int)).
%% type(my_successor, (time_step, time_step)).
type(different_cell_type, (cell_type, cell_type)).
type(different_pos, (pos, pos)).

body_pred(P,1):-
    constant(T,P).
type(P,(T,)):-
    constant(T,P).

%% different_cell_type

constant(agent, robot).
%% constant(pos, 1).
%% constant(pos, 2).
%% constant(pos, 3).
%% constant(cell_type, ct_4).
%% constant(cell_type, ct_5).
%% constant(cell_type, ct_6).
%% constant(cell_type, ct_7).
%% constant(cell_type, ct_8).
constant(cell_type, ct_b).
%% constant(time_step, 0).
%% constant(time_step, 9).
%% constant(time_step, 10).
%% constant(time_step, 11).
%% constant(time_step, 12).
%% constant(time_step, 13).
%% constant(time_step, 14).
%% constant(time_step, 15).
%% constant(time_step, 16).
%% constant(time_step, 17).
%% constant(time_step, 18).
%% constant(time_step, 19).
%% constant(time_step, 20).
%% constant(time_step, 21).
%% constant(time_step, 22).
%% constant(time_step, 23).
%% constant(time_step, 24).
%% constant(time_step, 25).
%% constant(time_step, 26).
%% constant(time_step, 27).
%% constant(time_step, 28).
%% constant(time_step, 29).
%% constant(time_step, 30).
%% constant(time_step, 31).
%% constant(time_step, 32).
%% constant(time_step, 33).
%% constant(time_step, 34).
%% constant(time_step, 35).
%% constant(time_step, 36).
%% constant(time_step, 37).
%% constant(time_step, 38).
%% constant(time_step, 39).
%% constant(time_step, 40).
%% constant(time_step, 41).
%% constant(time_step, 42).
%% constant(time_step, 43).
%% constant(time_step, 44).
%% constant(time_step, 45).
%% constant(time_step, 46).
%% constant(time_step, 47).
%% constant(time_step, 48).
%% constant(time_step, 49).
%% constant(time_step, 50).
%% constant(score_int, 60).
%% constant(score_int, 62).
%% constant(score_int, 64).
%% constant(score_int, 66).
%% constant(score_int, 68).
%% constant(score_int, 70).
%% constant(score_int, 72).
%% constant(score_int, 74).
%% constant(score_int, 76).
%% constant(score_int, 78).
%% constant(score_int, 80).
%% constant(score_int, 82).
%% constant(score_int, 84).
%% constant(score_int, 86).
%% constant(score_int, 88).
%% constant(score_int, 90).
%% constant(score_int, 92).
%% constant(score_int, 94).
%% constant(score_int, 96).
%% constant(score_int, 98).
%% constant(score_int, 100).


prop(antitransitive,my_succ).
prop(antitriangular,different_cell_type).
prop(antitriangular,different_pos).
prop(antitriangular,my_succ).
prop(asymmetric_ab_ba,different_cell_type).
prop(asymmetric_ab_ba,different_pos).
prop(asymmetric_ab_ba,my_succ).
prop(asymmetric_abc_bac,my_true_cell).
prop(pre_postcon,(ct_b,different_cell_type,ct_b)).
prop(precon,(ct_b,different_cell_type)).
prop(singleton,ct_b).
prop(singleton,robot).
prop(symmetric_ab,different_cell_type).
prop(symmetric_ab,different_pos).
prop(symmetric_ab,my_succ).
prop(symmetric_ab,my_successor).
prop(unique_a_b,my_succ).
prop(unique_a_bc,does_move).
prop(unique_ab_c,does_move).
prop(unique_ab_c,my_true_cell).
prop(unique_ac_b,does_move).
prop(unique_ac_b,my_true_cell).
prop(unique_b_a,my_succ).
prop(unique_b_ac,does_move).
prop(unique_bc_a,does_move).
prop(unique_bc_a,my_input_move).
prop(unique_bc_a,my_true_cell).
prop(unique_c_ab,does_move).
prop(unique_c_ab,my_true_cell).
prop(countk,ct_b,1).
prop(countk,different_pos,3).
prop(countk,my_succ,2).
prop(countk,robot,1).