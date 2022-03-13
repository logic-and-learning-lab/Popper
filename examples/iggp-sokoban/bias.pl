max_clauses(1).
max_vars(6).
max_body(6).

head_pred(next_at,3).
body_pred(bounds,1).
body_pred(controls,2).
body_pred(dir,1).
body_pred(does,2).
body_pred(is_box,1).
body_pred(is_down,1).
body_pred(is_left,1).
body_pred(is_noop,1).
body_pred(is_right,1).
body_pred(is_up,1).
body_pred(my_input,2).
body_pred(my_succ,2).
body_pred(my_true_at,3).
body_pred(my_true_target,2).

type(next_at,(int, int, obj)).
type(bounds,(int,)).
type(controls,(agent, obj)).
type(dir,(action,)).
type(does,(agent, action)).
type(is_box,(action,)).
type(is_down,(action,)).
type(is_left,(action,)).
type(is_noop,(action,)).
type(is_right,(action,)).
type(is_up,(action,)).
type(my_input,(agent, action)).
type(my_succ,(int, int,)).
type(my_true_at,(int, int, obj)).
type(my_true_target,(int, int)).

body_pred(P,1):-
    constant(_,P).
type(P,(T,)):-
    constant(T, P).

constant(agent, black).
%% constant(action, left).
%% constant(action, right).
%% constant(action, up).
%% constant(action, down).
%% constant(action, noop).
constant(int, c1).
constant(int, c2).
constant(int, c3).
constant(int, c4).
constant(int, c5).
constant(obj, x).
constant(obj, obj1).
constant(obj, obj2).
constant(obj, wall).

prop(asymmetric_ab_ba,my_succ).
prop(antitriangular,my_succ).
prop(antitransitive,my_succ).

prop(unique_b_a,my_succ).
prop(unique_a_b,my_succ).
prop(unique_b_a,does).
prop(unique_a_b,does).
prop(unique_b_a,controls).
prop(unique_a_b,controls).
prop(unique_ab_c,my_true_at).
prop(unique_c_ab,my_true_at).
prop(unique_b_a,my_input).

%% prop(unique_b_a,my_succ).
%% prop(unique_a_b,my_succ).
%% prop(asymmetric_ab_ba,my_succ).
%% prop(antitriangular,my_succ).
%% prop(antitransitive,my_succ).
%% prop(unique_b_a,does).
%% prop(unique_a_b,does).
%% prop(unique_b_a,controls).
%% prop(unique_a_b,controls).
%% prop(unique_ab_c,my_true_at).
%% prop(unique_c_ab,my_true_at).
%% prop(unique_b_a,my_input).
%% prop(unique_bc_a,my_true_at).
%% prop(unique_b_ac,my_true_at).
%% prop(unique_b_a,my_true_target).
%% prop(unique_ac_b,my_true_at).
%% prop(unique_a_bc,my_true_at).
%% prop(unique_a_b,my_input).
%% prop(unique_a_b,my_true_target).
%% prop(antitriangular,my_true_target).
%% prop(antitransitive,my_true_target).
%% prop(singleton,is_down).
%% prop(singleton,is_left).
%% prop(singleton,is_noop).
%% prop(singleton,is_right).
%% prop(singleton,is_up).
%% prop(singleton,black).
%% prop(singleton,left).
%% prop(singleton,right).
%% prop(singleton,up).
%% prop(singleton,down).
%% prop(singleton,noop).
%% prop(singleton,c1).
%% prop(singleton,c2).
%% prop(singleton,c3).
%% prop(singleton,c4).
%% prop(singleton,c5).
%% prop(singleton,x).
%% prop(singleton,obj1).
%% prop(singleton,obj2).
%% prop(singleton,wall).

prop(unsat_pair,is_box,dir).
prop(unsat_pair,is_noop,dir).
prop(unsat_pair,is_down,is_box).
prop(unsat_pair,is_left,is_box).
prop(unsat_pair,is_noop,is_box).
prop(unsat_pair,is_right,is_box).
prop(unsat_pair,is_up,is_box).
prop(unsat_pair,is_left,is_down).
prop(unsat_pair,is_noop,is_down).
prop(unsat_pair,is_right,is_down).
prop(unsat_pair,is_up,is_down).
prop(unsat_pair,is_noop,is_left).
prop(unsat_pair,is_right,is_left).
prop(unsat_pair,is_up,is_left).
prop(unsat_pair,is_right,is_noop).
prop(unsat_pair,is_up,is_noop).
prop(unsat_pair,is_up,is_right).
prop(unsat_pair,noop,dir).
prop(unsat_pair,my_input,does).
prop(unsat_pair,left,is_box).
prop(unsat_pair,right,is_box).
prop(unsat_pair,up,is_box).
prop(unsat_pair,noop,is_box).
prop(unsat_pair,left,is_down).
prop(unsat_pair,right,is_down).
prop(unsat_pair,up,is_down).
prop(unsat_pair,noop,is_down).
prop(unsat_pair,right,is_left).
prop(unsat_pair,up,is_left).
prop(unsat_pair,noop,is_left).
prop(unsat_pair,left,is_noop).
prop(unsat_pair,right,is_noop).
prop(unsat_pair,up,is_noop).
prop(unsat_pair,left,is_right).
prop(unsat_pair,up,is_right).
prop(unsat_pair,noop,is_right).
prop(unsat_pair,left,is_up).
prop(unsat_pair,right,is_up).
prop(unsat_pair,noop,is_up).
prop(unsat_pair,my_true_target,my_succ).
prop(unsat_pair,right,left).
prop(unsat_pair,up,left).
prop(unsat_pair,noop,left).
prop(unsat_pair,up,right).
prop(unsat_pair,is_box,down).
prop(unsat_pair,is_left,down).
prop(unsat_pair,is_noop,down).
prop(unsat_pair,is_right,down).
prop(unsat_pair,is_up,down).
prop(unsat_pair,left,down).
prop(unsat_pair,right,down).
prop(unsat_pair,up,down).
prop(unsat_pair,noop,down).
prop(unsat_pair,right,noop).
prop(unsat_pair,up,noop).
prop(unsat_pair,c2,c1).
prop(unsat_pair,c3,c1).
prop(unsat_pair,c4,c1).
prop(unsat_pair,c5,c1).
prop(unsat_pair,c3,c2).
prop(unsat_pair,c4,c2).
prop(unsat_pair,c5,c2).
prop(unsat_pair,c4,c3).
prop(unsat_pair,c5,c3).
prop(unsat_pair,c5,c4).
prop(unsat_pair,x,obj1).
prop(unsat_pair,obj2,obj1).
prop(unsat_pair,wall,obj1).
prop(unsat_pair,x,obj2).
prop(unsat_pair,wall,obj2).
prop(unsat_pair,x,wall).