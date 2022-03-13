max_clauses(1).
max_vars(6).
max_body(6).

head_pred(next_value,1).
body_pred(does,2).
body_pred(my_true_value,1).
body_pred(my_succ,2).
body_pred(c_pressButton,1).
body_pred(c_noop,1).
body_pred(c_player,1). % comment to make unsat
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).

type(next_value,(int,)).
type(does,(agent,action)).
type(my_true_value,(int,)).
type(my_succ,(int,int)).
type(c_pressButton,(action,)).
type(c_noop,(action,)).
type(c_player,(agent,)).
type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

%% prop(asymmetric_ab_ba,my_succ).
%% prop(unique_a_b,my_succ).
%% prop(unique_b_a,my_succ).
%% prop(antitransitive,my_succ).
%% prop(antitriangular,my_succ).