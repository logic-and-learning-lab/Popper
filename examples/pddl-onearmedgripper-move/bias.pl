% Generated via https://github.com/tomsilver/popper-policies
% Example expected output:
% ********** SOLUTION **********
% Precision:1.00 Recall:1.00 TP:12 FN:0 TN:92 FP:0 Size:16
% move(A,B,C):- negated_at_robby(B,C),goal_at(D,B,C),at_robby(A,C),carry(D,E,C),gripper(E,C),room(A,C),room(B,C).
% move(A,B,C):- goal_at(E,A,C),negated_at_robby(B,C),free(D,C),at_robby(A,C),room(A,C),negated_carry(E,D,C),room(B,C).
% ******************************

% Set max bounds
max_body(10).
max_vars(6).

% Predicates
body_pred(at,3).
body_pred(at_robby,2).
body_pred(ball,2).
body_pred(carry,3).
body_pred(free,2).
body_pred(gripper,2).
body_pred(negated_at,3).
body_pred(negated_at_robby,2).
body_pred(negated_carry,3).
body_pred(negated_free,2).
body_pred(room,2).

% Goal predicates
body_pred(goal_at,3).

% Action
head_pred(move,3).

% Type constraints
type(ball,(ball,ex_id)).
type(gripper,(gripper,ex_id)).
type(room,(room,ex_id)).
type(at_robby,(room,ex_id)).
type(negated_free,(gripper,ex_id)).
type(negated_at_robby,(room,ex_id)).
type(carry,(ball,gripper,ex_id)).
type(negated_carry,(ball,gripper,ex_id)).
type(at,(ball,room,ex_id)).
type(move,(room,room,ex_id)).
type(free,(gripper,ex_id)).
type(goal_at,(ball,room,ex_id)).
type(negated_at,(ball,room,ex_id)).

% Example ID can only appear once
:- clause(C), #count{V : clause_var(C,V),var_type(C,V,ex_id)} != 1.

% Action preconditions (and suppress ASP warning)
#defined body_literal/4.
:- not body_literal(0,room,2,(1,2)).
:- not body_literal(0,at_robby,2,(0,2)).
:- not body_literal(0,room,2,(0,2)).
