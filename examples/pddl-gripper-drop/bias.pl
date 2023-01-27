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
head_pred(drop,4).

% Type constraints
type(free,(object,ex_id)).
type(at,(object,object,ex_id)).
type(negated_at,(object,object,ex_id)).
type(room,(object,ex_id)).
type(negated_free,(object,ex_id)).
type(negated_at_robby,(object,ex_id)).
type(negated_carry,(object,object,ex_id)).
type(carry,(object,object,ex_id)).
type(gripper,(object,ex_id)).
type(at_robby,(object,ex_id)).
type(ball,(object,ex_id)).
type(drop,(object,object,object,ex_id)).

% Example ID can only appear once
:- clause(C), #count{V : clause_var(C,V),var_type(C,V,ex_id)} != 1.

% Action preconditions
:- not body_literal(0,room,2,(1,3)).
:- not body_literal(0,carry,3,(0,2,3)).
:- not body_literal(0,ball,2,(0,3)).
:- not body_literal(0,gripper,2,(2,3)).
:- not body_literal(0,at_robby,2,(1,3)).
