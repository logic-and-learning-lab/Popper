max_vars(7).
max_body(6).
max_clauses(1).
constant(agent_black, agent).
constant(obj_x, obj).
constant(obj_wall, obj).
constant(obj_obj1, obj).
constant(obj_d1, obj).
constant(mypos_1, mypos).
constant(mypos_2, mypos).
constant(mypos_3, mypos).
constant(mypos_4, mypos).
constant(mypos_5, mypos).
constant(score_100, score).
constant(action_noop, action).
constant(action_up, action).
constant(action_down, action).
constant(action_left, action).
constant(action_right, action).
head_pred(next_open,3).
body_pred(true_at,4).
body_pred(true_target,3).
body_pred(true_open,3).
body_pred(true_switch,4).
body_pred(input,2).
body_pred(does,3).
body_pred(role,1).
body_pred(bounds,1).
body_pred(controls,2).
body_pred(dir,1).
body_pred(door,1).
body_pred(is_box,1).
body_pred(object,1).
body_pred(player_obj,1).
body_pred(succ,2).
type(true_at,(ex,mypos,mypos,obj)).
type(true_target,(ex,mypos,mypos)).
type(true_open,(ex,obj,mypos)).
type(true_switch,(ex,mypos,mypos,obj)).
type(next_open,(ex,obj,mypos)).
type(input,(agent,action)).
type(does,(ex,agent,action)).
type(role,(agent,)).
type(bounds,(mypos,)).
type(controls,(agent,obj)).
type(dir,(action,)).
type(door,(obj,)).
type(is_box,(obj,)).
type(object,(obj,)).
type(player_obj,(obj,)).
type(succ,(mypos,mypos)).

%% magic_value_type(mypos).
%% magic_value_type(action).
%% magic_value_type(score).

asda:-
	clause(C),
	#count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).
