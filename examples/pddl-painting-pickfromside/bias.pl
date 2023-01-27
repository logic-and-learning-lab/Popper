% Generated via https://github.com/tomsilver/popper-policies
% Example expected output:
% ********** SOLUTION **********
% Precision:1.00 Recall:1.00 TP:3 FN:0 TN:59 FP:0 Size:5
% pickfromside(A,B,C):- ontable(A,C),gripperopen(B,C),negated_inshelf(A,D,C),goal_inshelf(A,D,C).
% ******************************

% Set max bounds
max_body(10).
max_vars(6).

% Predicates
body_pred(gripperopen,2).
body_pred(holding,2).
body_pred(holdingside,2).
body_pred(holdingtop,2).
body_pred(inbox,3).
body_pred(inshelf,3).
body_pred(isboxcolor,3).
body_pred(isclean,2).
body_pred(isdirty,2).
body_pred(isdry,2).
body_pred(isshelfcolor,3).
body_pred(iswet,2).
body_pred(negated_gripperopen,2).
body_pred(negated_holding,2).
body_pred(negated_holdingside,2).
body_pred(negated_holdingtop,2).
body_pred(negated_inbox,3).
body_pred(negated_inshelf,3).
body_pred(negated_isboxcolor,3).
body_pred(negated_isclean,2).
body_pred(negated_isdirty,2).
body_pred(negated_isdry,2).
body_pred(negated_isshelfcolor,3).
body_pred(negated_iswet,2).
body_pred(negated_notontable,2).
body_pred(negated_ontable,2).
body_pred(notontable,2).
body_pred(ontable,2).

% Goal predicates
body_pred(goal_inbox,3).
body_pred(goal_inshelf,3).
body_pred(goal_isboxcolor,3).
body_pred(goal_isshelfcolor,3).

% Action
head_pred(pickfromside,3).

% Type constraints
type(goal_isshelfcolor,(obj,shelf,ex_id)).
type(pickfromside,(obj,robot,ex_id)).
type(holdingtop,(obj,ex_id)).
type(isdirty,(obj,ex_id)).
type(iswet,(obj,ex_id)).
type(goal_inshelf,(obj,shelf,ex_id)).
type(goal_inbox,(obj,box,ex_id)).
type(isdry,(obj,ex_id)).
type(negated_isdry,(obj,ex_id)).
type(negated_notontable,(obj,ex_id)).
type(inbox,(obj,box,ex_id)).
type(inshelf,(obj,shelf,ex_id)).
type(negated_isdirty,(obj,ex_id)).
type(isboxcolor,(obj,box,ex_id)).
type(negated_isshelfcolor,(obj,shelf,ex_id)).
type(negated_isboxcolor,(obj,box,ex_id)).
type(notontable,(obj,ex_id)).
type(holdingside,(obj,ex_id)).
type(negated_ontable,(obj,ex_id)).
type(negated_inbox,(obj,box,ex_id)).
type(negated_iswet,(obj,ex_id)).
type(isshelfcolor,(obj,shelf,ex_id)).
type(negated_gripperopen,(robot,ex_id)).
type(holding,(obj,ex_id)).
type(isclean,(obj,ex_id)).
type(negated_holding,(obj,ex_id)).
type(negated_holdingside,(obj,ex_id)).
type(ontable,(obj,ex_id)).
type(goal_isboxcolor,(obj,box,ex_id)).
type(negated_holdingtop,(obj,ex_id)).
type(negated_isclean,(obj,ex_id)).
type(negated_inshelf,(obj,shelf,ex_id)).
type(gripperopen,(robot,ex_id)).

% Example ID can only appear once
:- clause(C), #count{V : clause_var(C,V),var_type(C,V,ex_id)} != 1.

% Action preconditions (and suppress ASP warning)
#defined body_literal/4.
:- not body_literal(0,gripperopen,2,(1,2)).
:- not body_literal(0,ontable,2,(0,2)).
