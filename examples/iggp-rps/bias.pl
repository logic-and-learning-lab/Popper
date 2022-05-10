max_clauses(1).
max_vars(7).
max_body(6).

head_pred(next_score,2).
body_pred(my_true_score,2).
body_pred(my_succ,2).
body_pred(does,2).
body_pred(beats,2).
body_pred(player,1).
body_pred(different,2).

type(next_score,(player,int)).
type(my_true_score,(player,int)).
type(my_succ,(int,int)).
type(does,(player,action)).
type(beats,(action,action)).
type(player,(player,)).
type(different,(player,player)).

prop(antitransitive,beats).
prop(antitransitive,different).
prop(antitransitive,my_succ).
prop(antitriangular,different).
prop(antitriangular,my_succ).
prop(asymmetric_ab_ba,beats).
prop(asymmetric_ab_ba,my_succ).
prop(symmetric_ab,different).
prop(unique_a_b,beats).
prop(unique_a_b,different).
prop(unique_a_b,does).
prop(unique_a_b,my_succ).
prop(unique_a_b,my_true_score).
prop(unique_b_a,beats).
prop(unique_b_a,different).
prop(unique_b_a,my_succ).
prop(countk,beats,3).
prop(countk,different,2).
prop(countk,my_succ,3).
prop(countk,my_true_score,2).
prop(countk,player,2).


