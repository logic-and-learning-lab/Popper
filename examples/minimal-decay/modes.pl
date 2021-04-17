max_clauses(2).
max_vars(5).
max_body(4).

head_pred(next_value,2).
body_pred(does,3).
body_pred(my_true_value,2).
body_pred(my_succ,2).

body_pred(c_pressButton,1).
body_pred(c_noop,1).
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).

type(next_value,(ex,int)).
type(does,(ex,agent,action)).
type(my_true_value,(ex,int)).
type(my_succ,(int,int)).

type(c_pressButton,(action,)).
type(c_noop,(action,)).

type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

:-
    body_literal(C,P1,A,1),
    body_literal(C,P2,A,1),
    var_type(P1,0,int),
    P1 != P2.