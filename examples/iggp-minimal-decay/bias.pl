head_pred(next_value,2).
body_pred(does,3).
body_pred(true_value,2).
body_pred(my_succ,2).
body_pred(pressButton,1).
body_pred(noop,1).
body_pred(player,1).
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).

type(next_value,(ex,int)).
type(does,(ex,agent,action)).
type(true_value,(ex,int)).
type(my_succ,(int,int)).
type(pressButton,(action,)).
type(noop,(action,)).
type(player,(agent,)).
type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.