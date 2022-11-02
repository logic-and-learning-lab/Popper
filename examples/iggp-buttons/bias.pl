head_pred(next,2).
body_pred(does,3).
body_pred(my_input,2).
body_pred(my_true,2).
body_pred(my_succ,2).
body_pred(role,1).
body_pred(c_p,1).
body_pred(c_q,1).
body_pred(c_r,1).
body_pred(c_a,1).
body_pred(c_b,1).
body_pred(c_c,1).
body_pred(not_my_true,2).

type(next,(ex,prop)).
type(does,(ex,agent,action)).
type(my_input,(agent,action)).
type(my_true,(ex,prop)).
type(my_succ,(prop,prop)).
type(role,(agent,)).
type(c_p,(prop,)).
type(c_q,(prop,)).
type(c_r,(prop,)).
type(c_a,(action,)).
type(c_b,(action,)).
type(c_c,(action,)).
type(not_my_true,(ex,prop)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.