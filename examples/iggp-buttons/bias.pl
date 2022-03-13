max_clauses(1).
max_vars(6).
max_body(6).

%% :- clause_var(7).
%% :- clause_var(6).
%% :- clause_var(5).
%% :- clause_var(4).

head_pred(next,1).
body_pred(does,2).
body_pred(my_input,1).
body_pred(my_true,1).
body_pred(my_succ,2).
body_pred(role,1).
body_pred(c_p,1).
body_pred(c_q,1).
body_pred(c_r,1).
body_pred(c_a,1).
body_pred(c_b,1).
body_pred(c_c,1).
body_pred(not_my_true,1).

type(next,(prop,)).
type(does,(agent,action)).
type(my_input,(agent,action)).
type(my_true,(prop,)).
type(my_succ,(prop,prop)).
type(role,(agent,)).
type(c_p,(prop,)).
type(c_q,(prop,)).
type(c_r,(prop,)).
type(c_a,(action,)).
type(c_b,(action,)).
type(c_c,(action,)).
type(not_my_true,(prop,)).
