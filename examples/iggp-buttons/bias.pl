max_vars(6).
max_body(6).

%% 10:17:46 next(A,B):-c_b(C),my_input(D,C),does(A,D,C),my_true(A,B),c_r(B)
%% 10:17:46 next(A,B):-c_b(C),my_input(D,C),c_p(E),does(A,D,C),c_q(B),my_true(A,E)
%% 10:17:46 next(A,B):-not_my_true(A,B),my_input(D,C),c_a(C),does(A,D,C),c_p(B)
%% 10:17:46 next(A,B):-my_succ(C,B),my_true(A,C)
%% 10:17:46 next(A,B):-c_q(B),does(A,E,D),c_c(D),c_r(C),role(E),my_true(A,C)
%% 10:17:46 next(A,B):-my_input(E,C),c_c(C),does(A,E,C),c_q(D),my_true(A,D),c_r(B)
%% 10:17:46 next(A,B):-my_input(C,D),c_p(B),c_c(D),my_true(A,B),does(A,C,D)
%% 10:17:46 next(A,B):-c_b(E),does(A,C,E),c_q(D),c_p(B),my_input(C,E),my_true(A,D)

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


%% prop(antitransitive,my_succ).
%% prop(antitriangular,my_succ).
%% prop(asymmetric_ab_ba,my_succ).
%% prop(pab_qac,(does,my_input)).
%% prop(postcon,(my_succ,c_p)).
%% prop(postcon,(my_succ,c_q)).
%% prop(postcon,(my_succ,c_r)).
%% prop(postcon,(my_succ,not_my_true)).
%% prop(pre_postcon,(c_p,my_succ,c_p)).
%% prop(pre_postcon,(c_p,my_succ,c_q)).
%% prop(pre_postcon,(c_p,my_succ,c_r)).
%% prop(pre_postcon,(c_p,my_succ,my_true)).
%% prop(pre_postcon,(c_p,my_succ,not_my_true)).
%% prop(pre_postcon,(c_q,my_succ,c_p)).
%% prop(pre_postcon,(c_q,my_succ,c_q)).
%% prop(pre_postcon,(c_q,my_succ,c_r)).
%% prop(pre_postcon,(c_q,my_succ,my_true)).
%% prop(pre_postcon,(c_q,my_succ,not_my_true)).
%% prop(pre_postcon,(c_r,my_succ,c_p)).
%% prop(pre_postcon,(c_r,my_succ,c_q)).
%% prop(pre_postcon,(c_r,my_succ,c_r)).
%% prop(pre_postcon,(c_r,my_succ,my_true)).
%% prop(pre_postcon,(c_r,my_succ,not_my_true)).
%% prop(pre_postcon,(my_true,my_succ,c_p)).
%% prop(pre_postcon,(my_true,my_succ,c_q)).
%% prop(pre_postcon,(my_true,my_succ,c_r)).
%% prop(pre_postcon,(my_true,my_succ,my_true)).
%% prop(pre_postcon,(my_true,my_succ,not_my_true)).
%% prop(pre_postcon,(not_my_true,my_succ,c_p)).
%% prop(pre_postcon,(not_my_true,my_succ,c_q)).
%% prop(pre_postcon,(not_my_true,my_succ,c_r)).
%% prop(pre_postcon,(not_my_true,my_succ,my_true)).
%% prop(pre_postcon,(not_my_true,my_succ,not_my_true)).
%% prop(pre_postcon,(role,my_input,c_a)).
%% prop(pre_postcon,(role,my_input,c_b)).
%% prop(pre_postcon,(role,my_input,c_c)).
%% prop(precon,(c_p,my_succ)).
%% prop(precon,(c_q,my_succ)).
%% prop(precon,(c_r,my_succ)).
%% prop(precon,(not_my_true,my_succ)).
%% prop(singleton,c_a).
%% prop(singleton,c_b).
%% prop(singleton,c_c).
%% prop(singleton,c_p).
%% prop(singleton,c_q).
%% prop(singleton,c_r).
%% prop(singleton,role).
%% prop(unique_a_b,does).
%% prop(unique_a_b,my_succ).
%% prop(unique_b_a,does).
%% prop(unique_b_a,my_succ).
%% prop(countk,c_a,1).
%% prop(countk,c_b,1).
%% prop(countk,c_c,1).
%% prop(countk,c_p,1).
%% prop(countk,c_q,1).
%% prop(countk,c_r,1).
%% prop(countk,role,1).
%% prop(unsat_pair,c_b,c_a).
%% prop(unsat_pair,c_c,c_a).
%% prop(unsat_pair,c_c,c_b).
%% prop(unsat_pair,c_q,c_p).
%% prop(unsat_pair,c_r,c_p).
%% prop(unsat_pair,c_r,c_q).
%% prop(unsat_pair,not_my_true,my_true).
%% prop(countk,my_true,3).
