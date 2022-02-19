%% max_clauses(6).
max_clauses(1).
max_vars(6).
max_body(6).

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

%% antitransitive(my_succ).
%% antitriangular(my_succ).
%% asymmetric(my_succ).
%% unique_pA(c_a).
%% unique_pA(c_b).
%% unique_pA(c_c).
%% unique_pA(c_p).
%% unique_pA(c_q).
%% unique_pA(c_r).
%% unique_pA(role).
%% unique_pAb(my_input).
%% unique_paB(my_succ).
%% unique_pAb(my_succ).
%% unique_pabC(does).
%% unique_paBc(does).
%% unique_paBC(does).
%% unsat_pair_pa(c_b,c_a).
%% unsat_pair_pa(c_c,c_a).
%% unsat_pair_pa(c_c,c_b).
%% unsat_pair_pa(c_q,c_p).
%% unsat_pair_pa(c_r,c_p).
%% unsat_pair_pa(c_r,c_q).
%% unsat_pair_pab(not_my_true,my_true).
%% unsat_postcon_pab_qb(my_succ,c_p).
%% unsat_postcon_pab_qb(my_succ,c_q).
%% unsat_postcon_pab_qb(my_succ,c_r).
%% unsat_precon_pa_qab(c_p,my_succ).
%% unsat_precon_pa_qab(c_q,my_succ).
%% unsat_precon_pa_qab(c_r,my_succ).


%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.