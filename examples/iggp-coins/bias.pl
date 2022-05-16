%% next_cell(A,B,C):-does_jump(A,E,B,D),my_pos(F),c_zerocoins(C),does_jump(A,E,F,D).
%% next_cell(A,B,C):-role(E),does_jump(A,E,D,B),c_twocoins(C),different(D,B).
%% next_cell(A,B,C):-role(D),different(B,E),my_true_cell(A,B,C),different(B,F),does_jump(A,D,E,F).

max_vars(6).
max_body(6).
max_clauses(1).

head_pred(next_cell,3).
body_pred(does_jump,4).
body_pred(my_succ,2).
body_pred(my_true_cell,3).
body_pred(role,1).
body_pred(my_pos,1).
body_pred(different,2).
body_pred(c_zerocoins,1).
body_pred(c_onecoin,1).
body_pred(c_twocoins,1).

type(next_cell,(ex,pos,cell_value)).
type(does_jump,(ex,agent,pos,pos)).
type(my_succ,(pos,pos)).
type(my_true_cell,(ex,pos,cell_value)).
type(role,(agent,)).
type(my_pos,(pos,)).
type(different,(pos,pos)).
type(c_zerocoins,(cell_value,)).
type(c_onecoin,(cell_value,)).
type(c_twocoins,(cell_value,)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.
