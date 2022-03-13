%% 15:23:10 next_cell(A,B,C):-role(E),my_true_cell(A,B,C),does_jump(A,E,F,D),different(B,F),different(D,B).
%% 15:23:10 next_cell(A,B,C):-does_jump(A,E,F,D),different(D,F),does_jump(A,E,F,B),c_twocoins(C).
%% 15:23:10 next_cell(A,B,C):-does_jump(A,F,B,E),does_jump(A,F,D,E),c_zerocoins(C),different(E,D).


max_vars(6).
max_body(6).
max_clauses(1).
head_pred(next_cell,2).
body_pred(does_jump,3).
body_pred(my_succ,2).
body_pred(my_true_cell,2).
body_pred(role,1).
body_pred(my_pos,1).
body_pred(different,2).
body_pred(c_zerocoins,1).
body_pred(c_onecoin,1).
body_pred(c_twocoins,1).

type(next_cell,(pos,cell_value)).
type(does_jump,(agent,pos,pos)).
type(my_succ,(pos,pos)).
type(my_true_cell,(pos,cell_value)).
type(role,(agent,)).
type(my_pos,(pos,)).
type(different,(pos,pos)).
type(c_zerocoins,(cell_value,)).
type(c_onecoin,(cell_value,)).
type(c_twocoins,(cell_value,)).