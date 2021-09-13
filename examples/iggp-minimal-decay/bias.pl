%% BEST PROGRAM:
%% next_value(A,B):-c_player(C),c5(B),c_pressButton(D),does(A,C,D)
%% next_value(A,B):-my_succ(B,D),my_true_value(A,D),c_player(C),c_noop(E),does(A,C,E)
%% TP: 8, FN: 0, TN: 46, FP: 0

%% Total programs: 2604
%% Generate:
%%     Called: 2614 times   Total: 2.31     Mean: 0.001     Max: 0.012
%% Test:
%%     Called: 2604 times   Total: 1.45     Mean: 0.001     Max: 0.040
%% Build_Rules:
%%     Called: 2603 times   Total: 1.37     Mean: 0.001     Max: 0.007
%% Ground:
%%     Called: 2603 times   Total: 0.20     Mean: 0.000     Max: 0.003
%% Add:
%%     Called: 2603 times   Total: 1.56     Mean: 0.001     Max: 0.002
%% Test_Individual_Rules.Is_Totally_Incomplete:
%%     Called: 1989 times   Total: 0.10     Mean: 0.000     Max: 0.000
%% Total operation time: 7.00s
%% Total execution time: 7.00s

max_clauses(2).
max_vars(5).
max_body(5).

head_pred(next_value,2).
body_pred(does,3).
body_pred(my_true_value,2).
body_pred(my_succ,2).
body_pred(c_pressButton,1).
body_pred(c_noop,1).
body_pred(c_player,1). % comment to make unsat
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
type(c_player,(agent,)).
type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

%% HACK BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.