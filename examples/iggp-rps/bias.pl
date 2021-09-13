%% BEST PROGRAM:
%% next_score(A,B,C):-my_true_score(A,B,D),my_succ(D,C),wins(A,B)
%% next_score(A,B,C):-wins(A,D),my_true_score(A,B,C),different(B,D)
%% next_score(A,B,C):-does(A,D,E),my_true_score(A,B,C),different(B,D),does(A,B,E)
%% TP: 108, FN: 0, TN: 356, FP: 0

%% Total programs: 1607
%% Generate:
%%     Called: 1619 times   Total: 4.64     Mean: 0.003     Max: 0.041
%% Test:
%%     Called: 1607 times   Total: 7.98     Mean: 0.005     Max: 0.048
%% Build_Rules:
%%     Called: 1606 times   Total: 3.20     Mean: 0.002     Max: 0.009
%% Ground:
%%     Called: 1606 times   Total: 0.26     Mean: 0.000     Max: 0.003
%% Add:
%%     Called: 1606 times   Total: 1.83     Mean: 0.001     Max: 0.004
%% Test_Individual_Rules.Is_Totally_Incomplete:
%%     Called: 1539 times   Total: 0.05     Mean: 0.000     Max: 0.001
%% Total operation time: 17.96s
%% Total execution time: 18.08s

max_clauses(4).
max_vars(5).
max_body(7).

head_pred(next_score,3).
body_pred(wins,2).
body_pred(my_true_score,3).
body_pred(my_succ,2).
body_pred(does,3).
body_pred(beats,2).
body_pred(player,1).
body_pred(different,2).

type(next_score,(ex,player,int)).
type(draws,(ex,player)).
type(wins,(ex,player)).
type(loses,(ex,player)).
type(my_true_score,(ex,player,int)).
type(my_succ,(int,int)).
type(does,(ex,player,action)).
type(beats,(action,action)).
type(player,(player,)).
type(different,(player,player)).

%% HACK BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.