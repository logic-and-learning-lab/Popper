%% SOLUTION:
%% next_value(A,B):-does(A,C,D),c_pressButton(D),c5(B),c_player(C)
%% next_value(A,B):-c_player(E),my_succ(B,C),my_true_value(A,C),does(A,E,D),c_noop(D)
%% Total programs: 24104
%% Generate:
%%     Called: 24114 times      Total: 29.08    Mean: 0.001     Max: 0.026
%% Test:
%%     Called: 24104 times      Total: 18.17    Mean: 0.001     Max: 0.031
%% Build_Rules:
%%     Called: 24103 times      Total: 4.01     Mean: 0.000     Max: 0.006
%% Ground:
%%     Called: 24103 times      Total: 0.84     Mean: 0.000     Max: 0.003
%% Add:
%%     Called: 24103 times      Total: 27.83    Mean: 0.001     Max: 0.006
%% Total operation time: 79.93s
%% Total execution time: 80.51s

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
body_pred(c1,1). % comment to make easier to solve
body_pred(c2,1). % comment to make easier to solve
body_pred(c3,1). % comment to make easier to solve
body_pred(c4,1). % comment to make easier to solve
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

functional(my_succ,2).
irreflexive(my_succ,2).
functional(my_true_value,2).

%% HACK BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.