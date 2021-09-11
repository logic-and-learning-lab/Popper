%% SOLUTION:
%% next_score(A,B,C):-different(B,D),my_true_score(A,B,C),wins(A,D)
%% next_score(A,B,C):-my_true_score(A,B,D),wins(A,B),my_succ(D,C)
%% next_score(A,B,C):-does(A,B,D),different(E,B),my_true_score(A,B,C),does(A,E,D)
%% Total programs: 8444
%% Generate:
%%     Called: 8456 times   Total: 13.57    Mean: 0.002     Max: 0.246
%% Test:
%%     Called: 8444 times   Total: 8.76     Mean: 0.001     Max: 0.032
%% Build_Rules:
%%     Called: 8443 times   Total: 1.37     Mean: 0.000     Max: 0.006
%% Ground:
%%     Called: 8443 times   Total: 0.67     Mean: 0.000     Max: 0.003
%% Add:
%%     Called: 8443 times   Total: 9.99     Mean: 0.001     Max: 0.005
%% Total operation time: 34.38s
%% Total execution time: 34.70s

max_clauses(4).
max_vars(5).
max_body(7).

head_pred(next_score,3).
%% body_pred(draws,2).
body_pred(wins,2).
%% body_pred(loses,2).
body_pred(my_true_score,3).
body_pred(my_succ,2).
body_pred(does,3).
body_pred(beats,2).
%% body_pred(player,1).
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

functional(beats,2).
functional(my_succ,2).
irreflexive(my_succ,2).
irreflexive(beats,2).

%% HACK BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.