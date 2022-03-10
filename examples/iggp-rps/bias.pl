max_clauses(1).
max_vars(7).
max_body(6).

head_pred(next_score,3).
%% body_pred(wins,2).
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
    #count{V : clause_var(V),var_type(V,ex)} != 1.