max_vars(7).

head_pred(next_score,3).
body_pred(my_true_score,3).
body_pred(my_succ,2).
body_pred(does,3).
body_pred(beats,2).
body_pred(player,1).
body_pred(different,2).

type(next_score,(ex,player,int)).
type(my_true_score,(ex,player,int)).
type(my_succ,(int,int)).
type(does,(ex,player,action)).
type(beats,(action,action)).
type(player,(player,)).
type(different,(player,player)).

%% %% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : var_type(C,V,ex)} != 1.