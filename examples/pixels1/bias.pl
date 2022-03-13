max_vars(5).
max_body(4).
max_clauses(1).
%% enable_recursion.

head_pred(out,3).
body_pred(in,3).
body_pred(succ,2).

type(out,(ex, int, int)).
type(in,(ex, int, int)).
type(succ,(int, int)).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

%% SURROUND A PIXEL
%% pixel to the left and a pixel to the right