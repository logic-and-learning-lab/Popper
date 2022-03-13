max_vars(6).
max_body(4).
%% max_clauses(1).
%% enable_recursion.

head_pred(out,4).
body_pred(in,4).
body_pred(succ,2).
body_pred(black,1).
body_pred(blue,1).

type(out,(ex, int, int, colour)).
type(in,(ex, int, int, colour)).
type(succ,(int, int)).
type(black,(colour, )).
type(blue,(colour, )).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

%% SURROUND A PIXEL
%% pixel to the left and a pixel to the right