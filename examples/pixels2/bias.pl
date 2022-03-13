max_vars(4).
max_body(2).
max_clauses(1).
enable_recursion.

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


%% SURROUND THE INPUT PIXEL
%% out(A,B,C):-in(A,B,C).
%% out(A,B,C):-in(A,B,D),succ(D,C).
%% out(A,B,C):-succ(C,D),in(A,B,D).
%% out(A,B,C):-succ(B,D),in(A,D,C).
%% out(A,B,C):-succ(D,B),in(A,D,C).
