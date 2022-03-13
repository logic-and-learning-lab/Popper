max_vars(4).
max_body(4).
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


%% functional(succ,2).
%% irreflexive(succ,2).

%% DIAGONAL

%% out(A,B,C):-in(A,B,C).
%% out(A,B,C):-succ(C,B),out(A,C,D),succ(D,B).
%% out(A,B,C):-succ(D,C),out(A,C,D),succ(D,B).
%% out(A,B,C):-succ(B,C),succ(D,B),out(A,D,B).
