max_vars(7).
max_body(7).

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

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

%% :-
%%     clause(Rule),
%%     #count{B,C : body_literal(Rule,does,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{B : body_literal(Rule,does,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{B,C : body_literal(Rule,my_true_score,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{B : body_literal(Rule,my_true_score,_,(A,B,C))} > 2.

%% :-
%%     clause(C),
%%     #count{V : clause_var(C,V),var_type(C,V,player)} > 2.

%% :-
%%     clause(C),
%%     #count{V : clause_var(C,V),var_type(C,V,action)} > 3.



%% ==========







%% :- clause(Rule), #count{V1,V2: body_literal(Rule,my_true_score,_,(V0,V1,V2))} > 2.
%% :- clause(Rule), #count{V1: body_literal(Rule,my_true_score,_,(V0,V1,V2))} > 2.
%% :- clause(Rule), #count{V2: body_literal(Rule,my_true_score,_,(V0,V1,V2))} > 1.
%% :- clause(Rule), #count{V0: body_literal(Rule,different,_,(V0,V1))} > 1.
%% :- clause(Rule), #count{V1: body_literal(Rule,different,_,(V0,V1))} > 1.

%% :- clause(Rule), #count{V1,V2: body_literal(Rule,does,_,(V0,V1,V2))} > 2.
%% :- clause(Rule), #count{V1: body_literal(Rule,does,_,(V0,V1,V2))} > 2.
%% :- clause(Rule), body_literal(Rule,does,_,(V0,V1,V2)), #count{V2: body_literal(Rule,does,_,(V0,V1,V2))} > 1.
%% %% :- clause(Rule), #count{V0: body_literal(Rule,my_succ,_,(V0,V1))} > 1.
%% %% :- clause(Rule), #count{V1: body_literal(Rule,my_succ,_,(V0,V1))} > 1.
%% %% :- clause(Rule), #count{V0: body_literal(Rule,beats,_,(V0,V1))} > 1.
%% %% :- clause(Rule), #count{V1: body_literal(Rule,beats,_,(V0,V1))} > 1.
