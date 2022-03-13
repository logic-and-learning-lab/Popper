max_clauses(1).
max_vars(6).
max_body(6).

head_pred(next_score,2).
body_pred(my_true_score,2).
body_pred(my_succ,2).
body_pred(does,2).
body_pred(beats,2).
body_pred(player,1).
body_pred(different,2).

type(next_score,(player,int)).
type(my_true_score,(player,int)).
type(my_succ,(int,int)).
type(does,(player,action)).
type(beats,(action,action)).
type(player,(player,)).
type(different,(player,player)).

%% next_score(A,B,C):- my_succ(F,C),beats(G,D),my_true_score(A,B,F),different(B,E),does(A,E,D),does(A,B,G).
%% next_score(A,B,C):- player(F),does(A,B,E),beats(D,E),does(A,F,D),my_true_score(A,B,C).
%% next_score(A,B,C):- does(A,E,D),my_true_score(A,B,C),does(A,B,D),different(E,B).


%% next_score(B,C):- my_succ(F,C),beats(G,D),my_true_score(B,F),different(B,E),does(E,D),does(B,G).
%% next_score(B,C):- player(F),does(B,E),beats(D,E),does(F,D),my_true_score(B,C).
%% next_score(B,C):- does(E,D),my_true_score(B,C),does(B,D),different(E,B).


%% a:-
%%     #count{Vars : body_literal(different,_,Vars)} > 1.

%% a:-
%%     #count{Vars : body_literal(my_succ,_,Vars)} > 1.


%% next_score(A,B,C):- does(A,D,F),different(E,D),my_true_score(A,B,C),does(A,E,F).
%% next_score(A,B,C):- does(A,D,F),different(E,D),my_true_score(A,B,C),does(A,E,F).
%% next_score(B,C):- does(D,F),different(E,D),my_true_score(B,C),does(E,F).


%% pos(next_score(23,p1,1)).
%% next_score(B,C):-
%%     does(G,D),
%%     beats(F,D),
%%     does(B,F),
%%     my_true_score(B,E),
%%     my_succ(E,C),
%%     my_true_score(G,C).


%% pos(next_score(15,p1,1)).
%% next_score(A,B,C):- does(A,F,E),does(A,B,G),my_true_score(A,B,D),different(F,B),beats(G,E),my_succ(D,C).
