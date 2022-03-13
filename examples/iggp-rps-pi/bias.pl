%% max_vars(7).
%% max_body(7).
enable_pi.
max_vars(7).
max_body(4).

head_pred(f,3).
body_pred(my_true_score,3).
body_pred(my_succ,2).
body_pred(does,3).
body_pred(beats,2).
body_pred(different,2).
%% body_pred(wins,2).

type(f,(ex, player, int)).
type(my_true_score,(ex, player, int)).
type(my_succ,(int, int)).
type(does,(ex, player, action)).
type(beats,(action, action)).
type(different,(player, player)).
type(wins,(ex, player)).


%% f(E,A,B):- my_true_score(E,A,C), my_succ(C,B), wins(E,A).
%% f(E,A,B):- my_true_score(E,A,B), wins(E,C), different(C,A).


%% wins(E,A):-
%%     does(E,A,B),
%%     does(E,C,D),
%%     beats(B,D),
%%     different(A,C).