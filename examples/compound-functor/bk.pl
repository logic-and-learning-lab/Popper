valid_n(n(a(X), b(Y))) :-
   Y is X + 1. 

valid([]).
valid([H|T]) :-
    valid_n(H),
    valid(T).