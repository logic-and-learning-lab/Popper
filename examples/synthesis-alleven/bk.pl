tail([_|T],T).
head([H|_],H).
empty([]).
zero(0).
even(A):-
    0 is A mod 2.