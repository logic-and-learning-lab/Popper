tail([_|T],T).
head([H|_],H).
empty([]).
my_append(A,B,C):-
    append(A,[B],C).

droplast(A,B):-f(A,C),tail(C,D),f(D,B).