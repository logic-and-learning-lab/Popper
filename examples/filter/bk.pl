cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).

prepend(A,B,C):-
    append([A],B,C).

cons1(A,B,C):-
    cons(A,B,C).
cons2(A,B,C):-
    cons(A,B,C).
succ(A,B):-
    B is A+1.

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(2).
even(4).
even(6).
even(8).
even(10).