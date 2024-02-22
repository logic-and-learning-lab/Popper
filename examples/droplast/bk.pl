tail([_|T],T).
head([H|_],H).
sum(A,B,C):- C is A+B.
empty([]).
zero(0).
one(1).
even(A):- 0 is A mod 2.
odd(A):- 1 is A mod 2.
append_(A,B,C):- append([A],B,C).