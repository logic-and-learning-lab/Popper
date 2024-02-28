is_list([]).
is_list([_|_]).
increment(A,B):- succ(A,B).
decrement(A,B):- succ(B,A).
tail([_|T],T).
head([H|_],H).
empty([]).
element([X|_],X):-!.
element([_|T],X):- element(T,X).
zero(0).
one(1).
geq(A,B):- A >= B.
even(A):- 0 is A mod 2.
odd(A):- 1 is A mod 2.