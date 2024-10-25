head([H|_],H).
tail([_|T],T).

element([X|_T],X).
element([_|T],X):- element(T,X).

increment(A,B):- succ(A,B).
decrement(A,B):- succ(B,A).

empty([]).

even(A):-
    nonvar(A),
    \+ is_list(A),
    0 is A mod 2.

odd(A):-
    nonvar(A),
    \+ is_list(A),
    1 is A mod 2.

zero(0).
one(1).

gt(A,B):-
    nonvar(A),
    nonvar(B),
    \+is_list(A),
    \+is_list(B),
    A > B.

geq(A,B):-
    nonvar(A),
    nonvar(B),
    \+is_list(A),
    \+is_list(B),
    A >= B.