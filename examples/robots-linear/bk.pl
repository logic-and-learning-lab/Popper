min_x(0).
max_x(100).
min_y(0).
max_y(0).

left(w(X,Y),w(Z,Y)) :- Z is X - 1, min_x(A), Z >= A.
right(w(X,Y),w(Z,Y)) :- Z is X + 1, max_x(A), A >= Z.
up(w(X,Y),w(X,Z)) :- Z is Y + 1, max_y(B), B >= Z.
down(w(X,Y),w(X,Z)) :- Z is Y - 1, min_y(B), Z >= B.

cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    C is A+B.
empty([]).
zero(0).
