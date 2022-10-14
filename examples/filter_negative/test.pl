:-[bk].
:-[exs].

filter_negative(A,D) :-  head(A,B), negative(B), tail(A,C), filter_negative(C,D).
filter_negative(A,E) :-  empty(A), empty(E).
filter_negative(A,E) :-  head(A,B), positive(B), tail(A,C), filter_negative(C,D), cons(B,D,E).

a:-
    forall(pos(filter_negative(A,B)), filter_negative(A,B)).

b:-
    forall(neg(filter_negative(A,B)), \+ filter_negative(A,B)).