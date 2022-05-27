:-[bk].
:-[exs].
f(A):-tail(A,B),f(B).
f(A):-c2(B),head(A,B).


a:-
    f([0,0,2,0,0,0]).