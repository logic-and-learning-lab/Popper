:-[bk].
:-[exs].



counteven(A,B):- zero(B),empty(A).
%% counteven(A,B):- prepend(D,C,A),counteven(C,B),odd(D).
counteven(A,B):- prepend(D,C,A),odd(D),counteven(C,B).
counteven(A,B):- prepend(E,C,A),even(E),counteven(C,D),increment(D,B).


a:-
    forall(pos(counteven(A,B)), counteven(A,B)).

b:-
    forall(neg(counteven(A,B)), \+ counteven(A,B)).