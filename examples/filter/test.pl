:- [ex].
:- [bk].

filter(A,B):-empty(A),empty(B).
filter(A,B):-cons(C,D,A),even(C),filter(D,E),cons(C,E,B).
filter(A,B):-cons(C,D,A),odd(C),filter(D,B).

a:-
    pos(f(A,B)),
    \+filter(A,B),
    writeln(A-B),
    halt.


b:-
    forall(pos(f(A,B)),filter(A,B)),
    halt.
