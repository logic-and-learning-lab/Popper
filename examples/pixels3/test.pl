:-[bk].
:-[exs].

:- table out/3.

out(A,B,C):-succ(D,B),out(A,D,C).
out(A,B,C):-succ(B,D),out(A,D,C).
out(A,B,C):-in(A,B,C).
out(A,B,C):-succ(D,B),in(A,C,D).
out(A,B,C):-in(A,C,D),succ(B,D).



a:-
    out(A,B,C),
    writeln(out(A,B,C)).