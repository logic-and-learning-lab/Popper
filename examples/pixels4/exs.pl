pos(out(e1,1,1)).
pos(out(e1,1,2)).
pos(out(e1,1,3)).
pos(out(e1,1,4)).
pos(out(e1,1,5)).

pos(out(e1,2,1)).
pos(out(e1,2,2)).
pos(out(e1,2,3)).
pos(out(e1,2,4)).
pos(out(e1,2,5)).


v(1).
v(2).
v(3).
v(4).
v(5).

neg(out(e1,A,B)):-
    v(A),
    v(B),
    \+ pos(out(e1,A,B)).
