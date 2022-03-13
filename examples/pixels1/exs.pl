%% pos(in(e1,1,1)).
pos(out(e1,0,1)).
pos(out(e1,1,1)).
pos(out(e1,2,1)).
%% pos(out(e1,3,1)).

%% 0000
%% 0000
%% 0100
%% 0000
%% ->
%% 0000
%% 0000
%% 1111
%% 0000

%% in(e2,2,2).
%% pos(out(e2,0,2)).
pos(out(e2,1,2)).
pos(out(e2,2,2)).
pos(out(e2,3,2)).


neg(out(e2,1,1)).
neg(out(e2,2,1)).
neg(out(e2,3,3)).

v(1).
v(2).
v(3).
v(4).
v(5).

neg(out(e1,A,B)):-
    v(A),
    v(B),
    not pos(out(e1,A,B)).
neg(out(e2,A,B)):-
    v(A),
    v(B),
    not pos(out(e2,A,B)).

%% 0000
%% 0010
%% 0000
%% 0000
%% ->
%% 0000
%% 0000
%% 1111
%% 0000