#T1
%% in(2,2)
pos(out(1,2)).
pos(out(2,2)).
pos(out(3,2)).
pos(out(2,3)).
pos(out(2,1)).

#T2
%% in(3,3)
pos(out(2,3)).
pos(out(3,3)).
pos(out(4,3)).
pos(out(3,2)).
pos(out(3,4)).

%% v(1).
%% v(2).
%% v(3).
%% v(4).
%% v(5).

%% neg(out(e1,A,B)):-
%%     v(A),
%%     v(B),
%%     not pos(out(e1,A,B)).
%% neg(out(e2,A,B)):-
%%     v(A),
%%     v(B),
%%     not pos(out(e2,A,B)).
