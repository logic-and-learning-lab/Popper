%% filter/keep the even elements of a list
%% f(V0,V1):- empty(V0),empty(V1).
%% f(V0,V1):- tail(V0,V3),head(V0,V2),odd(V2),f(V3,V1).
%% f(V0,V1):- head(V0,V3),tail(V0,V2),even(V3),f(V2,V4),prepend(V3,V4,V1).


tail([_|T],T).
head([H|_],H).
empty([]).
prepend(A,B,C):- append([A],B,C).
succ(A,B):- B is A+1.

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(2).
even(4).
even(6).
even(8).
even(10).