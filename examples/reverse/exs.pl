pos(f([1,2,3,4],[4,3,2,1])).
pos(f([1,2,3,4,1],[1,4,3,2,1])).
pos(f([a,b,c],[c,b,a])).
pos(f([a,b,c,d,e,f],[f,e,d,c,b,a])).
neg(f([a,b,c],[a,b,c])).
neg(f([a,b,c],[])).
neg(f([a,b],[a])).
neg(f([a,b,c],[b,c])).
neg(f([1,2,3,4],[1,4,3,2])).



f(A,B):-empty(A),empty(B).
f(A,B):-head(A,C),tail(A,D),f(D,E),my_append(E,C,B).

%% :-[bk].
%% :-
%%     forall(pos(X),(call(X))).

%% %% f([],[]).