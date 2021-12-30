
king(k).
rook(r).
white(w).
black(b).
distance([X1,Y1],[X2,Y2],D) :-
    D1 is abs(X1-X2),
    D2 is abs(Y1-Y2),
    D is max(D1,D2).
one(1).
cell(S,[X,Y],C,T) :- member(c(X,Y,C,T),S).


member(X,[X|_]).
member(X,L):- tail(L,T), member(X,T).

tail([_|T],T).
