king(k).
rook(r).
white(w).
black(b).
one(1).
%% distance((X1,Y1),(X2,Y2),D) :-
%%     D1 is abs(X1-X2),
%%     D2 is abs(Y1-Y2),
%%     D is max(D1,D2).

rank(A):-
    cell(_,(A,_),_,_).
file(A):-
    cell(_,(_,A),_,_).

distance((X1,Y1),(X2,Y2),D1) :-
    rank(X1),
    rank(X2),
    file(Y1),
    file(Y2),
    |X1-X2| = D1,
    |Y1-Y2| = D2,
    D1 >= D2.

distance((X1,Y1),(X2,Y2),D2) :-
    rank(X1),
    rank(X2),
    file(Y1),
    file(Y2),
    |X1-X2| = D1,
    |Y1-Y2| = D2,
    D1 <= D2.


cell(0,(6, 5), w, r).
cell(0,(4, 4), b, k).
cell(0,(7, 4), w, k).
cell(1,(5, 7), w, r).
cell(1,(5, 1), b, k).
cell(1,(6, 6), w, k).
cell(2,(2, 2), w, r).
cell(2,(2, 6), b, k).
cell(2,(3, 1), w, k).
cell(3,(4, 3), w, r).
cell(3,(1, 7), b, k).
cell(3,(5, 2), w, k).
cell(4,(6, 5), w, r).
cell(4,(7, 7), b, k).
cell(4,(7, 4), w, k).
cell(5,(2, 1), w, r).
cell(5,(1, 2), b, k).
cell(5,(3, 1), w, k).
cell(6,(5, 8), w, r).
cell(6,(2, 6), b, k).
cell(6,(5, 7), w, k).
cell(7,(2, 1), w, r).
cell(7,(8, 4), b, k).
cell(7,(3, 2), w, k).
cell(8,(1, 1), w, r).
cell(8,(4, 8), b, k).
cell(8,(0, 2), w, k).
cell(9,(1, 7), w, r).
cell(9,(6, 1), b, k).
cell(9,(0, 8), w, k).
cell(10,(6, 8), w, r).
cell(10,(6, 6), b, k).
cell(10,(6, 7), w, k).
cell(11,(2, 5), w, r).
cell(11,(3, 4), b, k).
cell(11,(1, 4), w, k).
cell(12,(5, 4), w, r).
cell(12,(2, 8), b, k).
cell(12,(6, 4), w, k).
cell(13,(6, 8), w, r).
cell(13,(6, 2), b, k).
cell(13,(5, 8), w, k).
cell(14,(3, 6), w, r).
cell(14,(2, 7), b, k).
cell(14,(3, 7), w, k).
cell(15,(2, 6), w, r).
cell(15,(7, 3), b, k).
cell(15,(1, 5), w, k).
cell(16,(4, 5), w, r).
cell(16,(7, 4), b, k).
cell(16,(4, 4), w, k).
cell(17,(2, 1), w, r).
cell(17,(3, 3), b, k).
cell(17,(2, 0), w, k).
cell(18,(3, 5), w, r).
cell(18,(7, 1), b, k).
cell(18,(2, 6), w, k).
cell(19,(5, 8), w, r).
cell(19,(3, 1), b, k).
cell(19,(6, 7), w, k).
cell(20,(6, 5), w, r).
cell(20,(5, 3), b, k).
cell(20,(6, 6), w, k).
cell(21,(7, 4), w, r).
cell(21,(8, 1), b, k).
cell(21,(7, 5), w, k).
cell(22,(3, 8), w, r).
cell(22,(8, 4), b, k).
cell(22,(4, 8), w, k).
cell(23,(5, 5), w, r).
cell(23,(6, 1), b, k).
cell(23,(6, 4), w, k).
cell(24,(6, 7), w, r).
cell(24,(2, 3), b, k).
cell(24,(7, 6), w, k).
cell(25,(4, 2), w, k).
cell(25,(2, 2), b, k).
cell(25,(5, 5), b, k).
cell(26,(4, 7), b, k).
cell(26,(7, 6), w, r).
cell(26,(1, 7), w, r).
cell(27,(2, 6), b, r).
cell(27,(5, 6), w, r).
cell(27,(7, 8), w, k).
cell(28,(7, 2), w, r).
cell(28,(4, 7), w, r).
cell(28,(1, 1), w, k).
cell(29,(6, 6), b, r).
cell(29,(8, 3), w, k).
cell(29,(8, 6), b, k).
cell(30,(8, 3), w, k).
cell(30,(1, 6), b, k).
cell(30,(6, 5), b, r).
cell(31,(1, 8), w, k).
cell(31,(5, 7), b, k).
cell(31,(7, 4), w, r).
cell(32,(2, 3), b, r).
cell(32,(1, 1), w, r).
cell(32,(5, 2), w, r).
cell(33,(6, 5), w, k).
cell(33,(4, 2), w, r).
cell(33,(1, 8), b, k).
cell(34,(1, 7), b, k).
cell(34,(5, 2), b, r).
cell(34,(3, 5), w, k).
cell(35,(1, 3), b, r).
cell(35,(5, 1), w, k).
cell(35,(1, 4), w, k).
cell(36,(1, 8), b, r).
cell(36,(7, 7), w, k).
cell(36,(8, 8), w, k).
cell(37,(4, 1), b, k).
cell(37,(1, 6), w, k).
cell(37,(1, 3), b, r).
cell(38,(7, 6), b, k).
cell(38,(8, 5), w, r).
cell(38,(7, 8), w, k).
cell(39,(8, 4), w, k).
cell(39,(7, 7), b, k).
cell(39,(7, 4), w, k).
cell(40,(3, 5), w, k).
cell(40,(5, 7), b, k).
cell(40,(2, 1), w, k).
cell(41,(8, 6), w, r).
cell(41,(7, 2), b, k).
cell(41,(4, 4), b, k).
cell(42,(6, 5), b, k).
cell(42,(5, 1), b, k).
cell(42,(8, 1), b, r).
cell(43,(1, 3), w, r).
cell(43,(7, 3), w, k).
cell(43,(8, 3), b, r).
cell(44,(8, 3), w, r).
cell(44,(7, 3), b, k).
cell(44,(4, 4), w, k).
cell(45,(7, 2), w, r).
cell(45,(7, 4), b, r).
cell(45,(4, 7), w, r).
cell(46,(5, 1), b, k).
cell(46,(5, 6), w, r).
cell(46,(3, 4), w, r).
cell(47,(4, 8), b, k).
cell(47,(8, 5), b, r).
cell(47,(2, 2), w, k).
cell(48,(6, 4), b, r).
cell(48,(1, 7), b, r).
cell(48,(7, 3), w, r).
cell(49,(4, 7), w, r).
cell(49,(7, 1), b, k).
cell(49,(7, 6), w, k).