
% WITH ADDITIONAL BIAS
% BEST PROG 67:
f(A):-cell(A,F,B,C),cell(A,E,B,D),distance(F,E,G),king(D),rook(C),one(G),white(B).
% Precision:1.00, Recall:1.00, TP:4, FN:0, TN:10, FP:0
% real	1m18.319s user	1m18.172s sys	0m0.129s

% WITHOUT ADDITIONAL BIAS
% BEST PROG 2039:
% f(A):-cell(A,D,B,C),white(B),king(C),cell(A,F,B,G),rook(G),distance(D,F,E),one(E).
% Precision:1.00, Recall:1.00, TP:4, FN:0, TN:10, FP:0
% real	74m32.151s user 73m0.988s sys	1m28.636s



max_vars(8).
max_body(9).
max_clauses(1).

head_pred(f,1).
body_pred(distance,3).
body_pred(cell,4).
body_pred(king,1).
body_pred(rook,1).
body_pred(white,1).
body_pred(black,1).
body_pred(one,1).


type(f,(state,)).
type(distance,(pos, pos, integer)).
type(cell,(state, pos, color, piecetype)).
type(king,(piecetype,)).
type(rook,(piecetype,)).
type(white,(color,)).
type(black,(color,)).
type(one,(integer,)).

direction(f,(in,)).
direction(distance,(in, in, out)).
direction(cell,(in, out, out, out)).
direction(king,(in,)).
direction(rook,(in,)).
direction(white,(in,)).
direction(black,(in,)).
direction(one,(in,)).

%% ADDITIONAL BIAS TO REDUCE LEARNING TIME
at_most_twice(king).
at_most_twice(rook).
at_most_twice(white).
at_most_twice(black).
at_most_twice(cell).
:-
    at_most_twice(P),
    clause(C),
    #count{Vars : body_literal(C,P,A,Vars)} > 2.
