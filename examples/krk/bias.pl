% BEST PROG 559:
%% f(A):-cell(A,G,E,C),white(E),cell(A,F,E,D),rook(C),king(D),distance(F,G,B),one(B).
% Precision:1.00, Recall:1.00, TP:4, FN:0, TN:10, FP:0
%% Total execution time: 41.29s

max_vars(7).
max_body(7).
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