max_body(15).
%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:50 FN:0 TN:50 FP:0 Size:19
%% zendo(A):- piece(A,C),rhs(C),contact(C,D),size(D,B),large(B).
%% zendo(A):- piece(A,C),contact(C,D),size(D,B),large(B),blue(D).
%% zendo(A):- piece(A,C),red(C),coord1(C,B),piece(A,D),blue(D),coord1(D,B).


head_pred(zendo,1).
body_pred(piece,2).
body_pred(contact,2).
body_pred(coord1,2).
body_pred(coord2,2).
body_pred(size,2).
body_pred(blue,1).
body_pred(green,1).
body_pred(red,1).
body_pred(small,1).
body_pred(medium,1).
body_pred(large,1).
body_pred(upright,1).
body_pred(lhs,1).
body_pred(rhs,1).
body_pred(strange,1).

type(zendo,(state,)).
type(piece,(state,piece)).
type(contact,(piece,piece)).
type(coord1,(piece,real)).
type(coord2,(piece,real)).
type(size,(piece,real)).
type(blue,(piece,)).
type(green,(piece,)).
type(red,(piece,)).
type(small,(real,)).
type(medium,(real,)).
type(large,(real,)).
type(upright,(piece,)).
type(lhs,(piece,)).
type(rhs,(piece,)).
type(strange,(piece,)).

direction(zendo,(in,)).
direction(piece,(in,out)).
direction(contact,(in,out)).
direction(coord1,(in,out)).
direction(coord2,(in,out)).
direction(size,(in,out)).
direction(blue,(in,)).
direction(green,(in,)).
direction(red,(in,)).
direction(small,(in,)).
direction(medium,(in,)).
direction(large,(in,)).
direction(upright,(in,)).
direction(lhs,(in,)).
direction(rhs,(in,)).
direction(strange,(in,)).