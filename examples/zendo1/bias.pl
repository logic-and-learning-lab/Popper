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



:-
    clause(Rule),
    #count{A : body_literal(Rule,contact,_,(A,B))} > 2.

:-
    clause(Rule),
    #count{B : body_literal(Rule,contact,_,(A,B))} > 2.

%% ('blue', '0') 58
%% ('contact', '00') 60
%% ('contact', '01') 2
%% ('contact', '10') 2
%% ('coord1', '00') 139
%% ('coord1', '01') 15
%% ('coord1', '10') 1
%% ('coord2', '00') 139
%% ('coord2', '01') 22
%% ('coord2', '10') 1
%% ('green', '0') 34
%% ('large', '0') 4
%% ('lhs', '0') 35
%% ('medium', '0') 3
%% ('piece', '00') 139
%% ('piece', '01') 1
%% ('piece', '10') 5
%% ('red', '0') 47
%% ('rhs', '0') 39
%% ('size', '00') 139
%% ('size', '01') 20
%% ('size', '10') 1
%% ('small', '0') 4
%% ('strange', '0') 31
%% ('upright', '0') 34
