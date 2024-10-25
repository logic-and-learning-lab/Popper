%% From the paper: Céline Hocquette, Andrew Cropper: Relational Program Synthesis with Numerical Reasoning. AAAI 2023: 6425-6433

%% a zendo structure must follow the rule:
%% zendo(V0):- piece(V0,V2),size(V2,V1),small(V1),contact(V2,V3),red(V3).

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

% directions specify which are arguments are input and which are output
% monadic predicates have a single input argument: they hold if a property is true given an input
% the dyadic predicates size/2, coord1/2, and coord2/2 have an input and an output: they return as output
% an attribute of the input
% the dyadic predicates piece/2, and contact/2 have an input and an output: they return as output
% an object in relation with the input

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
    clause(C),
    #count{V : var_type(C,V,state)} != 1.
