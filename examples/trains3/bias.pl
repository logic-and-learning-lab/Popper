max_body(10).
max_vars(7).

head_pred(f,1).
body_pred(has_car,2).
body_pred(has_load,2).
body_pred(short,1).
body_pred(long,1).
body_pred(two_wheels,1).
body_pred(three_wheels,1).
body_pred(roof_open,1).
body_pred(roof_flat,1).
body_pred(roof_closed,1).
body_pred(zero_load,1).
body_pred(one_load,1).
body_pred(two_load,1).
body_pred(three_load,1).
body_pred(circle,1).
body_pred(triangle,1).
body_pred(rectangle,1).
body_pred(diamond,1).
body_pred(hexagon,1).
body_pred(inverted_triangle,1).

type(f,(train,)).
type(has_car,(train,car)).
type(has_load,(car,load)).
type(short,(car,)).
type(long,(car,)).
type(two_wheels,(car,)).
type(three_wheels,(car,)).
type(roof_open,(car,)).
type(roof_flat,(car,)).
type(roof_closed,(car,)).
type(zero_load,(load,)).
type(one_load,(load,)).
type(two_load,(load,)).
type(three_load,(load,)).
type(circle,(load,)).
type(triangle,(load,)).
type(rectangle,(load,)).
type(diamond,(load,)).
type(hexagon,(load,)).
type(inverted_triangle,(load,)).

direction(f,(in,)).
direction(has_car,(in,out)).
direction(has_load,(in,out)).
direction(short,(in,)).
direction(long,(in,)).
direction(two_wheels,(in,)).
direction(three_wheels,(in,)).
direction(roof_open,(in,)).
direction(roof_flat,(in,)).
direction(roof_closed,(in,)).
direction(zero_load,(in,)).
direction(one_load,(in,)).
direction(two_load,(in,)).
direction(three_load,(in,)).
direction(circle,(in,)).
direction(triangle,(in,)).
direction(rectangle,(in,)).
direction(diamond,(in,)).
direction(hexagon,(in,)).
direction(inverted_triangle,(in,)).






:-
    clause(Rule),
    #count{B : body_literal(Rule,has_car,_,(A,B))} > 4.

:-
    clause(Rule),
    #count{B : body_literal(Rule,has_load,_,(A,B))} > 3.

%% ('circle', '0') 1099
%% ('diamond', '0') 589
%% ('has_car', '00') 3010
%% ('has_car', '01') 1
%% ('has_car', '10') 4
%% ('has_load', '00') 4550
%% ('has_load', '01') 1
%% ('has_load', '10') 3
%% ('hexagon', '0') 608
%% ('inverted_triangle', '0') 580
%% ('long', '0') 1508
%% ('one_load', '0') 1745
%% ('rectangle', '0') 1090
%% ('roof_closed', '0') 1812
%% ('roof_flat', '0') 893
%% ('roof_open', '0') 1198
%% ('short', '0') 1503
%% ('three_load', '0') 584
%% ('three_wheels', '0') 771
%% ('triangle', '0') 584
%% ('two_load', '0') 1657
%% ('two_wheels', '0') 2240
%% ('zero_load', '0') 564
