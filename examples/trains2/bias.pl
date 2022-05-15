%% S f(A):- has_car(A,B),has_car(A,C),roof_closed(C),two_wheels(B),roof_open(B).
%% S f(A):- has_car(A,B),has_load(B,C),triangle(C),roof_open(B).


max_vars(6).
max_body(6).
max_clauses(1).

head_pred(f,1).
body_pred(has_car,2).
body_pred(has_load,2).
body_pred(short,1).
body_pred(long,1).
body_pred(two_wheels,1).
body_pred(three_wheels,1).
body_pred(roof_open,1).
body_pred(roof_closed,1).
body_pred(zero_load,1).
body_pred(one_load,1).
body_pred(two_load,1).
body_pred(three_load,1).
body_pred(circle,1).
body_pred(triangle,1).
body_pred(rectangle,1).

type(f,(train,)).
type(has_car,(train,car)).
type(has_load,(car,load)).
type(short,(car,)).
type(long,(car,)).
type(two_wheels,(car,)).
type(three_wheels,(car,)).
type(roof_open,(car,)).
type(roof_closed,(car,)).
type(zero_load,(load,)).
type(one_load,(load,)).
type(two_load,(load,)).
type(three_load,(load,)).
type(circle,(load,)).
type(triangle,(load,)).
type(rectangle,(load,)).

direction(f,(in,)).
direction(has_car,(in,out)).
direction(has_load,(in,out)).
direction(short,(in,)).
direction(long,(in,)).
direction(two_wheels,(in,)).
direction(three_wheels,(in,)).
direction(roof_open,(in,)).
direction(roof_closed,(in,)).
direction(zero_load,(in,)).
direction(one_load,(in,)).
direction(two_load,(in,)).
direction(three_load,(in,)).
direction(circle,(in,)).
direction(triangle,(in,)).
direction(rectangle,(in,)).

prop(pre_postcon,(long,has_load,triangle)).
prop(pre_postcon,(short,has_load,three_load)).
prop(pre_postcon,(short,has_load,zero_load)).
prop(pre_postcon,(three_wheels,has_load,triangle)).
prop(unique_b_a,has_car).
prop(unique_b_a,has_load).
prop(unsat_pair,rectangle,circle).
prop(unsat_pair,roof_open,roof_closed).
prop(unsat_pair,short,long).
prop(unsat_pair,three_load,one_load).
prop(unsat_pair,three_wheels,short).
prop(unsat_pair,triangle,circle).
prop(unsat_pair,triangle,rectangle).
prop(unsat_pair,triangle,three_load).
prop(unsat_pair,two_load,one_load).
prop(unsat_pair,two_load,three_load).
prop(unsat_pair,two_wheels,three_wheels).
prop(unsat_pair,zero_load,one_load).
prop(unsat_pair,zero_load,three_load).
prop(unsat_pair,zero_load,triangle).
prop(unsat_pair,zero_load,two_load).
