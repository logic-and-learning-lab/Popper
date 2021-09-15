%% BEST PROGRAM:
%% f(A):-has_car(A,B),roof_open(C),roof_closed(B),has_car(A,C)
%% f(A):-three_load(B),roof_open(C),has_car(A,C),has_load(C,B)
%% f(A):-has_load(C,B),triangle(D),has_car(A,E),rectangle(B),has_load(E,D),has_car(A,C)
%% TP: 792, FN: 0, TN: 208, FP: 0

%% Total programs: 1274
%% Generate:
%%     Called: 1290 times   Total: 11.54    Mean: 0.009     Max: 0.249
%% Test:
%%     Called: 1274 times   Total: 14.05    Mean: 0.011     Max: 0.053
%% Build_Rules:
%%     Called: 1273 times   Total: 2.29     Mean: 0.002     Max: 0.017
%% Ground:
%%     Called: 1273 times   Total: 0.69     Mean: 0.001     Max: 0.006
%% Add:
%%     Called: 1273 times   Total: 2.26     Mean: 0.002     Max: 0.004
%% Test_Individual_Rules.Is_Totally_Incomplete:
%%     Called: 1259 times   Total: 0.06     Mean: 0.000     Max: 0.002
%% Total operation time: 30.89s
%% Total execution time: 31.55s


max_clauses(3).
max_vars(5).
max_body(6).

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