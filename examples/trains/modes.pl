max_clauses(4).
max_vars(5).
max_body(5).

modeh(f,1).
modeb(has_car,2).
modeb(has_load,2).
modeb(short,1).
modeb(long,1).
modeb(two_wheels,1).
modeb(three_wheels,1).
modeb(roof_open,1).
modeb(roof_closed,1).
modeb(zero_load,1).
modeb(one_load,1).
modeb(two_load,1).
modeb(circle,1).
modeb(triangle,1).
modeb(rectangle,1).

type(f,(train,)).
type(has_car,(train,car)).
type(has_load,(train,load)).
type(short,(car,)).
type(long,(car,)).
type(two_wheels,(car,)).
type(three_wheels,(car,)).
type(roof_open,(car,)).
type(roof_closed,(car,)).
type(zero_load,(car,)).
type(one_load,(car,)).
type(two_load,(car,)).
type(circle,(load,)).
type(triangle,(load,)).
type(rectangle,(load,)).

%% direction(f,(in,)).
%% direction(has_car,(in,out)).
%% direction(has_load,(in,out)).
%% direction(short,(in,)).
%% direction(long,(in,)).
%% direction(two_wheels,(in,)).
%% direction(three_wheels,(in,)).
%% direction(roof_open,(in,)).
%% direction(roof_closed,(in,)).
%% direction(zero_load,(in,)).
%% direction(one_load,(in,)).
%% direction(two_load,(in,)).
%% direction(circle,(in,)).
%% direction(triangle,(in,)).
%% direction(rectangle,(in,)).