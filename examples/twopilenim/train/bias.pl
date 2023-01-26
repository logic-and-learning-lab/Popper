
max_body(9).
max_vars(8).

head_pred(f,2).
body_pred(cell_is_value,3).
body_pred(not_cell_is_value,3).
body_pred(shifted,3).
%body_pred(scanning,4).
%body_pred(at_action_cell,2).
body_pred(at_cell_with_value,2).
body_pred(up,1).
body_pred(right,1).
body_pred(left,1).
body_pred(down,1).
body_pred(downright,1).
body_pred(downleft,1).
body_pred(upright,1).
body_pred(upleft,1).

% cell_is_value V → C Check whether the attended cell has a given value
% shifted O × C→ C Shift attention by an offset, then check a condition
% scanning O × C × C→ C Repeatedly shift attention by the given offset, and check which of two conditions is satisfied first
% at action cell C→ P Attend to the action cell and check a condition
% at cell with value V × C→ P attend to a cell with the value and check condition



type(f,(board,position)).
type(not_cell_is_value,(board,position,value)).
type(cell_is_value,(board,position,value)).
type(shifted,(offset,position,position)).
type(scanning,(offset,cell,cell,cell)).
type(at_action_cell,(board,action)).
type(at_cell_with_value,(value,cell,action)).
type(up,(offset,)).
type(right,(offset,)).
type(left,(offset,)).
type(down,(offset,)).
type(upright,(offset,)).
type(downright,(offset,)).
type(upleft,(offset,)).
type(downleft,(offset,)).

direction(f,(in,in)).
direction(not_cell_is_value,(in,in,in)).
direction(cell_is_value,(in,in,in)).
direction(shifted,(in,in,out)).
direction(scanning,(in,in,in,out)).
direction(at_action_cell,(in,out)).
direction(at_cell_with_value,(in,in,out)).
direction(up,(out,)).
direction(right,(out,)).
direction(left,(out,)).
direction(down,(out,)).
direction(upright,(out,)).
direction(downright,(out,)).
direction(upleft,(out,)).
direction(downleft,(out,)).


body_pred(P,1):-
    constant(P,_).
type(P,(T,)):-
    constant(P,T).
direction(P,(out,)):-
    constant(P,_).



constant(token,value).
constant(empty,value).
