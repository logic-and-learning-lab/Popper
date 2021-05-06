%% python3 popper.py examples/robots-recursion
%% f(A,B) :- move_up(A,B),at_top(B).
%% f(A,B) :- move_up(A,C),f(C,B).
%% 1.51s user 0.06s system 99% cpu 1.583 total

max_vars(4).
max_body(3).
max_clauses(3).
enable_recursion.

head_pred(f,2).
body_pred(at_top,1).
body_pred(at_bottom,1).
body_pred(at_left,1).
body_pred(at_right,1).
body_pred(move_left,2).
body_pred(move_right,2).
body_pred(move_up,2).
body_pred(move_down,2).

direction(f,(in,out)).
direction(move_left,(in,out)).
direction(move_right,(in,out)).
direction(move_up,(in,out)).
direction(move_down,(in,out)).
direction(at_top,(in,)).
direction(at_bottom,(in,)).
direction(at_left,(in,)).
direction(at_right,(in,)).