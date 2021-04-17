%% f(A) :- tail(A,C),head(C,D),last(A,B),even(B),even(D).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  2.61s user 0.04s system 99% cpu 2.649 total

max_vars(7).
max_body(7).
max_clauses(2).

head_pred(f,1).
body_pred(f,1).
body_pred(tail,2).
body_pred(head,2).
body_pred(last,2).
body_pred(empty,1).
body_pred(zero,1).
body_pred(even,1).

type(f,(list,)).
type(tail,(list,list)).
type(head,(list,element)).
type(last,(list,element)).
type(empty,(list,)).
type(zero,(element,)).
type(even,(element,)).

direction(f,(in,)).
direction(tail,(in,out)).
direction(head,(in,out)).
direction(last,(in,out)).
direction(empty,(in,)).
direction(zero,(in,)).
direction(even,(in,)).