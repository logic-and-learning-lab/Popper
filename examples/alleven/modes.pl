%% f(A) :- tail(A,C),head(C,D),last(A,B),even(B),even(D).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  2.61s user 0.04s system 99% cpu 2.649 total

max_vars(7).
max_body(7).
max_clauses(2).

modeh(f,1).
modeb(f,1).
modeb(tail,2).
modeb(head,2).
modeb(last,2).
modeb(empty,1).
modeb(zero,1).
modeb(even,1).

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