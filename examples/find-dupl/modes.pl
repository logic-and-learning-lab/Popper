modeh(f,2).
type(f,0,list).
type(f,1,element).
direction(f,0,in).
direction(f,1,out).
modeb(f,2).

%% ESSENTIAL
modeb(element,2).
% ALL MODES
max_vars(5).
max_body(5).
max_clauses(2).

modeb(head,2).
modeb(tail,2).
modeb(geq,2).
modeb(empty,1).
modeb(even,1).
modeb(odd,1).
modeb(one,1).
modeb(zero,1).

modeb(decrement,2).
%% modeb(increment,2). % METAGOL CRASHES WHEN GIVEN
%% modeb(element,2). % CANNOT USE IN EVERYTHING BECAUSE OF MEMBER PROBLEM

type(cons,0,element).
type(cons,1,list).
type(cons,2,list).
direction(cons,0,in).
direction(cons,1,in).
direction(cons,2,out).

type(head,0,list).
type(head,1,element).
direction(head,0,in).
direction(head,1,out).

type(tail,0,list).
type(tail,1,list).
direction(tail,0,in).
direction(tail,1,out).

type(empty,0,list).
direction(empty,0,in).

type(element,0,list).
type(element,1,element).
direction(element,0,in).
direction(element,1,out).

type(increment,0,element).
type(increment,1,element).
direction(increment,0,in).
direction(increment,1,out).

type(decrement,0,element).
type(decrement,1,element).
direction(decrement,0,in).
direction(decrement,1,out).

type(geq,0,element).
type(geq,1,element).
direction(geq,0,in).
direction(geq,1,in).

type(even,0,element).
direction(even,0,in).

type(odd,0,element).
direction(odd,0,in).

type(one,0,element).
direction(one,0,in).

type(zero,0,element).
direction(zero,0,out).