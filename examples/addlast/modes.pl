%% (base) ➜  addlast time popp exs.pl bias.pl bk.pl
%% f(A,B) :- last(A,C),cons(C,A,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl bias.pl bk.pl  0.15s user 0.03s system 90% cpu 0.199 total


included_clause_fV0V1tailV0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,tail,2,(CV0,CV1)),CV1!=CV0.
included_program_fV0V1tailV0V1:-included_clause_fV0V1tailV0V1(C0).
%% (0.1) elimination constraint:
:-included_program_fV0V1tailV0V1,num_recursive(f,0).
%% (0.1) specialisation constraint:
:-included_clause_fV0V1tailV0V1(C0),C0 < 1,not clause(1).


max_vars(5).
max_body(5).
max_clauses(1).

modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(tail,2).
type(tail,0,list).
type(tail,1,list).
direction(tail,0,in).
direction(tail,1,out).

functional(tail,2).
modeb(head,2).
type(head,0,list).
type(head,1,element).
direction(head,0,in).
direction(head,1,out).

modeb(last,2).
type(last,0,list).
type(last,1,element).
direction(last,0,in).
direction(last,1,out).

modeb(length,2).
type(length,0,list).
type(length,1,int).
direction(length,0,in).
direction(length,1,out).

modeb(sum,3).
type(sum,0,int).
type(sum,1,int).
type(sum,2,int).
direction(sum,0,in).
direction(sum,1,in).
direction(sum,2,out).

modeb(cons,3).
type(cons,0,element).
type(cons,1,list).
type(cons,2,list).
direction(cons,0,in).
direction(cons,1,in).
direction(cons,2,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(zero,1).
type(zero,0,int).
direction(zero,0,in).