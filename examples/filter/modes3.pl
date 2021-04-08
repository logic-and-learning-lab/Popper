%% (base) ➜  filter popp exs.pl modes3.pl bk.pl
%% f(A,B) :- empty(B),empty(A).
%% f(A,B) :- cons2(D,E,A),even(D),f(E,C),cons1(D,C,B).
%% f(A,B) :- cons2(C,D,A),odd(C),f(D,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes3.pl bk.pl  199.41s user 0.45s system 100% cpu 3:19.84 total

max_vars(5).
max_body(4).
max_clauses(3).

:-
    body_literal(1,empty,_,_).
%% :-
    %% body_literal(2,empty,_,_).

modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).
modeb(f,2).

modeb(odd,1).
type(odd,0,element).
direction(odd,1,in).

modeb(even,1).
type(even,0,element).
direction(even,1,in).

modeb(cons1,3).
type(cons1,0,element).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,in).
direction(cons1,1,in).
direction(cons1,2,out).

modeb(cons2,3).
type(cons2,0,element).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,out).
direction(cons2,1,out).
direction(cons2,2,in).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,out).

same(cons1,cons2).

:-
    body_literal(C,P,_,(H1,_,L1)),
    body_literal(C,Q,_,(H2,_,L1)),
    H1 != H2,
    same(P,Q).
:-
    body_literal(C,P,_,(_,T1,L1)),
    body_literal(C,Q,_,(_,T2,L1)),
    T1 != T2,
    same(P,Q).

:-
    body_literal(C,P,_,(H,T,L1)),
    body_literal(C,Q,_,(H,T,L2)),
    L1 != L2,
    same(P,Q).

:-
    body_literal(C,P,_,(_,T,L)),
    body_literal(C,Q,_,(_,L,T)),
    same(P,Q).

:-
    body_literal(C,P,_,Vars),
    body_literal(C,Q,_,Vars),
    same(P,Q).

only_once(cons1).
only_once(cons2).

:-
    only_once(P),
    clause(C),
    #count{Vars : body_literal(C,P,A,Vars)} > 1.
