%% (base) ➜  filter popp exs.pl modes2.pl bk.pl
%% f(A,B) :- empty(B),empty(A).
%% f(A,B) :- even(C),cons2(C,E,A),f(E,D),cons1(C,D,B).
%% f(A,B) :- odd(C),cons2(C,D,A),f(D,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes2.pl bk.pl  38.87s user 0.27s system 99% cpu 39.157 total

max_vars(5).
max_body(4).
max_clauses(3).

:-
    body_literal(1,empty,_,_).
:-
    body_literal(2,empty,_,_).

modeh(f,2).
modeb(f,2).
modeb(empty,1).
modeb(odd,1).
modeb(even,1).
modeb(cons1,3).
modeb(cons2,3).

type(f,(list,list)).
type(empty,(list,)).
type(odd,(element,)).
type(even,(element,)).
type(cons1,(element,list,list)).
type(cons2,(element,list,list)).

direction(f,(in,out)).
direction(empty,(out,)).
direction(odd,(in,)).
direction(even,(in,)).
direction(cons1,(in,in,out)).
direction(cons2,(out,out,in)).

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
