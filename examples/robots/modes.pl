%% f(A,B) :- right(A,D),right(D,E),right(E,C),right(C,F),right(F,B).
%% python3 popper.py examples/robots/  35.02s user 0.11s system 99% cpu 35.219 total

max_vars(6).
max_body(6).
max_clauses(3).

modeh(f,2).
modeb(up,2).
modeb(down,2).
modeb(left,2).
modeb(right,2).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).