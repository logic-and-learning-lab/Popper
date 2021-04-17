%% f(A,B) :- right(A,D),right(D,E),right(E,C),right(C,F),right(F,B).
%% python3 popper.py examples/robots/  35.02s user 0.11s system 99% cpu 35.219 total

max_vars(6).
max_body(6).
max_clauses(3).

head_pred(f,2).
body_pred(up,2).
body_pred(down,2).
body_pred(left,2).
body_pred(right,2).

pred(P,A):-
    head_pred(P,A).
pred(P,A):-
    body_pred(P,A).
direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).