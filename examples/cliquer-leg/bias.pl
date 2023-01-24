
head_pred(leg, 2).
body_pred(edge, 2).

max_body(3).

%solution
%leg(X,Z) :- edge(X,Y), edge(Y,Z).