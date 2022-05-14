%% dcc paper
%% target is
%% f :- a, b.
%% f :- c, d.

%% overly specific solution is:
%% f :- x1.
%% f :- x2.
%% f :- x3.
%% f :- x4.

max_vars(1).
max_body(5).

head_pred(f,1).
body_pred(x0,1).
body_pred(a,1).
body_pred(b,1).
body_pred(c,1).
body_pred(d,1).
body_pred(x1,1).
body_pred(x2,1).
body_pred(x3,1).
body_pred(x4,1).

max_clauses(1).