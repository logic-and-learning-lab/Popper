%% dcc paper
%% target is
%% f :- x1, x2, x3, x4.

%% overly specific solution is:
%% f :- a.
%% f :- b.
%% f :- c.
%% f :- d.

max_vars(1).
max_body(5).

head_pred(f,1).
body_pred(a,1).
body_pred(b,1).
body_pred(c,1).
body_pred(d,1).
body_pred(x1,1).
body_pred(x2,1).
body_pred(x3,1).
body_pred(x4,1).