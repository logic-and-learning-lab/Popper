head_pred(out, 3).
body_pred(child, 3).
body_pred(parent, 3).

max_body(4).
max_vars(4).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:9 FN:0 TN:0 FP:0 Size:5
%% out(A,B,C):- child(A,B,C).
%% out(A,B,C):- parent(C,B,D),parent(A,B,D).
%% ******************************
%% python popper.py ./examples/sql-06  0.85s user 0.60s system 136% cpu 1.067 total

