head_pred(ans, 3).
body_pred(treat, 1).
body_pred(untreat, 1).
body_pred(input1, 3).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:1 FN:0 TN:0 FP:0 Size:2
%% ans(A,B,C):- input1(A,B,C).
%% ******************************
%% python popper.py ./examples/sql-09  0.48s user 0.64s system 165% cpu 0.680 total

