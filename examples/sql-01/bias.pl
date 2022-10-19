head_pred(ans, 1).
body_pred(le20130201, 1).
body_pred(ge20130215, 1).
body_pred(input1, 3).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:4
%% ans(A):- ge20130215(B),le20130201(C),input1(A,C,B).
%% ******************************
%% python popper.py ./examples/sql-01/  2.96s user 0.95s system 124% cpu 3.137 total

