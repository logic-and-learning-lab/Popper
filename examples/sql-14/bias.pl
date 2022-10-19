head_pred(ans, 4).
body_pred(active, 1).
body_pred(correct, 1).
body_pred(input1, 4).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:2
%% ans(A,B,C,D):- input1(A,B,C,D).
%% ******************************
%% python popper.py ./examples/sql-14  0.52s user 0.58s system 158% cpu 0.692 total

