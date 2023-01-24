head_pred(ans, 1).
body_pred(class, 2).
body_pred(lt, 2).
body_pred(enroll, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:7 FN:0 TN:0 FP:0 Size:4
%% ans(A):- enroll(A,B),enroll(A,C),lt(B,C).
%% ******************************
%% python popper.py ./examples/sql-15  0.72s user 0.63s system 148% cpu 0.910 total

