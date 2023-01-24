head_pred(ans, 1).
body_pred(input1, 2).
body_pred(isred, 1).
body_pred(isgreen, 1).
body_pred(input2, 2).
body_pred(input3, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:7 FN:0 TN:0 FP:0 Size:5
%% ans(A):- input3(D,C),input3(B,A),input3(B,C),input3(D,A).
%% ******************************
%% python popper.py ./examples/sql-12  8.34s user 0.88s system 98% cpu 9.333 total

