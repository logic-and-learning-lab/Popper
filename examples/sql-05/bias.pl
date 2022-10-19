head_pred(ans, 1).
body_pred(input1, 2).
body_pred(input2, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:5 FN:0 TN:0 FP:0 Size:5
%% ans(A):- input2(B,C),input2(D,A),input2(B,A),input2(D,C).
%% ******************************
%% python popper.py ./examples/sql-05  1.94s user 0.59s system 117% cpu 2.163 total

