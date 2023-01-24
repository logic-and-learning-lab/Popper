head_pred(out, 2).
body_pred(a, 2).
body_pred(b, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:7 FN:0 TN:0 FP:0 Size:3
%% out(A,B):- b(B,C),a(A,C).
%% ******************************
%% python popper.py ./examples/sql-13  0.45s user 0.52s system 143% cpu 0.676 total
