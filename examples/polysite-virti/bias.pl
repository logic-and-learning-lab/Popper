head_pred(virti, 1).
body_pred(cicm, 3).
body_pred(virtim, 2).
body_pred(mneq, 2).

body_body(4).
max_vars(5).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:5
%% virti(A):- virtim(A,C),virtim(B,C),virtim(B,D),virtim(A,D).
%% ******************************
