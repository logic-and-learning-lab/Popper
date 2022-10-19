head_pred(polysite, 1).
body_pred(cicm, 3).
body_pred(virtim, 2).
body_pred(mneq, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:5
%% polysite(A):- cicm(B,A,D),cicm(B,A,C),mneq(C,D).
%% ******************************

