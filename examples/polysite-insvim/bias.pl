head_pred(insvim, 2).
body_pred(cicm, 3).
body_pred(virtim, 2).
body_pred(mneq, 2).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:19 FN:0 TN:0 FP:0 Size:4
%% insvim(A,B):- cicm(C,A,B),virtim(E,D),cicm(C,E,D).
%% ******************************
