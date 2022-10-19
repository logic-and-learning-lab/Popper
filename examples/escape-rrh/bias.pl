head_pred(rrh, 2).
body_pred(mmetharg, 3).
body_pred(mmethret, 3).
body_pred(vh, 2).
body_pred(hfh, 3).

allow_singletons.

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:3
%% rrh(A,B):- mmetharg(C,F,A),hfh(D,E,B).
%% ******************************
