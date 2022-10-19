head_pred(reachablecast, 2).
body_pred(reachablem, 1).
body_pred(vh, 2).
body_pred(ht, 2).
body_pred(mcheckcastinst, 4).
body_pred(notSub, 2).

allow_singletons.

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:5 FN:0 TN:0 FP:0 Size:2
%% reachablecast(A,B):- mcheckcastinst(D,C,A,B).
%% ******************************
