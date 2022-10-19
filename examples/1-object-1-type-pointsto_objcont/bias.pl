
head_pred(pointsto_objcont, 3).
body_pred(enclosing_type, 2).
body_pred(points_initial, 2).
body_pred(store, 3).
body_pred(load, 3).
body_pred(invocation, 2).
body_pred(receiver_actual, 2).
body_pred(receiver_formal, 2).
body_pred(formal, 3).
body_pred(actual, 3).
body_pred(assign, 6).
body_pred(pointsto, 4).
body_pred(heappointsto, 3).


allow_singletons.
enable_recursion.

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:2
%% pointsto_objcont(A,B,C):- pointsto(D,A,B,C).
%% ******************************
