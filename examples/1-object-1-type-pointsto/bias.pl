
head_pred(pointsto, 4).
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
body_pred(pointsto_objcont, 3).
body_pred(heappointsto, 3).


allow_singletons.
enable_recursion.

max_body(4).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:0.73 TP:8 FN:3 TN:0 FP:0 Size:3
%% pointsto(A,B,C,D):- pointsto_objcont(B,E,D),enclosing_type(C,A).
%% ******************************
