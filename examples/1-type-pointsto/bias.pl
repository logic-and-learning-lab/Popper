head_pred(pointsto, 3).
body_pred(enclosing_type, 2).
body_pred(points_initial, 2).
body_pred(store, 3).
body_pred(load, 3).
body_pred(invocation, 2).
body_pred(receiver_actual, 2).
body_pred(receiver_formal, 2).
body_pred(formal, 3).
body_pred(actual, 3).
body_pred(assign, 4).


allow_singletons.
enable_recursion.

%% Precision:1.00 Recall:1.00 TP:10 FN:0 TN:0 FP:0 Size:7
%% pointsto(A,B,C):- points_initial(D,C),enclosing_type(B,A).
%% pointsto(A,B,C):- enclosing_type(E,A),load(F,D,B),points_initial(E,C).
