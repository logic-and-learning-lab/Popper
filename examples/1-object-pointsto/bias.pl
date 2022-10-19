head_pred(pointsto, 3).
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

max_body(3).

%% Precision:1.00 Recall:1.00 TP:9 FN:0 TN:0 FP:0 Size:9
%% pointsto(A,B,C):- points_initial(B,C),receiver_actual(D,A).
%% pointsto(A,B,C):- assign(A,D,F,B),invocation(C,E).
%% pointsto(A,B,C):- assign(A,B,D,E),invocation(C,F).
