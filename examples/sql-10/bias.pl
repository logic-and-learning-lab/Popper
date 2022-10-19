head_pred(out, 3).
body_pred(in, 4).
body_pred(lt, 2).

max_body(4).
max_vars(5).

allow_singletons.

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:3
%% out(A,B,C):- lt(B,C),in(D,A,E,C).
%% ******************************
%% python popper.py ./examples/sql-10  0.59s user 0.44s system 134% cpu 0.766 total

