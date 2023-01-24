head_pred(out, 2).
body_pred(in, 4).
body_pred(not_null, 1).

max_body(4).
max_vars(5).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:8
%% out(A,B):- in(D,B,C,A),in(D,B,C,E),not_null(E).
%% out(A,B):- in(E,B,A,C),in(E,B,D,C),not_null(D).
%% ******************************
%% python popper.py ./examples/sql-04  37.53s user 0.99s system 98% cpu 39.252 total
