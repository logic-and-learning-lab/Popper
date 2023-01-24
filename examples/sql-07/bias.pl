head_pred(out, 2).
body_pred(in, 3).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:5 FN:0 TN:0 FP:0 Size:4
%% out(A,B):- in(A,D,C),in(E,B,C),in(E,D,C).
%% ******************************
%% python popper.py ./examples/sql-07  2.43s user 0.80s system 123% cpu 2.621 total
