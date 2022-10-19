head_pred(out, 2).
body_pred(in, 4).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:4
%% out(A,B):- in(D,F,E,C),in(D,F,A,C),in(D,F,E,B).
%% ******************************
%% python popper.py ./examples/sql-03  99.17s user 1.87s system 97% cpu 1:44.02 total
