head_pred(ans, 3).
body_pred(active, 1).
body_pred(inactive, 1).
body_pred(input1, 3).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:5
%% ans(A,B,C):- input1(A,D,B),input1(F,E,C),input1(F,D,B),inactive(E).
%% ******************************
%% python popper.py ./examples/sql-08  87.54s user 1.92s system 97% cpu 1:32.18 total
