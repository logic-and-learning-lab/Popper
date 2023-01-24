head_pred(out, 2).
body_pred(family, 4).

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:1 FN:0 TN:0 FP:0 Size:4
%% out(A,B):- family(C,B,E,D),family(C,F,E,D),family(A,F,E,D).
%% ******************************
%% python popper.py ./examples/sql-02  64.49s user 1.33s system 99% cpu 1:06.43 total
