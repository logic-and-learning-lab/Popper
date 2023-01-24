head_pred(ans, 1).
body_pred(junior, 1).
body_pred(focus, 1).
body_pred(class, 2).
body_pred(enroll, 2).
body_pred(faculty, 2).
body_pred(student, 3).


%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:0 FP:0 Size:5
%% ans(A):- student(B,C,F),student(D,A,E),student(D,A,F),student(B,C,E).
%% ******************************
%% python popper.py ./examples/sql-11  147.07s user 1.76s system 93% cpu 2:38.68 total
