max_body(10).

head_pred(rhh, 2).
body_pred(mmetharg, 3).
body_pred(mmethret, 3).
body_pred(vh, 2).
body_pred(hfh, 3).

%allow_singletons.

%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:6 FN:0 TN:0 FP:0 Size:3
%% rhh(A,B):- hfh(E,F,B),hfh(A,C,D).
%% ******************************

%% :-
%%     clause(Rule),
%%     #count{B : body_literal(Rule,mmetharg,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{B : body_literal(Rule,mmetharg,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{A,B : body_literal(Rule,hfh,_,(A,B,C))} > 2.

%% :-
%%     clause(Rule),
%%     #count{A,C : body_literal(Rule,hfh,_,(A,B,C))} > 2.
%% :-
%%     clause(Rule),
%%     #count{B,C : body_literal(Rule,hfh,_,(A,B,C))} > 2.


%% ('hfh', '001') 2
%% ('hfh', '010') 2
%% ('hfh', '100') 2

%% ('vh', '00') 4
%% ('vh', '01') 1
%% ('vh', '10') 1
