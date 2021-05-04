%% python3 popper.py examples/iggp-minimal-decay
%% next_value(A,B) :- c5(B),does(A,E,D),does(C,E,D),my_true_value(C,B).
%% next_value(A,B) :- c_player(D),my_true_value(A,C),c_noop(E),my_succ(B,C),does(A,D,E).
%% 15.74s user 2.88s system 111% cpu 16.720 total

max_clauses(2).
max_vars(5).
max_body(5).

head_pred(next_value,2).
body_pred(does,3).
body_pred(my_true_value,2).
body_pred(my_succ,2).
body_pred(c_pressButton,1).
body_pred(c_noop,1).
body_pred(c_player,1). % comment to make unsat
%% body_pred(c1,1). % AC: I commented out to make the example faster
%% body_pred(c2,1). % AC: I commented out to make the example faster
%% body_pred(c3,1). % AC: I commented out to make the example faster
body_pred(c4,1).
body_pred(c5,1).

type(next_value,(ex,int)).
type(does,(ex,agent,action)).
type(my_true_value,(ex,int)).
type(my_succ,(int,int)).
type(c_pressButton,(action,)).
type(c_noop,(action,)).
type(c_player,(agent,)).
type(c1,(int,)).
type(c2,(int,)).
type(c3,(int,)).
type(c4,(int,)).
type(c5,(int,)).

functional(my_succ,2).
irreflexive(my_succ,2).
functional(my_true_value,2).
