%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

constant(agent_player, agent).
constant(score_0, score).
constant(score_25, score).
constant(score_50, score).
constant(score_75, score).
constant(score_100, score).
constant(int_1, int).
constant(int_2, int).
constant(int_3, int).
constant(int_4, int).
constant(int_5, int).
constant(int_6, int).
constant(int_7, int).
constant(int_8, int).
constant(int_9, int).
constant(int_10, int).
constant(int_11, int).
constant(int_12, int).
constant(int_13, int).
constant(int_14, int).
constant(int_15, int).
constant(int_16, int).
constant(int_17, int).
constant(int_18, int).
constant(int_19, int).
constant(int_20, int).
constant(int_21, int).
constant(int_22, int).
constant(int_23, int).
constant(int_24, int).
constant(int_26, int).
constant(int_27, int).
constant(int_28, int).
constant(int_29, int).
constant(int_30, int).
constant(int_31, int).
constant(word_fizz, word).
constant(word_buzz, word).
constant(word_fizzbuzz, word).
head_pred(goal,3).
body_pred(int,1).
body_pred(true_count,2).
body_pred(true_success,2).
body_pred(input_say,2).
body_pred(role,1).
body_pred(divisible,2).
body_pred(less_than,2).
body_pred(succ,2).
body_pred(minus,3).
type(true_count,(ex,int)).
type(true_success,(ex,int)).
type(input_say,(agent,word)).
type(goal,(ex,agent,score)).
type(role,(agent,)).
type(divisible,(int,int)).
type(less_than,(int,int)).
type(succ,(int,int)).
type(minus,(int,int,int)).
type(int,(int,)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).