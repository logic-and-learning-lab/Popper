%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%% ;;;
%% ;;;  Game Theory: The Centipede Game
%% ;;;
%% ;;;  A two player game with alternating play in which, in each round,
%% ;;;  each player has the option to either continue or finish. If a player
%% ;;;  finishes, the game ends immediately. Otherwise, the game continues.
%% ;;;  The payoffs are constructed so that the decisions are as follows:
%% ;;;
%% ;;;  Let [x,y] denote a payoff of 'x' to White and 'y' to Black.
%% ;;;
%% ;;;  1. White decides whether to finish at [5,0] or continue.
%% ;;;  2. Black decides whether to finish at [0,15] or continue.
%% ;;;  3. White decides whether to finish at [15,10] or continue.
%% ;;;  4. Black decides whether to finish at [10,25] or continue.
%% ;;;     (etc)
%% ;;;  17. White decides whether to finish at [85,80] or continue.
%% ;;;  18. Black decides whether to finish at [80,95] or continue.
%% ;;;  19. The game finishes at [95,90].
%% ;;;
%% ;;;  Conventional game theory suggests that rational players will finish
%% ;;;  the game immediately. To see why this is the case, consider Black's
%% ;;;  final move: a decision between 95 points and 90 points. A rational
%% ;;;  player will finish rather than continue. Assuming that Black will
%% ;;;  finish on move 18, then by similar logic White will finish on move 17,
%% ;;;  to get 85 points rather than 80, and so on, until White must logically
%% ;;;  choose to finish on move 1.
%% ;;;
%% ;;;  Background: http://en.wikipedia.org/wiki/Centipede_game
%% ;;;
%% ;;;  GDL BY: Sam Schreiber (schreib@cs.stanford.edu)
%% ;;;
%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


max_body(7).
max_vars(7).
constant(agent_white, agent).
constant(agent_black, agent).
constant(int_0, int).
constant(int_5, int).
constant(int_10, int).
constant(int_15, int).
constant(int_20, int).
constant(int_25, int).
constant(int_30, int).
constant(int_35, int).
constant(int_40, int).
constant(int_45, int).
constant(int_50, int).
constant(int_55, int).
constant(int_60, int).
constant(int_65, int).
constant(int_70, int).
constant(int_75, int).
constant(int_80, int).
constant(int_85, int).
constant(int_90, int).
constant(int_95, int).
constant(int_100, int).
constant(action_finish, action).
constant(action_continue, action).
constant(action_noop, action).
constant(prop_gameOver, prop).
head_pred(goal,3).
body_pred(true_whitePayoff,2).
body_pred(true_blackPayoff,2).
body_pred(true_control,2).
body_pred(role,1).
body_pred(succ,2).
type(true_whitePayoff,(ex,int)).
type(true_blackPayoff,(ex,int)).
type(true_control,(ex,agent)).
type(goal,(ex,agent,int)).
type(role,(agent,)).
type(succ,(int,int)).

:-
	clause(C),
	#count{V : var_type(C,V,ex)} != 1.

body_pred(P,1):-
	constant(P,_).

type(P,(T,)):-
	constant(P,T).