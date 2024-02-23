%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%% ;;;
%% ;;;  Game Theory: Simplified War of Attrition (a variant of the Dollar Auction)
%% ;;;
%% ;;;  A two player game with alternating play. Each player starts with 16 points.
%% ;;;  On their turn, a player may either choose to end the game, or spend one point
%% ;;;  in order to "lay claim to the prize" (as long as they have a point to spend).
%% ;;;  When the game is ended, the player who last "laid claim to the prize" gets
%% ;;;  a prize consisting of five points.
%% ;;;
%% ;;;  Scores are equal to a player's total points multiplied by five, so that
%% ;;;  the maximum score a player can achieve is 100.
%% ;;;
%% ;;;  Background: http://en.wikipedia.org/wiki/Dollar_auction
%% ;;;              http://en.wikipedia.org/wiki/War_of_attrition_(game)
%% ;;;
%% ;;;  GDL BY: Sam Schreiber (schreib@cs.stanford.edu)
%% ;;;
%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


head_pred(next_score,3).
body_pred(does,3).
body_pred(my_true_claim_made_by,2).
body_pred(my_true_control,2).
body_pred(my_succ,2).
body_pred(my_true_gameOver,1).
body_pred(my_true_score,3).
body_pred(opponent,2).

type(next_score,(ex,agent,int)).
type(does,(ex,agent,action)).
type(my_true_claim_made_by,(ex,agent,)).
type(my_true_control,(ex,agent,)).
type(my_succ,(int,int)).
type(my_true_gameOver,(ex,)).
type(my_true_score,(ex,agent,int)).
type(opponent,(agent,agent)).

constant(lay_claim,action).
constant(end_game,action).
constant(noop,action).
constant(white,agent).
constant(black,agent).
constant(c5,int).
constant(c10,int).
constant(c15,int).
constant(c20,int).
constant(c25,int).
constant(c30,int).
constant(c35,int).
constant(c40,int).
constant(c45,int).
constant(c50,int).
constant(c55,int).
constant(c60,int).
constant(c65,int).
constant(c70,int).
constant(c75,int).
constant(c80,int).
constant(c85,int).
constant(c90,int).
constant(c95,int).
constant(c100,int).

body_pred(P,1):-
    constant(P,_).
type(P,(T,)):-
    constant(P,T).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : var_type(C,V,ex)} != 1.
