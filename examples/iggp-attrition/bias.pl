max_vars(6).
max_body(5).

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
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.

:- body_literal(Rule,opponent,_,(V0,V1)), V0 > V1.

%% prop(asymmetric_ab_ba,my_succ).
%% prop(antitriangular,my_succ).
%% prop(antitransitive,my_succ).
%% prop(unique_b_a,my_succ).
%% prop(unique_a_b,my_succ).

%% prop(unique_b_a,opponent).
%% prop(unique_a_b,opponent).
%% prop(antitriangular,opponent).
%% prop(antitransitive,opponent).

%% prop(unique_ac_b,does).
%% prop(unique_ab_c,does).
%% prop(unique_ab_c,my_true_score).
%% prop(unique_a_b,my_true_claim_made_by).
%% prop(unique_a_b,my_true_control).

%% prop(unsat_pair,my_true_control,my_true_claim_made_by).

%% %% ==========

%% prop(singleton,lay_claim).
%% prop(singleton,end_game).
%% prop(singleton,noop).
%% prop(singleton,white).
%% prop(singleton,black).
%% prop(singleton,c5).
%% prop(singleton,c10).
%% prop(singleton,c15).
%% prop(singleton,c20).
%% prop(singleton,c25).
%% prop(singleton,c30).
%% prop(singleton,c35).
%% prop(singleton,c40).
%% prop(singleton,c45).
%% prop(singleton,c50).
%% prop(singleton,c55).
%% prop(singleton,c60).
%% prop(singleton,c65).
%% prop(singleton,c70).
%% prop(singleton,c75).
%% prop(singleton,c80).
%% prop(singleton,c85).
%% prop(singleton,c90).
%% prop(singleton,c95).
%% prop(singleton,c100).

%% prop(unsat_pair,noop,lay_claim).
%% prop(unsat_pair,lay_claim,end_game).
%% prop(unsat_pair,noop,end_game).
%% prop(unsat_pair,white,black).
%% prop(unsat_pair,c50,c5).
%% prop(unsat_pair,c55,c5).
%% prop(unsat_pair,c60,c5).
%% prop(unsat_pair,c65,c5).
%% prop(unsat_pair,c70,c5).
%% prop(unsat_pair,c75,c5).
%% prop(unsat_pair,c80,c5).
%% prop(unsat_pair,c85,c5).
%% prop(unsat_pair,c90,c5).
%% prop(unsat_pair,c95,c5).
%% prop(unsat_pair,c5,c10).
%% prop(unsat_pair,c15,c10).
%% prop(unsat_pair,c20,c10).
%% prop(unsat_pair,c25,c10).
%% prop(unsat_pair,c30,c10).
%% prop(unsat_pair,c35,c10).
%% prop(unsat_pair,c40,c10).
%% prop(unsat_pair,c45,c10).
%% prop(unsat_pair,c50,c10).
%% prop(unsat_pair,c55,c10).
%% prop(unsat_pair,c60,c10).
%% prop(unsat_pair,c65,c10).
%% prop(unsat_pair,c70,c10).
%% prop(unsat_pair,c75,c10).
%% prop(unsat_pair,c80,c10).
%% prop(unsat_pair,c85,c10).
%% prop(unsat_pair,c90,c10).
%% prop(unsat_pair,c95,c10).
%% prop(unsat_pair,c100,c10).
%% prop(unsat_pair,c5,c15).
%% prop(unsat_pair,c20,c15).
%% prop(unsat_pair,c25,c15).
%% prop(unsat_pair,c30,c15).
%% prop(unsat_pair,c35,c15).
%% prop(unsat_pair,c40,c15).
%% prop(unsat_pair,c45,c15).
%% prop(unsat_pair,c50,c15).
%% prop(unsat_pair,c55,c15).
%% prop(unsat_pair,c60,c15).
%% prop(unsat_pair,c65,c15).
%% prop(unsat_pair,c70,c15).
%% prop(unsat_pair,c75,c15).
%% prop(unsat_pair,c80,c15).
%% prop(unsat_pair,c85,c15).
%% prop(unsat_pair,c90,c15).
%% prop(unsat_pair,c95,c15).
%% prop(unsat_pair,c5,c20).
%% prop(unsat_pair,c25,c20).
%% prop(unsat_pair,c30,c20).
%% prop(unsat_pair,c35,c20).
%% prop(unsat_pair,c40,c20).
%% prop(unsat_pair,c45,c20).
%% prop(unsat_pair,c50,c20).
%% prop(unsat_pair,c55,c20).
%% prop(unsat_pair,c60,c20).
%% prop(unsat_pair,c65,c20).
%% prop(unsat_pair,c70,c20).
%% prop(unsat_pair,c75,c20).
%% prop(unsat_pair,c80,c20).
%% prop(unsat_pair,c85,c20).
%% prop(unsat_pair,c90,c20).
%% prop(unsat_pair,c95,c20).
%% prop(unsat_pair,c5,c25).
%% prop(unsat_pair,c30,c25).
%% prop(unsat_pair,c35,c25).
%% prop(unsat_pair,c40,c25).
%% prop(unsat_pair,c45,c25).
%% prop(unsat_pair,c50,c25).
%% prop(unsat_pair,c55,c25).
%% prop(unsat_pair,c60,c25).
%% prop(unsat_pair,c65,c25).
%% prop(unsat_pair,c70,c25).
%% prop(unsat_pair,c75,c25).
%% prop(unsat_pair,c80,c25).
%% prop(unsat_pair,c85,c25).
%% prop(unsat_pair,c90,c25).
%% prop(unsat_pair,c95,c25).
%% prop(unsat_pair,c5,c30).
%% prop(unsat_pair,c35,c30).
%% prop(unsat_pair,c40,c30).
%% prop(unsat_pair,c45,c30).
%% prop(unsat_pair,c50,c30).
%% prop(unsat_pair,c55,c30).
%% prop(unsat_pair,c60,c30).
%% prop(unsat_pair,c65,c30).
%% prop(unsat_pair,c70,c30).
%% prop(unsat_pair,c75,c30).
%% prop(unsat_pair,c80,c30).
%% prop(unsat_pair,c85,c30).
%% prop(unsat_pair,c90,c30).
%% prop(unsat_pair,c95,c30).
%% prop(unsat_pair,c5,c35).
%% prop(unsat_pair,c40,c35).
%% prop(unsat_pair,c45,c35).
%% prop(unsat_pair,c50,c35).
%% prop(unsat_pair,c55,c35).
%% prop(unsat_pair,c60,c35).
%% prop(unsat_pair,c65,c35).
%% prop(unsat_pair,c70,c35).
%% prop(unsat_pair,c75,c35).
%% prop(unsat_pair,c80,c35).
%% prop(unsat_pair,c85,c35).
%% prop(unsat_pair,c90,c35).
%% prop(unsat_pair,c95,c35).
%% prop(unsat_pair,c5,c40).
%% prop(unsat_pair,c45,c40).
%% prop(unsat_pair,c50,c40).
%% prop(unsat_pair,c55,c40).
%% prop(unsat_pair,c60,c40).
%% prop(unsat_pair,c65,c40).
%% prop(unsat_pair,c70,c40).
%% prop(unsat_pair,c75,c40).
%% prop(unsat_pair,c80,c40).
%% prop(unsat_pair,c85,c40).
%% prop(unsat_pair,c90,c40).
%% prop(unsat_pair,c95,c40).
%% prop(unsat_pair,c5,c45).
%% prop(unsat_pair,c50,c45).
%% prop(unsat_pair,c55,c45).
%% prop(unsat_pair,c60,c45).
%% prop(unsat_pair,c65,c45).
%% prop(unsat_pair,c70,c45).
%% prop(unsat_pair,c75,c45).
%% prop(unsat_pair,c80,c45).
%% prop(unsat_pair,c85,c45).
%% prop(unsat_pair,c90,c45).
%% prop(unsat_pair,c95,c45).
%% prop(unsat_pair,c55,c50).
%% prop(unsat_pair,c60,c50).
%% prop(unsat_pair,c65,c50).
%% prop(unsat_pair,c70,c50).
%% prop(unsat_pair,c75,c50).
%% prop(unsat_pair,c80,c50).
%% prop(unsat_pair,c85,c50).
%% prop(unsat_pair,c90,c50).
%% prop(unsat_pair,c95,c50).
%% prop(unsat_pair,c60,c55).
%% prop(unsat_pair,c65,c55).
%% prop(unsat_pair,c70,c55).
%% prop(unsat_pair,c75,c55).
%% prop(unsat_pair,c80,c55).
%% prop(unsat_pair,c85,c55).
%% prop(unsat_pair,c90,c55).
%% prop(unsat_pair,c95,c55).
%% prop(unsat_pair,c65,c60).
%% prop(unsat_pair,c70,c60).
%% prop(unsat_pair,c75,c60).
%% prop(unsat_pair,c80,c60).
%% prop(unsat_pair,c85,c60).
%% prop(unsat_pair,c90,c60).
%% prop(unsat_pair,c95,c60).
%% prop(unsat_pair,c70,c65).
%% prop(unsat_pair,c75,c65).
%% prop(unsat_pair,c80,c65).
%% prop(unsat_pair,c85,c65).
%% prop(unsat_pair,c90,c65).
%% prop(unsat_pair,c95,c65).
%% prop(unsat_pair,c75,c70).
%% prop(unsat_pair,c80,c70).
%% prop(unsat_pair,c85,c70).
%% prop(unsat_pair,c90,c70).
%% prop(unsat_pair,c95,c70).
%% prop(unsat_pair,c80,c75).
%% prop(unsat_pair,c85,c75).
%% prop(unsat_pair,c90,c75).
%% prop(unsat_pair,c95,c75).
%% prop(unsat_pair,c85,c80).
%% prop(unsat_pair,c90,c80).
%% prop(unsat_pair,c95,c80).
%% prop(unsat_pair,c90,c85).
%% prop(unsat_pair,c95,c85).
%% prop(unsat_pair,c95,c90).
%% prop(unsat_pair,c5,c100).
%% prop(unsat_pair,c15,c100).
%% prop(unsat_pair,c20,c100).
%% prop(unsat_pair,c25,c100).
%% prop(unsat_pair,c30,c100).
%% prop(unsat_pair,c35,c100).
%% prop(unsat_pair,c40,c100).
%% prop(unsat_pair,c45,c100).
%% prop(unsat_pair,c50,c100).
%% prop(unsat_pair,c55,c100).
%% prop(unsat_pair,c60,c100).
%% prop(unsat_pair,c65,c100).
%% prop(unsat_pair,c70,c100).
%% prop(unsat_pair,c75,c100).
%% prop(unsat_pair,c80,c100).
%% prop(unsat_pair,c85,c100).
%% prop(unsat_pair,c90,c100).
%% prop(unsat_pair,c95,c100).


%% %% FASTPOPPER LEARNS THIS
%% %% next_score(A,B):-does(C,D),opponent(A,C),my_true_score(A,B),lay_claim(D).
%% %% next_score(A,B):-my_true_score(A,B),opponent(C,A),c80(B),my_true_control(C).
%% %% next_score(A,B):-my_succ(B,D),my_true_score(A,D),does(A,C),lay_claim(C).
%% %% next_score(A,B):-my_true_score(A,B),does(A,C),end_game(C).
%% %% next_score(A,B):-my_true_score(A,C),does(E,D),opponent(A,E),c65(C),c90(B),end_game(D).
%% %% next_score(A,B):-c55(C),my_true_score(A,C),c80(B).
%% %% next_score(A,B):-my_true_score(A,C),c95(B),does(E,D),opponent(E,A),c70(C),end_game(D).



