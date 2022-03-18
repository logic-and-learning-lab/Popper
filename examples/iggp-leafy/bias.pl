max_vars(6).
max_body(6).
max_clauses(1).

head_pred(next_leaf,2).
%% body_pred(close,2).
%% body_pred(does_move,3).
%% body_pred(my_index,1).
%% body_pred(my_input_move,3).
%% body_pred(my_true_isplayer,3).
body_pred(not_my_true_isplayer,3).
body_pred(my_true_leaf,2).
%% body_pred(agent,1).
%% body_pred(pos,1).

type(next_leaf,(pos,pos)).
type(close,(pos,pos)).
type(does_move,(pos,pos,action)).
type(my_index,(pos,)).
type(my_input_move,(agent,pos,pos)).
type(my_true_isplayer,(pos,pos,agent)).
type(not_my_true_isplayer,(pos,pos,agent)).
type(my_true_leaf,(pos,pos)).
type(agent,(agent,)).
type(pos,(pos,)).

body_pred(P,1):-
    constant(T, P).
type(P,(T,)):-
    constant(T, P).

%% (<= (next (leaf ?X ?Y)) (true (leaf ?X ?Y)) (not (true (isplayer ?X ?Y red))) (not (true (isplayer ?X ?Y blue))))

constant(agent, red).
constant(agent, blue).
%% constant(pos, c1).
%% constant(pos, c2).
%% constant(pos, c3).
%% constant(pos, c4).
%% constant(pos, c5).
%% constant(pos, c6).
%% constant(pos, c7).
%% constant(pos, c8).
%% constant(int, c0).
%% constant(int, c50).
%% constant(int, c100).

%% (<= (next (leaf ?X ?Y))
%%     (true (leaf ?X ?Y))
%%     (not_true_isplayer ?X ?Y Z1,
%%     (not (true (isplayer ?X ?Y Z2),
%%     red(Z1),
%%     blue(Z1).



:-
    body_pred(close,(V0,V1)), V0 > V1.

%% prop(singleton,c1).
%% prop(singleton,c2).
%% prop(singleton,c3).
%% prop(singleton,c4).
%% prop(singleton,c5).
%% prop(singleton,c6).
%% prop(singleton,c7).
%% prop(singleton,c8).
%% prop(singleton,c9).
%% prop(unsat_pair,c2,c1).
%% prop(unsat_pair,c3,c1).
%% prop(unsat_pair,c4,c1).
%% prop(unsat_pair,c5,c1).
%% prop(unsat_pair,c6,c1).
%% prop(unsat_pair,c7,c1).
%% prop(unsat_pair,c8,c1).
%% prop(unsat_pair,c9,c1).
%% prop(unsat_pair,c3,c2).
%% prop(unsat_pair,c4,c2).
%% prop(unsat_pair,c5,c2).
%% prop(unsat_pair,c6,c2).
%% prop(unsat_pair,c7,c2).
%% prop(unsat_pair,c8,c2).
%% prop(unsat_pair,c9,c2).
%% prop(unsat_pair,c4,c3).
%% prop(unsat_pair,c5,c3).
%% prop(unsat_pair,c6,c3).
%% prop(unsat_pair,c7,c3).
%% prop(unsat_pair,c8,c3).
%% prop(unsat_pair,c9,c3).
%% prop(unsat_pair,c5,c4).
%% prop(unsat_pair,c6,c4).
%% prop(unsat_pair,c7,c4).
%% prop(unsat_pair,c8,c4).
%% prop(unsat_pair,c9,c4).
%% prop(unsat_pair,c6,c5).
%% prop(unsat_pair,c7,c5).
%% prop(unsat_pair,c8,c5).
%% prop(unsat_pair,c9,c5).
%% prop(unsat_pair,c7,c6).
%% prop(unsat_pair,c8,c6).
%% prop(unsat_pair,c9,c6).
%% prop(unsat_pair,c8,c7).
%% prop(unsat_pair,c9,c7).
%% prop(unsat_pair,c9,c8).
%% prop(unsat_pair,pos,c9).
%% prop(unsat_pair,my_index,c9).

%% prop(unsat_pair,my_true_isplayer,not_my_true_isplayer).
%% prop(antitriangular,close).
%% prop(antitransitive,close).
%% prop(unique_ab_c,my_true_isplayer).
%% prop(unique_c_ab,my_true_isplayer).
%% prop(unique_a_bc,does_move).
%% prop(unique_bc_a,does_move).


prop(antitransitive,close).
prop(antitriangular,close).
prop(singleton,blue).
prop(singleton,red).
prop(unique_a_bc,does_move).
prop(unique_ab_c,my_true_isplayer).
prop(unique_bc_a,does_move).
prop(unique_c_ab,my_true_isplayer).
prop(countk,blue,1).
prop(countk,red,1).
prop(unsat_pair,my_true_isplayer,not_my_true_isplayer).
prop(unsat_pair,red,blue).