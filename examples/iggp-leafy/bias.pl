%% next_leaf(V0, V1) :- true_leaf(V0, V1), V1 = 1, pos(V0), pos(V1).
%% next_leaf(V0, V1) :- true_leaf(V0, V0), pos(V0), pos(V1).
%% next_leaf(V0, V1) :- true_leaf(V8, V1), true_leaf(V0, V1), true_leaf(V9, V1), V8 = 3, V9 = 1, pos(V0), pos(V1), pos(V8), pos(V9).

max_vars(5).
max_body(10).
max_clauses(1).

head_pred(next_leaf,3).
body_pred(close,3).
body_pred(does_move,4).
body_pred(my_index,2).
body_pred(my_input_move,4).
body_pred(my_true_isplayer,4).
body_pred(my_true_leaf,3).
body_pred(c1,1).
body_pred(c2,1).
body_pred(c3,1).
body_pred(c4,1).
body_pred(c5,1).
body_pred(c6,1).
body_pred(c7,1).
body_pred(c8,1).
body_pred(c9,1).

%% (<= (next (leaf ?X ?Y)) (true (leaf ?X ?Y)) (not (true (isplayer ?X ?Y red))) (not (true (isplayer ?X ?Y blue))))

type(next_leaf,(ex,pos,pos)).
type(close,(ex,pos,pos)).
type(does_move,(ex,pos,pos,action)).
type(my_index,(ex,pos)).
type(my_input_move,(ex,agent,pos,pos)).
type(my_true_isplayer,(ex,pos,pos,agent)).
type(my_true_leaf,(ex,pos,pos)).
type(c1,(pos,)).
type(c2,(pos,)).
type(c3,(pos,)).
type(c4,(pos,)).
type(c5,(pos,)).
type(c6,(pos,)).
type(c7,(pos,)).
type(c8,(pos,)).
type(c9,(pos,)).

%% #modeb(true_leaf(ph(pos), ph(pos))).
%% #modeb(true_isplayer(ph(pos), ph(pos), ph(agent))).
%% #modeh(next_isplayer(ph(pos), ph(pos), ph(agent))).
%% #modeb(input_move(ph(agent), ph(pos), ph(pos))).
%% #modeb(does_move(ph(agent), ph(pos), ph(pos))).
%% #modeb(role(ph(agent))).
%% #modeb(close(ph(pos), ph(pos))).
%% #modeb(index(ph(pos))).
%% #modeb(agent(ph(agent))).
%% #modeb(pos(ph(pos))).
%% #modeb(int(ph(int))).

%% BECAUSE WE DO NOT LEARN FROM INTERPRETATIONS
:-
    clause(C),
    #count{V : clause_var(C,V),var_type(C,V,ex)} != 1.


%% true, next :: prop -> bool.

%% legal, input, does :: agent -> action -> bool.

%% goal :: agent -> int -> bool.

%% terminal :: bool.

%% role :: agent -> bool.

%% red, blue :: agent.

%% 1, 2, 3, 4, 5, 6, 7, 8 :: pos.

%% 0, 50, 100 :: int.

%% leaf :: pos -> pos -> prop.

%% isplayer :: pos -> pos -> agent -> prop.

%% move :: pos -> pos -> action.

%% close :: pos -> pos -> bool.

%% index :: pos -> bool.

