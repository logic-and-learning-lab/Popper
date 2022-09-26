% BEST PROG 559:
%% f(A):-cell(A,G,E,C),white(E),cell(A,F,E,D),rook(C),king(D),distance(F,G,B),one(B).
% Precision:1.00, Recall:1.00, TP:4, FN:0, TN:10, FP:0
%% Total execution time: 41.29s

max_vars(7).
max_body(5).
max_clauses(1).

%% disable_datalog.
%% allow_singeltons.
%% :- not body_size(_,4).

head_pred(f,1).
body_pred(distance,3).
body_pred(cell,4).
body_pred(king,1).
body_pred(rook,1).
body_pred(white,1).
body_pred(black,1).
body_pred(one,1).

type(f,(state,)).
type(distance,(pos, pos, integer)).
type(cell,(state, pos, color, piecetype)).
type(king,(piecetype,)).
type(rook,(piecetype,)).
type(white,(color,)).
type(black,(color,)).
type(one,(integer,)).

direction(f,(in,)).
direction(distance,(in, in, out)).
direction(cell,(in, out, out, out)).
direction(king,(in,)).
direction(rook,(in,)).
direction(white,(in,)).
direction(black,(in,)).
direction(one,(in,)).

%% :-
%%     clause(C),
%%     #count{V : var_in_rule(C,V),var_type(C,V,state)} != 1.


%% %% (base) ➜  popper-public git:(main) ✗ clingo popper/lp/alan.pl examples/krk/bias.pl -q -n800
%% %% 4.3s

%% %% NO DIRECTIONS
%% %% 1.2s

%% %% NO TYPES
%% %% ??

%% %% non_datalog.
%% %% 4.52

%% %% allow_singletons.
%% %% 1.24


%% prop(unique_ab_c,distance).
%% prop(unique_ab_cd,cell).
%% prop(singleton,white).
%% prop(singleton,rook).
%% prop(unique_abc_d,cell).
%% prop(unique_abd_c,cell).
%% prop(unsat_pair,rook,king).
%% prop(unsat_pair,white,black).
%% prop(singleton,one).
%% prop(singleton,king).
%% prop(singleton,black).