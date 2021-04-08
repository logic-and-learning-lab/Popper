cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    C is A+B.
empty([]).
zero(0).
even(A):-
    0 is A mod 2.


dummy1(_,_):-
    false.
dummy2(_,_):-
    false.
dummy3(_,_):-
    false.
dummy4(_,_):-
    false.

dummy1m(_):-
    false.
dummy2m(_):-
    false.
dummy3m(_):-
    false.
dummy4m(_):-
    false.

%% do_test(TP,FN,TN,FP):-
%%     aggregate_all(count, (pos(X), call(X)), TP),
%%     aggregate_all(count, (neg(X), call(X)), FP),
%%     num_neg(NumNeg),
%%     num_pos(NumPos),
%%     TN is NumNeg-FP,
%%     FN is NumPos-TP.

%% do_test_minimal(TP,FN,TN,FP):-
%%     test_pos(TP,FN),
%%     test_neg(TN,FP).

%% test_pos(TP,FN):-
%%     TPS = counter(0),
%%     FNS = counter(0),

%%     (
%%         pos(X),
%%         (call(X) ->
%%             (arg(1,TPS,S0),S1 is S0+1, nb_setarg(1,TPS,S1));
%%             (arg(1,FNS,S0),S1 is S0+1, nb_setarg(1,FNS,S1))
%%         ),

%%         ((\+arg(1,TPS,0),\+arg(1,FNS,0)) -> (arg(1,TPS,TP),arg(1,FNS,FN)); fail);
%%         arg(1,TPS,TP),
%%         arg(1,FNS,FN)
%%         ).

%% test_neg(TN,FP):-
%%     TNS = counter(0),
%%     FPS = counter(0),
%%     (
%%         neg(X),
%%         (call(X) ->
%%             (arg(1,FPS,S0),S1 is S0+1, nb_setarg(1,FPS,S1));
%%             (arg(1,TNS,S0),S1 is S0+1, nb_setarg(1,TNS,S1))
%%         ),

%%         (\+arg(1,FPS,0) -> (arg(1,FPS,FP),arg(1,TNS,TN)); fail);
%%         arg(1,FPS,FP),
%%         arg(1,TNS,TN)
%%         ).