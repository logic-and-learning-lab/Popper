:-[bk].
:-[exs].



%% next_count(A,V0) :- succ(V16, V0), true_count(A,V16), int(V0), word(V0), int(V16), word(V16).
next_success(A,V0) :- succ(V3, V0), true_success(A,V3), int(V0), word(V0), int(V3), word(V3).


%% a:-
%%     forall(pos(A), call(A)).

a:-
    pos(A), \+call(A), writeln(A).

b:-
    forall(pos(A), \+call(A)).

