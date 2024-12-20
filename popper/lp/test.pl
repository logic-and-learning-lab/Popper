%%%%%%%%%% EXAMPLE LOADING %%%%%%%%%%
:- dynamic
    neg_index/2.

load_examples:-
    load_pos,
    load_neg.

get_pos(S):-
    findall(K, pos_index(K, _Atom), S).

load_pos:-
    current_predicate(pos/1),!,
    findall(X, pos(X), Pos),
    assert_pos_aux(Pos,0).
load_pos.

load_neg:-
    current_predicate(neg/1),!,
    findall(X, neg(X), Neg),
    assert_neg_aux(Neg,0).
load_neg.

assert_pos_aux([],_).
assert_pos_aux([H|T],I1):-
    assertz(pos_index(I1, H)),
    I2 is I1+1,
    assert_pos_aux(T,I2).

assert_neg_aux([],_).
assert_neg_aux([H|T],I1):-
    assertz(neg_index(I1, H)),
    H=..[_|Args],
    NegFact=..[neg_fact|Args],
    assertz(NegFact),
    I2 is I1-1,
    assert_neg_aux(T,I2).

%%%%%%%%%% EXAMPLE TESTING %%%%%%%%%%

ex_index(ID,Atom):-
    current_predicate(pos_index/2),
    pos_index(ID,Atom).
ex_index(ID,Atom):-
    current_predicate(neg_index/2),
    neg_index(ID,Atom).

%% test_ex(Atom):-
%%     current_predicate(timeout/1),!,
%%     timeout(T),
%%     catch(call_with_time_limit(T, call(Atom)),time_limit_exceeded,false),!.

test_ex(X):-
    current_predicate(timeout/1),!,
    timeout(T),
    catch(call_with_time_limit(T, call(X)),time_limit_exceeded,false),!.
    %% call_with_inference_limit(call(X), 100000, Result),!,
    %% call_with_inference_limit(call(X), 10000, Result),!,
    %% Result \= inference_limit_exceeded.

test_ex(Atom):-
    call(Atom),!.

pos_covered(Xs):-
    findall(ID, (pos_index(ID,Atom),test_ex(Atom)), Xs).

neg_covered(Xs):-
    findall(ID, (neg_index(ID,Atom),test_ex(Atom)), Xs).

neg_uncovered(Xs):-
    findall(ID, (neg_index(ID,Atom),\+test_ex(Atom)), Xs).

is_more_inconsistent(Xs):-
    neg_index(Id,Atom),
    \+member(Id,Xs),
    test_ex(Atom),!.

covers_any(Xs,Id):-
    member(Id,Xs),
    neg_index(Id,Atom),
    test_ex(Atom),
    %% writeln(Id),
    !.


prog1_covers(Atom):-
    Atom =.. [_|T],
    List2 = [prog1|T],
    Atom2 =..List2,
    call(Atom2).

prog2_covers(Atom):-
    Atom =.. [_|T],
    List2 = [prog2|T],
    Atom2 =..List2,
    call(Atom2).

covers_more:-
    neg_index(_,Atom),
    prog2_covers(Atom),
    \+prog1_covers(Atom),!.



inconsistent:-
    neg_index(_,Atom),
    test_ex(Atom),!.

sat:-
    pos_index(_,Atom),
    test_ex(Atom),!.

covers_at_least_k_pos(K):-
    Counter = counter(0),
    pos_index(_,Atom),
    once(test_ex(Atom)),
    arg(1, Counter, N0),
    N is N0 + 1,
    ((N>=K -> true,!);
    (nb_setarg(1, Counter, N),
    fail)).

%% covers_at_least_k:-
%%     pos_index(_,Atom),
%%     test_ex(Atom),!.

succeeds_k_times(Goal,Body,Times):-
    Counter = counter(0),
    Goal,
    once(Body),
    arg(1, Counter, N0),
    N is N0 + 1,
    ((N>=Times -> true,!);
    (nb_setarg(1, Counter, N),
    fail)).

findfirstn(N, Template, Goal_0, Instances) :-
   findall(Template, call_firstn(Goal_0, N), Instances).

call_firstn(Goal_0, N) :-
   N + N mod 1 >= 0, % ensures that N >=0 and N is an integer
   call_nth(Goal_0, Nth),
   ( Nth == N -> ! ; true ).


%% ========== FUNCTIONAL CHECKS ==========
non_functional:-
    pos(Atom),
    non_functional(Atom),!.

%% %% ========== REDUNDANCY CHECKS ==========

subsumes(C,D) :- \+ \+ (copy_term(D,D2), numbervars(D2,0,_), subset(C,D2)).


%% subsumes2(C,D) :- \+ \+ (copy_term(D,D2), numbervars(D2,0,_), writeln(D2),subset(C,D2)).

subset([], _D).
subset([A|B], D):-
    member(A, D),
    subset(B,D).

redundant_literal(C1):-
    select(_,C1,C2),
    subsumes(C1,C2),
    !.

redundant_clause(P1):-
    select(C1,P1,P2),
    member(C2,P2),
    subsumes(C1,C2),!.

%% %% TODO: ADD MEANINGFUL COMMENT
find_redundant_rule(P1,K1,K2):-
    select(K1-C1,P1,P2),
    member(K2-C2,P2),
    subsumes(C1,C2),!.


subsumes2(A,B):-
    subsumes(A,B),
    writeln(asda),
    writeln(A),
    writeln(B),
    \+ subsumes(B,A).

subsumes2(A,B):-
    subsumes(A,B),
    subsumes(B,A),
    length(A,N1),
    length(B,N2),
    writeln(asda-N1-N2),
    writeln(A),
    writeln(B),
    %% writeln(N1-N2),
    N1 =< N2.

%% subsumes2(A,B):-
%%     subsumes(A,B),
%%     subsumes(B,A),
%%     length(A,N1),
%%     length(B,N2),
%%     writeln(asda-N1-N2),
%%     writeln(A),
%%     writeln(B),
%%     %% writeln(N1-N2),
%%     N1 < N2.
%% %% TODO: ADD MEANINGFUL COMMENT
%% reduce_theory(P1,K2):-
%%     %% writeln(P1),
%%     %% writeln(P2),
%%     reduce_theory_(P1,P2),
%%     findall(K, member(K-_,P2), K2).

%% reduce_theory_(P1,P2):-
%%     select(K1-C1,P1,P3),
%%     member(K2-C2,P3),
%%     subsumes(C1,C2),!,
%%     %% writeln(asda),
%%     %% writeln(C1),
%%     %% writeln(C2),
%%     %% subsumes2(C2,C1),!,
%%     %% writeln(K1-K2),
%%     reduce_theory_(P3,P2).

%% reduce_theory_(P1,P1).

%% tmp(S, Inconsistent):-
%%     pos_covered(S),
%%     (inconsistent -> Inconsistent=true; Inconsistent=false).