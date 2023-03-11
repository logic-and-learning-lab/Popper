%%%%%%%%%% EXAMPLE LOADING %%%%%%%%%%
:- dynamic
    neg_index/2.

load_examples:-
    load_pos,
    load_neg.

load_pos:-
    current_predicate(pos/1),!,
    findall(X, pos(X), Pos),
    assert_pos_aux(Pos,1).
load_pos.

load_neg:-
    current_predicate(neg/1),!,
    findall(X, neg(X), Neg),
    assert_neg_aux(Neg,-1).
load_neg.

assert_pos_aux([],_).
assert_pos_aux([H|T],I1):-
    assertz(pos_index(I1, H)),
    I2 is I1+1,
    assert_pos_aux(T,I2).

assert_neg_aux([],_).
assert_neg_aux([H|T],I1):-
    assertz(neg_index(I1, H)),
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

%% ========== FUNCTIONAL CHECKS ==========
non_functional:-
    pos(Atom),
    non_functional(Atom),!.

%% %% ========== REDUNDANCY CHECKS ==========

subsumes(C,D) :- \+ \+ (copy_term(D,D2), numbervars(D2,0,_), subset(C,D2)).

subset([], _D).
subset([A|B], D):-
    member(A, D),
    subset(B,D).

redundant_literal(C1):-
    select(_,C1,C2),
    subsumes(C1,C2),!.
    %% writeln(C1),
    %% writeln(C2).

redundant_clause(P1):-
    select(C1,P1,P2),
    member(C2,P2),
    subsumes(C1,C2),!.

%% %% TODO: ADD MEANINGFUL COMMENT
find_redundant_rule(P1,K1,K2):-
    select(K1-C1,P1,P2),
    member(K2-C2,P2),
    subsumes(C1,C2),!.
