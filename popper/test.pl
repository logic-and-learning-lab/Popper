do_test(TP,FN,TN,FP):-
    do_test_pos(TP,FN),!,
    do_test_neg(TN,FP),!.

do_test_pos(0,0):-
    \+ current_predicate(pos/1),!.
do_test_pos(TP,FN):-
    aggregate_all(count,(pos(X),test_ex(X)),TP),
    num_pos(NumPos),
    FN is NumPos-TP.

do_test_neg(0,0):-
    \+ current_predicate(neg/1),!.
do_test_neg(TN,FP):-
    aggregate_all(count,(neg(X),test_ex(X)),FP),
    num_neg(NumNeg),
    TN is NumNeg-FP.

test_ex(X):-
    timeout(T),
    catch(call_with_time_limit(T, call(X)),time_limit_exceeded,false),!.

count_pos(0):-
    \+ current_predicate(pos/1),!.
count_pos(N):-
    aggregate_all(count,pos(_),N),!.
count_neg(0):-
    \+ current_predicate(neg/1),!.
count_neg(N):-
    aggregate_all(count,neg(_),N),!.

%%%%%%%%%% MINIMAL TESTING %%%%%%%%%%
%% we do not need to test all the examples
%% we want to know:
%% 1. whether a program is incomplete (whether a positive example is not entailed) FN > 0
%% 2. whether a program is inconsistent (whether a negative example is entailed) FP > 0
%% 3. whether a program is totally incomplete (whether no positive examples are entailed) TP > 0 FN > 0
%% AC: TN IS UNSUSED WITH MINIMAL TESTING
do_test_minimal(TP,FN,0,FP):-
    minimal_test_pos(TP,FN),!,
    minimal_test_neg(FP),!.

test_pos_ex_aux(X,Counter,_):-
    test_ex(X),!,
    arg(1,Counter,C0),
    C1 is C0+1,
    nb_setarg(1,Counter,C1).

test_pos_ex_aux(_,_,Counter):-
    arg(1,Counter,C0),
    C1 is C0+1,
    nb_setarg(1,Counter,C1).

minimal_test_pos(0,0):-
    \+ current_predicate(pos/1),!.
minimal_test_pos(TP,FN):-
    TPC = counter(0),
    FNC = counter(0),
    (
        pos(X),
        test_pos_ex_aux(X,TPC,FNC),
        %% if TP > 0 and FN > 0 then a program is incomplete but not totally_incomplete
        ((\+arg(1,TPC,0),\+arg(1,FNC,0)) -> (arg(1,TPC,TP),arg(1,FNC,FN)); fail);
        arg(1,TPC,TP),
        arg(1,FNC,FN)
    ).

minimal_test_neg(0):-
    \+ current_predicate(neg/1),!.
minimal_test_neg(1):-
    neg(X),
    test_ex(X),!.
minimal_test_neg(0).


%% ==========

subsumes(C,D) :- \+ \+ (copy_term(D,D2), numbervars(D2,0,_), subset(C,D2)).

subset([], _D).
subset([A|B], D):-
    member(A, D),
    subset(B,D).

redundant_literal(C1):-
    select(_,C1,C2),
    subsumes(C1,C2),!.

redundant_clause(P1):-
    select(C1,P1,P2),
    member(C2,P2),
    subsumes(C1,C2),!.