cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).

cons1(A,B,C):-
    cons(A,B,C).
cons2(A,B,C):-
    cons(A,B,C).
succ(A,B):-
    B is A+1.

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(2).
even(4).
even(6).
even(8).
even(10).

%% :- ['exs'].


%% non_functional(Atom1):-
%%     Atom1=..[f,A,B],
%%     Atom2=..[f,A,C],
%%     call(Atom2),
%%     B \= C.

%% popper_program_validation(_Prog):-
%%     catch(call_with_time_limit(0.1,forall(pos(Atom),\+non_functional(Atom))),time_limit_exceeded,(writeln('FALSE'),false)),!.

%% popper_program_validation(_):-
%%     writeln('FAIL!!').
