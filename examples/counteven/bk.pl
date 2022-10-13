%% :-table (counteven/2) as incremental.
%% :-dynamic([counteven/2], [incremental(true)]).

increment(A,B):-
    (nonvar(A) -> (\+ is_list(A), integer(A)); true),
    (nonvar(B) -> (\+ is_list(B), integer(B)); true),
    my_succ(A,B).

decrement(A,B):-
    (nonvar(A) -> (\+ is_list(A), integer(A)); true),
    (nonvar(B) -> (\+ is_list(B), integer(B)); true),
    my_succ(B,A).

changesign(A,B) :- B is -A.

my_succ(A,B) :- nonvar(A), integer(A), A<0,!, A1 is -A, succ(B1,A1), B is -B1.
my_succ(A,B) :- nonvar(B), integer(B), B<1,!, B1 is -B, succ(B1,A1), A is -A1.
my_succ(A,B) :- nonvar(A), integer(A), A>=0,!, succ(A,B).
my_succ(A,B) :- nonvar(B), integer(B), B>=1,!, succ(A,B).

double(X,Y) :- nonvar(X), integer(X), Y is 2*X.
triple(X,Y) :- nonvar(X), integer(X), Y is 3*X.

my_length(A,B):-
    (nonvar(A) -> is_list(A); true),
    (nonvar(B) -> (\+ is_list(B), integer(B)); true),
    length(A,B).

ord(A,B) :-
    nonvar(B),!,
    integer(B),
    B>=97,
    B=<122,
    atom_codes(A,[B]).

ord(A,B) :-
    nonvar(A),
    atom(A),
    (var(B);integer(B)),
    between(97,122,B),
    atom_codes(A,[B]).

bin(0,[0]) :- !.
bin(1,[1]) :- !.
bin(A,B) :-
   \+ is_list(A),
   A>1,
   X is A mod 2,
   Y is A//2,
   bin(Y,B1),
   append(B1,[X],B).

mod2(A,B) :- nonvar(A), \+is_list(A), B is A mod 2.

max(A,B,A) :- nonvar(A), nonvar(B), A>= B,!.
max(_,B,B).

min(A,B,A) :- nonvar(A), nonvar(B), A=< B,!.
min(_,B,B).

prepend(A,B,C):-
    append([A],B,C).
%% cons2(A,B,C):-
%%     append(A,[B],C).
%% consend(A,B,C) :-
%%     append(B,[A],C).
%% comps([H|T],H,T).

xor(1,1,0).
xor(1,0,1).
xor(0,1,1).
xor(0,0,0).

tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    (nonvar(A) -> \+ is_list(A); true),
    (nonvar(B) -> \+ is_list(B); true),
    (nonvar(C) -> \+ is_list(B); true),
    C is A+B.
mult(A,B,C):-
    (nonvar(A) -> \+ is_list(A); true),
    (nonvar(B) -> \+ is_list(B); true),
    (nonvar(C) -> \+ is_list(B); true),
    C is A*B.
empty([]).

element([X|_],X):-!.
element([_|T],X):-
    element(T,X).

zero(0).
one(1).
negative(A) :- nonvar(A), A<0.
positive(A) :- nonvar(A), A>=0.

gt(A,B):-
    nonvar(A),
    nonvar(B),
    \+is_list(A),
    \+is_list(B),
    integer(A),
    integer(B),
    A > B.

eq(A,A).

geq(A,B):-
    nonvar(A),
    nonvar(B),
    \+is_list(A),
    \+is_list(B),
    integer(A),
    integer(B),
    A >= B.


leq(A,B):-
    nonvar(A),
    nonvar(B),
    \+is_list(A),
    \+is_list(B),
    integer(A),
    integer(B),
    A =< B.


even(A):-
    nonvar(A),
    integer(A),
    \+ is_list(A),
    0 is A mod 2.

odd(A):-
    nonvar(A),
    integer(A),
    \+ is_list(A),
    1 is A mod 2.
