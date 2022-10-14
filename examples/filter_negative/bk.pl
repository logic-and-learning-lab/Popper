
map1(_P,[]).
map1(P,[H|T]) :- call(P,H),map1(P,T).

is_list([]).
is_list([_|_]).

fold(_P,Acc,[],Acc).
fold(P,Acc,[H|T],Out) :- call(P,Acc,H,Inter),fold(P,Inter,T,Out).

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

cons(A,B,C):-
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

empty_in([]).
empty_out([]).

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


c0(0).
c1(1).
c2(2).
c3(3).
c4(4).
c5(5).
c6(6).
c7(7).
c8(8).
c9(9).
c10(10).
c11(11).
c12(12).
c13(13).
c14(14).
c15(15).
c16(16).
c17(17).
c18(18).
c19(19).
c20(20).
c21(21).
c22(22).
c23(23).
c24(24).
c25(25).
c26(26).
c27(27).
c28(28).
c29(29).
c30(30).
c31(31).
c32(32).
c33(33).
c34(34).
c35(35).
c36(36).
c37(37).
c38(38).
c39(39).
c40(40).
c41(41).
c42(42).
c43(43).
c44(44).
c45(45).
c46(46).
c47(47).
c48(48).
c49(49).
c50(50).
c51(51).
c52(52).
c53(53).
c54(54).
c55(55).
c56(56).
c57(57).
c58(58).
c59(59).
c60(60).
c61(61).
c62(62).
c63(63).
c64(64).
c65(65).
c66(66).
c67(67).
c68(68).
c69(69).
c70(70).
c71(71).
c72(72).
c73(73).
c74(74).
c75(75).
c76(76).
c77(77).
c78(78).
c79(79).
c80(80).
c81(81).
c82(82).
c83(83).
c84(84).
c85(85).
c86(86).
c87(87).
c88(88).
c89(89).
c90(90).
c91(91).
c92(92).
c93(93).
c94(94).
c95(95).
c96(96).
c97(97).
c98(98).
c99(99).
c100(100).