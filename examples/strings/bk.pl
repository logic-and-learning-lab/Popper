is_empty(s([],_)).
not_is_empty(s([_|_],_)).

is_space(s([' '|_],_)).
not_is_space(A):- \+is_space(A).

is_uppercase(s([H|_],_)):-is_uppercase_aux(H).
not_is_uppercase(A):- \+ is_uppercase(A).

is_lowercase(s([H|_],_)):-is_lowercase_aux(H).
not_is_lowercase(A):- \+ is_lowercase(A).

is_letter(s([H|_],_)):-is_lowercase_aux(H);is_uppercase(H).
not_is_letter(A):- \+ is_letter(A).

is_number(s([H|_],_)):-is_number_aux(H).
not_is_number(A):- \+ is_number(A).

skip1(s([_|Ta],B),s(Ta,B)).
copy1(s([H|Ta],[H|Tb]),s([H|Ta],Tb)).
write1(s(A,[H|Tb]),s(A,Tb),H).
copyskip1(s([H|Ta],[H|Tb]),s(Ta,Tb)).
skipcopy1(A,B):-skip1(A,C),copy1(C,B).


mk_uppercase(s([H1|Ta],[H2|Tb]),s(Ta,Tb)):-
    convert_case(H2,H1).
mk_lowercase(s([H1|Ta],[H2|Tb]),s(Ta,Tb)):-
    convert_case(H1,H2).


is_uppercase_aux('A').
is_uppercase_aux('B').
is_uppercase_aux('C').
is_uppercase_aux('D').
is_uppercase_aux('E').
is_uppercase_aux('F').
is_uppercase_aux('G').
is_uppercase_aux('H').
is_uppercase_aux('I').
is_uppercase_aux('J').
is_uppercase_aux('K').
is_uppercase_aux('L').
is_uppercase_aux('M').
is_uppercase_aux('N').
is_uppercase_aux('O').
is_uppercase_aux('P').
is_uppercase_aux('Q').
is_uppercase_aux('R').
is_uppercase_aux('S').
is_uppercase_aux('T').
is_uppercase_aux('U').
is_uppercase_aux('V').
is_uppercase_aux('W').
is_uppercase_aux('X').
is_uppercase_aux('Y').
is_uppercase_aux('Z').

is_lowercase_aux('a').
is_lowercase_aux('b').
is_lowercase_aux('c').
is_lowercase_aux('d').
is_lowercase_aux('e').
is_lowercase_aux('f').
is_lowercase_aux('g').
is_lowercase_aux('h').
is_lowercase_aux('i').
is_lowercase_aux('j').
is_lowercase_aux('k').
is_lowercase_aux('l').
is_lowercase_aux('m').
is_lowercase_aux('n').
is_lowercase_aux('o').
is_lowercase_aux('p').
is_lowercase_aux('q').
is_lowercase_aux('r').
is_lowercase_aux('s').
is_lowercase_aux('t').
is_lowercase_aux('u').
is_lowercase_aux('v').
is_lowercase_aux('w').
is_lowercase_aux('x').
is_lowercase_aux('y').
is_lowercase_aux('z').

convert_case('A','a').
convert_case('B','b').
convert_case('C','c').
convert_case('D','d').
convert_case('E','e').
convert_case('F','f').
convert_case('G','g').
convert_case('H','h').
convert_case('I','i').
convert_case('J','j').
convert_case('K','k').
convert_case('L','l').
convert_case('M','m').
convert_case('N','n').
convert_case('O','o').
convert_case('P','p').
convert_case('Q','q').
convert_case('R','r').
convert_case('S','s').
convert_case('T','t').
convert_case('U','u').
convert_case('V','v').
convert_case('W','w').
convert_case('X','x').
convert_case('Y','y').
convert_case('Z','z').

is_number_aux('0').
is_number_aux('1').
is_number_aux('2').
is_number_aux('3').
is_number_aux('4').
is_number_aux('5').
is_number_aux('6').
is_number_aux('7').
is_number_aux('8').
is_number_aux('9').

%% non_functional(Atom1):-
%%     Atom1=..[f,A,B],
%%     Atom2=..[f,A,C],
%%     call(Atom2),
%%     B \= C.

non_functional(Atom1):-
  Atom1 =..[P,s(In,Out1),s(_,[])],
  Atom2 =..[P,s(In,Out2),s(_,[])],
  call(Atom2),
  Out1 \= Out2,!.


%% test:-
%%     forall(pos(A),test_pos(A)),
%%     %forall(neg(A),test_neg(A)),
%%     halt.

%% target(P/A):-
%%     pos(Atom),!,
%%     functor(Atom,P,A).

%% target_exists:-
%%     target(P/A),
%%     current_predicate(P/A).

%% test_pos(A):-
%%     target_exists,
%%     call(A),!,
%%     writeln('acc,1').
%% test_pos(_):-
%%     writeln('acc,0').

%% test_neg(A):-
%%     target_exists,
%%     call(A),!,
%%     writeln('acc,0').
%% test_neg(_):-
%%     writeln('acc,1').
