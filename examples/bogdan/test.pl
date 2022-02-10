:- [bk].
:- [exs].

%% f0(A,B):-mk_uppercase(A,C),inv1(C,B).
%% inv1(A,B):-copyskip1(A,B), is_empty(B).
%% inv1(A,B):-copyskip1(A,C), inv1(C,B).

f0(A,B):-mk_uppercase(A,C),inv1(C,B).
inv1(A,B):-copyskip1(A,B),is_empty(B).
inv1(A,B):-copyskip1(A,C),inv1(C,B).


%% non_functional(Atom1):-
%%     Atom1=..[P,s(In,Out1),s(_,[])],
%%     Atom2=..[P,s(In,Out2),s(_,[])],
%%     call_with_timeout(Atom2),
%%     Out2 \= Out1,!.


non_functional:-
    pos(Atom),
    non_functional(Atom),!.

functional:-
    \+ non_functional.

%% a(X):-
    %% f0(s(['j', 'a', 'm', 'e', 's'],X),s(_,[])).