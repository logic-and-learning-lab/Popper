%% pos(f(p1a,1)).
%% pos(f(p1b,0)).
%% neg(f(p1a,0)).
%% neg(f(p1b,1)).

does(1,p1,stone).
does(1,p2,scissors).
my_true_score(1,p1,0).
my_true_score(1,p2,0).

%% wins(p2a)
%% wins(p2b)
%% wins(p1b)
%% wins(p1a)

%% pos(f(p2a,2)).
%% pos(f(p2b,3)).
%% neg(f(p2a,3)).
%% neg(f(p2b,2)).

does(2,p1,paper).
does(2,p2,scissors).
my_true_score(2,p1,2).
my_true_score(2,p2,2).


wins(E,A):-
    does(E,A,B),
    does(E,C,D),
    beats(B,D),
    different(A,C).

my_succ(0,1).
my_succ(1,2).
my_succ(2,3).


beats(paper,stone).
beats(scissors,paper).
beats(stone,scissors).

%% different(p1,p2).
%% different(p2,p1).

different(A,B):- A \= B.
%% same(A,A):- does(A,_).
%% different(A,B):-
%%     does(_,A,_),
%%     does(_,B,_),
%%     A != B.

%% f(E,A,B):- my_true_score(E,A,C), my_succ(C,B), wins(E,A).
%% f(E,A,B):- my_true_score(E,A,B), wins(E,C), different(C,A).
