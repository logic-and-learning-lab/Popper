%% STATICS
my_succ(0,1).
my_succ(1,2).
my_succ(2,3).
player(p1).
player(p2).
beats(paper,stone).
beats(scissors,paper).
beats(stone,scissors).

beats(paper,stone).
beats(scissors,paper).
beats(stone,scissors).

different(p1,p2).
different(p2,p1).

c_p1(p1).
c_p2(p2).

player(p1).
player(p2).

action(paper).
action(scissors).
action(stone).

draws(E,A):-
    does(E,A,B),
    does(E,C,B),
    different(A,C).

wins(E,A):-
    does(E,A,B),
    does(E,C,D),
    beats(B,D),
    player(C).

loses(E,A):-
    does(E,A,B),
    does(E,C,D),
    beats(D,B),
    player(C).

%% DYNAMICS
does(11,p1,stone).
does(11,p2,stone).
does(13,p1,scissors).
does(13,p2,paper).
does(15,p1,scissors).
does(15,p2,paper).
does(16,p1,scissors).
does(16,p2,stone).
does(2,p1,paper).
does(2,p2,stone).
does(3,p1,stone).
does(3,p2,scissors).
does(4,p1,stone).
does(4,p2,paper).
does(5,p1,stone).
does(5,p2,paper).
does(6,p1,scissors).
does(6,p2,stone).
does(7,p1,stone).
does(7,p2,scissors).
does(8,p1,paper).
does(8,p2,scissors).
does(9,p1,stone).
does(9,p2,scissors).
my_true_score(1,p1,2).
my_true_score(1,p2,0).
my_true_score(10,p1,2).
my_true_score(10,p2,1).
my_true_score(11,p1,0).
my_true_score(11,p2,2).
my_true_score(12,p1,0).
my_true_score(12,p2,3).
my_true_score(13,p1,0).
my_true_score(13,p2,0).
my_true_score(14,p1,1).
my_true_score(14,p2,0).
my_true_score(15,p1,0).
my_true_score(15,p2,1).
my_true_score(16,p1,1).
my_true_score(16,p2,0).
my_true_score(2,p1,2).
my_true_score(2,p2,0).
my_true_score(3,p1,0).
my_true_score(3,p2,0).
my_true_score(4,p1,0).
my_true_score(4,p2,1).
my_true_score(5,p1,1).
my_true_score(5,p2,0).
my_true_score(6,p1,2).
my_true_score(6,p2,0).
my_true_score(7,p1,1).
my_true_score(7,p2,1).
my_true_score(8,p1,1).
my_true_score(8,p2,1).
my_true_score(9,p1,1).
my_true_score(9,p2,0).
my_true_step(1,3).
my_true_step(10,3).
my_true_step(11,2).
my_true_step(12,3).
my_true_step(13,1).
my_true_step(14,3).
my_true_step(15,2).
my_true_step(16,1).
my_true_step(2,2).
my_true_step(3,2).
my_true_step(4,1).
my_true_step(5,2).
my_true_step(6,2).
my_true_step(7,2).
my_true_step(8,2).
my_true_step(9,1).
