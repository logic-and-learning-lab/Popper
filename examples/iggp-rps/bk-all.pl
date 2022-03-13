#defined does/2.

beats(paper,stone).
beats(scissors,paper).
beats(stone,scissors).

different(p1,p2).
different(p2,p1).

player(p1).
player(p2).

action(paper).
action(scissors).
action(stone).

my_succ(0,1).
my_succ(1,2).
my_succ(2,3).