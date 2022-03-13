
bk = """
my_succ(0,1).
my_succ(1,2).
my_succ(2,3).
my_succ(3,4).
my_succ(4,5).
c_pressButton(pressButton).
c_noop(noop).
c_player(player).
c1(1).
c2(2).
c3(3).
c4(4).
c5(5).
"""

tasks = []

# t1
task(
    bk = """
    does(player,pressButton).
    my_true_value(0).
    """,
    ex = """
    pos(next_value(5)).
    neg(next_value(0)).
    neg(next_value(1)).
    neg(next_value(2)).
    neg(next_value(3)).
    neg(next_value(4)).
""")

# t2
task(
    bk = """
    does(player,noop).
    my_true_value(3).
    """,
    exs = """
    pos(next_value(2)).
    neg(next_value(0)).
    neg(next_value(1)).
    neg(next_value(3)).
    neg(next_value(4)).
    neg(next_value(5)).
""")

# # t3
# does(3,player,pressButton).
# my_true_value(3,3).

# # t4
# my_true_value(4,4).
# does(4,player,pressButton).

# my_true_value(5,5).
# does(5,player,pressButton).

# my_true_value(6,2).
# does(6,player,noop).

# does(7,player,noop).

# my_true_value(8,2).
# does(8,player,pressButton).

# does(9,player,pressButton).






# pos(next_value(3,5)).
# pos(next_value(4,5)).
# pos(next_value(5,5)).
# pos(next_value(6,1)).
# pos(next_value(8,5)).
# pos(next_value(9,5)).

# neg(next_value(3,0)).
# neg(next_value(3,1)).
# neg(next_value(3,2)).
# neg(next_value(3,3)).
# neg(next_value(3,4)).
# neg(next_value(4,0)).
# neg(next_value(4,1)).
# neg(next_value(4,2)).
# neg(next_value(4,3)).
# neg(next_value(4,4)).
# neg(next_value(5,0)).
# neg(next_value(5,1)).
# neg(next_value(5,2)).
# neg(next_value(5,3)).
# neg(next_value(5,4)).
# neg(next_value(6,0)).
# neg(next_value(6,2)).
# neg(next_value(6,3)).
# neg(next_value(6,4)).
# neg(next_value(6,5)).
# neg(next_value(7,0)).
# neg(next_value(7,1)).
# neg(next_value(7,2)).
# neg(next_value(7,3)).
# neg(next_value(7,4)).
# neg(next_value(7,5)).
# neg(next_value(8,0)).
# neg(next_value(8,1)).
# neg(next_value(8,2)).
# neg(next_value(8,3)).
# neg(next_value(8,4)).
# neg(next_value(9,0)).
# neg(next_value(9,1)).
# neg(next_value(9,2)).
# neg(next_value(9,3)).
# neg(next_value(9,4)).