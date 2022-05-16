c1(1).
c2(2).
c3(3).
c4(4).
c5(5).
c6(6).
c7(7).
c8(8).
c9(9).

close(1,2).
close(2,1).
close(2,3).
close(3,2).
close(3,4).
close(4,3).
close(4,5).
close(5,4).
close(5,6).
close(6,5).
close(6,7).
close(7,6).
close(7,8).
close(8,7).

my_index(1).
my_index(2).
my_index(3).
my_index(4).
my_index(5).
my_index(6).
my_index(7).
my_index(8).

agent(red).
agent(blue).
pos(1).
pos(2).
pos(3).
pos(4).
pos(5).
pos(6).
pos(7).
pos(8).
int(0).
int(50).
int(100).

not_my_true_isplayer(A,B,C):-
    pos(A),
    pos(B),
    agent(C),
    not my_true_isplayer(A,B,C).

red(red).
blue(blue).
c1(1).
c2(2).
c3(3).
c4(4).
c5(5).
c6(6).
c7(7).
c8(8).
c0(0).
c50(50).
c100(100).

my_input_move(blue, 1, 1).
my_input_move(blue, 1, 2).
my_input_move(blue, 1, 3).
my_input_move(blue, 1, 4).
my_input_move(blue, 1, 5).
my_input_move(blue, 1, 6).
my_input_move(blue, 1, 7).
my_input_move(blue, 1, 8).
my_input_move(blue, 2, 1).
my_input_move(blue, 2, 2).
my_input_move(blue, 2, 3).
my_input_move(blue, 2, 4).
my_input_move(blue, 2, 5).
my_input_move(blue, 2, 6).
my_input_move(blue, 2, 7).
my_input_move(blue, 2, 8).
my_input_move(blue, 3, 1).
my_input_move(blue, 3, 2).
my_input_move(blue, 3, 3).
my_input_move(blue, 3, 4).
my_input_move(blue, 3, 5).
my_input_move(blue, 3, 6).
my_input_move(blue, 3, 7).
my_input_move(blue, 3, 8).
my_input_move(blue, 4, 1).
my_input_move(blue, 4, 2).
my_input_move(blue, 4, 3).
my_input_move(blue, 4, 4).
my_input_move(blue, 4, 5).
my_input_move(blue, 4, 6).
my_input_move(blue, 4, 7).
my_input_move(blue, 4, 8).
my_input_move(blue, 5, 1).
my_input_move(blue, 5, 2).
my_input_move(blue, 5, 3).
my_input_move(blue, 5, 4).
my_input_move(blue, 5, 5).
my_input_move(blue, 5, 6).
my_input_move(blue, 5, 7).
my_input_move(blue, 5, 8).
my_input_move(blue, 6, 1).
my_input_move(blue, 6, 2).
my_input_move(blue, 6, 3).
my_input_move(blue, 6, 4).
my_input_move(blue, 6, 5).
my_input_move(blue, 6, 6).
my_input_move(blue, 6, 7).
my_input_move(blue, 6, 8).
my_input_move(blue, 7, 1).
my_input_move(blue, 7, 2).
my_input_move(blue, 7, 3).
my_input_move(blue, 7, 4).
my_input_move(blue, 7, 5).
my_input_move(blue, 7, 6).
my_input_move(blue, 7, 7).
my_input_move(blue, 7, 8).
my_input_move(blue, 8, 1).
my_input_move(blue, 8, 2).
my_input_move(blue, 8, 3).
my_input_move(blue, 8, 4).
my_input_move(blue, 8, 5).
my_input_move(blue, 8, 6).
my_input_move(blue, 8, 7).
my_input_move(blue, 8, 8).
my_input_move(red, 1, 1).
my_input_move(red, 1, 2).
my_input_move(red, 1, 3).
my_input_move(red, 1, 4).
my_input_move(red, 1, 5).
my_input_move(red, 1, 6).
my_input_move(red, 1, 7).
my_input_move(red, 1, 8).
my_input_move(red, 2, 1).
my_input_move(red, 2, 2).
my_input_move(red, 2, 3).
my_input_move(red, 2, 4).
my_input_move(red, 2, 5).
my_input_move(red, 2, 6).
my_input_move(red, 2, 7).
my_input_move(red, 2, 8).
my_input_move(red, 3, 1).
my_input_move(red, 3, 2).
my_input_move(red, 3, 3).
my_input_move(red, 3, 4).
my_input_move(red, 3, 5).
my_input_move(red, 3, 6).
my_input_move(red, 3, 7).
my_input_move(red, 3, 8).
my_input_move(red, 4, 1).
my_input_move(red, 4, 2).
my_input_move(red, 4, 3).
my_input_move(red, 4, 4).
my_input_move(red, 4, 5).
my_input_move(red, 4, 6).
my_input_move(red, 4, 7).
my_input_move(red, 4, 8).
my_input_move(red, 5, 1).
my_input_move(red, 5, 2).
my_input_move(red, 5, 3).
my_input_move(red, 5, 4).
my_input_move(red, 5, 5).
my_input_move(red, 5, 6).
my_input_move(red, 5, 7).
my_input_move(red, 5, 8).
my_input_move(red, 6, 1).
my_input_move(red, 6, 2).
my_input_move(red, 6, 3).
my_input_move(red, 6, 4).
my_input_move(red, 6, 5).
my_input_move(red, 6, 6).
my_input_move(red, 6, 7).
my_input_move(red, 6, 8).
my_input_move(red, 7, 1).
my_input_move(red, 7, 2).
my_input_move(red, 7, 3).
my_input_move(red, 7, 4).
my_input_move(red, 7, 5).
my_input_move(red, 7, 6).
my_input_move(red, 7, 7).
my_input_move(red, 7, 8).
my_input_move(red, 8, 1).
my_input_move(red, 8, 2).
my_input_move(red, 8, 3).
my_input_move(red, 8, 4).
my_input_move(red, 8, 5).
my_input_move(red, 8, 6).
my_input_move(red, 8, 7).
my_input_move(red, 8, 8).