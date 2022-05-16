constant_aux(cell_type, 4).
constant_aux(cell_type, 5).
constant_aux(cell_type, 6).
constant_aux(cell_type, 7).
constant_aux(cell_type, 8).
constant_aux(cell_type, b).

different_cell_type(A,B):-
    constant_aux(cell_type,A),
    constant_aux(cell_type,B).
    %% A < B.

pos(1..3).
different_pos(A,B):-
    pos(A),
    pos(B).
    %% A < B.

ct_4(4).
ct_5(5).
ct_6(6).
ct_7(7).
ct_8(8).
ct_b(b).
robot(robot).

my_index(1).
my_index(2).
my_index(3).
my_input_move(robot,1,1).
my_input_move(robot,1,2).
my_input_move(robot,1,3).
my_input_move(robot,2,1).
my_input_move(robot,2,2).
my_input_move(robot,2,3).
my_input_move(robot,3,1).
my_input_move(robot,3,2).
my_input_move(robot,3,3).
my_succ(1,2).
my_succ(2,3).
my_successor(0,1).
my_successor(1,2).
my_successor(10,11).
my_successor(11,12).
my_successor(12,13).
my_successor(13,14).
my_successor(14,15).
my_successor(15,16).
my_successor(16,17).
my_successor(17,18).
my_successor(18,19).
my_successor(19,20).
my_successor(2,3).
my_successor(20,21).
my_successor(21,22).
my_successor(22,23).
my_successor(23,24).
my_successor(24,25).
my_successor(25,26).
my_successor(26,27).
my_successor(27,28).
my_successor(28,29).
my_successor(29,30).
my_successor(3,4).
my_successor(30,31).
my_successor(31,32).
my_successor(32,33).
my_successor(33,34).
my_successor(34,35).
my_successor(35,36).
my_successor(36,37).
my_successor(37,38).
my_successor(38,39).
my_successor(39,40).
my_successor(4,5).
my_successor(40,41).
my_successor(41,42).
my_successor(42,43).
my_successor(43,44).
my_successor(44,45).
my_successor(45,46).
my_successor(46,47).
my_successor(47,48).
my_successor(48,49).
my_successor(49,50).
my_successor(5,6).
my_successor(6,7).
my_successor(7,8).
my_successor(8,9).
my_successor(9,10).
role(robot).
scoremap(26,100).
scoremap(27,100).
scoremap(28,100).
scoremap(29,100).
scoremap(30,100).
scoremap(31,98).
scoremap(32,96).
scoremap(33,94).
scoremap(34,92).
scoremap(35,90).
scoremap(36,88).
scoremap(37,86).
scoremap(38,84).
scoremap(39,82).
scoremap(40,80).
scoremap(41,78).
scoremap(42,76).
scoremap(43,74).
scoremap(44,72).
scoremap(45,70).
scoremap(46,68).
scoremap(47,66).
scoremap(48,64).
scoremap(49,62).
scoremap(50,60).
tile(1).
tile(2).
tile(3).
tile(4).
tile(5).
tile(6).
tile(7).
tile(8).
tile(b).