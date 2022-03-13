succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,6).
succ(6,7).
succ(7,8).
succ(8,9).

black(black).
blue(blue).
cyan(cyan).
red(red).
orange(orange).
yellow(yellow).

iblack(A,B):-
    input(A,B,black).
iblue(A,B):-
    input(A,B,blue).
ired(A,B):-
    input(A,B,red).
iorange(A,B):-
    input(A,B,orange).
iyellow(A,B):-
    input(A,B,yellow).
icyan(A,B):-
    input(A,B,cyan).