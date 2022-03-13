output(A,B,C):- succ(B,D),input(D,F,C),input(A,F,E),cyan(E).
output(A,B,C):- input(A,E,F),input(D,B,C),input(D,E,F),blue(F).
