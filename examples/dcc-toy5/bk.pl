:-table (f/1) as incremental.
:-dynamic([f/1], [incremental(true)]).

tail([_|T],T).
head([H|_],H).
empty([]).

c0(0).
c1(1).
c2(2).
c3(3).
c4(4).
c5(5).
c6(6).
c7(7).
c8(8).
c9(9).
c10(10).
