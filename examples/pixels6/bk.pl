:-table (out/4) as incremental.
:-dynamic([out/4], [incremental(true)]).

succ(0,1).
succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).


black(black).
blue(blue).


in(e1,1,1,black).
%% pos(out(e1,2,1,blue)).