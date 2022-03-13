:-table (out/3) as incremental.
:-dynamic([out/3], [incremental(true)]).

succ(1,2).
succ(2,3).
succ(3,4).
succ(4,5).
succ(5,5).

in(e1,1,1).
in(e2,1,1).