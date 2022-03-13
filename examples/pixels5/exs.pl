%% :-table (out/3) as incremental.
%% :-dynamic([out/3], [incremental(true)]).

%% succ(1,2).
%% succ(2,3).
%% succ(3,4).
%% succ(4,5).
%% succ(5,5).

%% 0000
%% 0000
%% 0100
%% 0000
%% =>
%% 0000
%% 0100
%% 1110
%% 0100

%% in(e1,2,2).
%% =>


%% ----------


%% in(e2,3,4).

%% in(e2,1,2).
%% in(e2,2,3).
%% %% =>
pos(out(e2,1,2)).
pos(out(e2,2,3)).
pos(out(e2,3,4)).
pos(out(e2,4,5)).
pos(out(e1,2,2)).
pos(out(e1,1,1)).
pos(out(e1,3,3)).
pos(out(e1,4,4)).
pos(out(e1,5,5)).

neg(out(e2,1,1)).
neg(out(e2,2,2)).
neg(out(e2,1,3)).
neg(out(e2,4,3)).
neg(out(e2,1,1)).
neg(out(e2,4,2)).
neg(out(e2,4,1)).
neg(out(e2,3,2)).
neg(out(e1,1,2)).
neg(out(e1,1,4)).
neg(out(e1,1,3)).
neg(out(e1,4,1)).
neg(out(e1,3,3)).



%% %% ----------

%% %% 0000
%% %% 0000
%% %% 0000
%% %% 0001
%% %% =>
%% %% 0000
%% %% 0000
%% %% 0001
%% %% 0011

%% %% in(e3,4,1).
%% %% =>
%% pos(out(e3,3,1)).
%% pos(out(e3,4,1)).
%% pos(out(e3,4,2)).

%% neg(out(e3,1,3)).
%% neg(out(e3,4,3)).
%% neg(out(e3,1,1)).
%% neg(out(e3,4,4)).
%% neg(out(e3,3,3)).
%% neg(out(e3,2,2)).
%% neg(out(e3,1,4)).
%% neg(out(e3,2,4)).
%% neg(out(e3,3,2)).
