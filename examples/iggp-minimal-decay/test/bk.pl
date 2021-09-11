%% STATICS
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

%% DYNAMICS
does(1,player,noop).
does(2,player,noop).
does(3,player,pressButton).
my_true_value(1,4).
my_true_value(2,0).
my_true_value(3,1).