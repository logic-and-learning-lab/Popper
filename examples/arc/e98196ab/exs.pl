#T1
pos(output(1, 1, blue)).
%% pos(output(1, 2, black)).
%% pos(output(1, 3, black)).
%% pos(output(1, 4, black)).
%% pos(output(1, 5, black)).
pos(output(10, 1, cyan)).
%% pos(output(10, 2, black)).
%% pos(output(10, 3, black)).
%% pos(output(10, 4, black)).
%% pos(output(10, 5, black)).
pos(output(11, 1, blue)).
%% pos(output(11, 2, black)).
%% pos(output(11, 3, black)).
%% pos(output(11, 4, black)).
%% pos(output(11, 5, black)).
pos(output(2, 1, cyan)).
%% pos(output(2, 2, black)).
%% pos(output(2, 3, black)).
%% pos(output(2, 4, black)).
%% pos(output(2, 5, black)).
%% pos(output(3, 1, black)).
pos(output(3, 2, blue)).
%% pos(output(3, 3, black)).
%% pos(output(3, 4, black)).
%% pos(output(3, 5, black)).
%% pos(output(4, 1, black)).
%% pos(output(4, 2, black)).
pos(output(4, 3, cyan)).
%% pos(output(4, 4, black)).
%% pos(output(4, 5, black)).
%% pos(output(5, 1, black)).
%% pos(output(5, 2, black)).
%% pos(output(5, 3, black)).
pos(output(5, 4, blue)).
%% pos(output(5, 5, black)).
%% pos(output(6, 1, black)).
%% pos(output(6, 2, black)).
%% pos(output(6, 3, black)).
%% pos(output(6, 4, black)).
pos(output(6, 5, cyan)).
%% pos(output(7, 1, black)).
%% pos(output(7, 2, black)).
%% pos(output(7, 3, black)).
pos(output(7, 4, blue)).
%% pos(output(7, 5, black)).
%% pos(output(8, 1, black)).
%% pos(output(8, 2, black)).
pos(output(8, 3, cyan)).
%% pos(output(8, 4, black)).
%% pos(output(8, 5, black)).
%% pos(output(9, 1, black)).
pos(output(9, 2, blue)).
%% pos(output(9, 3, black)).
%% pos(output(9, 4, black)).
%% pos(output(9, 5, black)).

neg(output(1,1,black)).
neg(output(1,1,cyan)).
neg(output(1,2,blue)).
neg(output(1,2,cyan)).
neg(output(1,3,blue)).
neg(output(1,3,cyan)).
neg(output(1,4,blue)).
neg(output(1,4,cyan)).
neg(output(1,5,blue)).
neg(output(1,5,cyan)).
neg(output(10,1,black)).
neg(output(10,1,blue)).
neg(output(10,2,blue)).
neg(output(10,2,cyan)).
neg(output(10,3,blue)).
neg(output(10,3,cyan)).
neg(output(10,4,blue)).
neg(output(10,4,cyan)).
neg(output(10,5,blue)).
neg(output(10,5,cyan)).
neg(output(11,1,black)).
neg(output(11,1,cyan)).
neg(output(11,2,blue)).
neg(output(11,2,cyan)).
neg(output(11,3,blue)).
neg(output(11,3,cyan)).
neg(output(11,4,blue)).
neg(output(11,4,cyan)).
neg(output(11,5,blue)).
neg(output(11,5,cyan)).
neg(output(2,1,black)).
neg(output(2,1,blue)).
neg(output(2,2,blue)).
neg(output(2,2,cyan)).
neg(output(2,3,blue)).
neg(output(2,3,cyan)).
neg(output(2,4,blue)).
neg(output(2,4,cyan)).
neg(output(2,5,blue)).
neg(output(2,5,cyan)).
neg(output(3,1,blue)).
neg(output(3,1,cyan)).
neg(output(3,2,black)).
neg(output(3,2,cyan)).
neg(output(3,3,blue)).
neg(output(3,3,cyan)).
neg(output(3,4,blue)).
neg(output(3,4,cyan)).
neg(output(3,5,blue)).
neg(output(3,5,cyan)).
neg(output(4,1,blue)).
neg(output(4,1,cyan)).
neg(output(4,2,blue)).
neg(output(4,2,cyan)).
neg(output(4,3,black)).
neg(output(4,3,blue)).
neg(output(4,4,blue)).
neg(output(4,4,cyan)).
neg(output(4,5,blue)).
neg(output(4,5,cyan)).
neg(output(5,1,blue)).
neg(output(5,1,cyan)).
neg(output(5,2,blue)).
neg(output(5,2,cyan)).
neg(output(5,3,blue)).
neg(output(5,3,cyan)).
neg(output(5,4,black)).
neg(output(5,4,cyan)).
neg(output(5,5,blue)).
neg(output(5,5,cyan)).
neg(output(6,1,blue)).
neg(output(6,1,cyan)).
neg(output(6,2,blue)).
neg(output(6,2,cyan)).
neg(output(6,3,blue)).
neg(output(6,3,cyan)).
neg(output(6,4,blue)).
neg(output(6,4,cyan)).
neg(output(6,5,black)).
neg(output(6,5,blue)).
neg(output(7,1,blue)).
neg(output(7,1,cyan)).
neg(output(7,2,blue)).
neg(output(7,2,cyan)).
neg(output(7,3,blue)).
neg(output(7,3,cyan)).
neg(output(7,4,black)).
neg(output(7,4,cyan)).
neg(output(7,5,blue)).
neg(output(7,5,cyan)).
neg(output(8,1,blue)).
neg(output(8,1,cyan)).
neg(output(8,2,blue)).
neg(output(8,2,cyan)).
neg(output(8,3,black)).
neg(output(8,3,blue)).
neg(output(8,4,blue)).
neg(output(8,4,cyan)).
neg(output(8,5,blue)).
neg(output(8,5,cyan)).
neg(output(9,1,blue)).
neg(output(9,1,cyan)).
neg(output(9,2,black)).
neg(output(9,2,cyan)).
neg(output(9,3,blue)).
neg(output(9,3,cyan)).
neg(output(9,4,blue)).
neg(output(9,4,cyan)).
neg(output(9,5,blue)).
neg(output(9,5,cyan)).


%% x(A):-
%%     pos(output(A,_,_)).
%% y(A):-
%%     pos(output(_,A,_)).
%% c(A):-
%%     pos(output(_,_,A)).

%% neg(output(A,B,C)):-
%%     x(A),
%%     y(B),
%%     c(C),
%%     %% pos(output(A,_,_)),
%%     %% pos(output(_,B,_)),
%%     %% pos(output(_,_,C)),
%%     not pos(output(A,B,C)).

