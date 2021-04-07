max_clauses(2).
max_vars(5).
max_body(4).

modeh(next_value,2).
modeb(does,3).
modeb(my_true_value,2).
modeb(my_succ,2).

modeb(c_pressButton,1).
modeb(c_noop,1).
modeb(c1,1).
modeb(c2,1).
modeb(c3,1).
modeb(c4,1).
modeb(c5,1).

type(next_value,0,ex).
type(next_value,1,int).
type(does,0,ex).
type(does,1,agent).
type(does,2,action).
type(my_true_value,0,ex).
type(my_true_value,1,int).
type(my_succ,0,int).
type(my_succ,1,int).

type(c_pressButton,0,action).
type(c_noop,0,action).

type(c1,0,int).
type(c2,0,int).
type(c3,0,int).
type(c4,0,int).
type(c5,0,int).

:-
    body_literal(C,P1,A,1),
    body_literal(C,P2,A,1),
    type(P1,0,int),
    P1 != P2.

%% next_value(A) :-
%%     does(B,C),
%%     pressButton(C),
%%     c5(A).
%% next_value(A) :-
%%     succ(A,B),
%%     true_value(B),
%%     does(C,D),
%%     c_noop(D).