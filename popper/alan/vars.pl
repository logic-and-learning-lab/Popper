var(0..N-1):-
    max_vars(N).

clause_var(Clause,Var):-
    head_var(Clause,Var).
clause_var(Clause,Var):-
    body_var(Clause,Var).

head_var(Clause,Var):-
    head_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).
body_var(Clause,Var):-
    body_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).

%% VAR IS IN VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% VAR IS IN A LITERAL
var_in_literal(Clause,P,Vars,Var):-
    literal(Clause,P,Vars),
    var_member(Var,Vars).

%% TODO: GENERALISE FOR ARITIES > 4
head_vars(1,(0,)):-
    modeh(_,1).
head_vars(2,(0,1)):-
    modeh(_,2).
head_vars(3,(0,1,2)):-
    modeh(_,3).
head_vars(4,(0,1,2,3)):-
    modeh(_,4).

need_arity(A):-
    modeh(_,A).
need_arity(A):-
    modeb(_,A).

%% POSSIBLE VARIABLE COMBINATIONS
%% TODO: GENERALISE
vars(1,(Var1,)):-
    need_arity(1),
    var(Var1).
vars(2,(Var1,Var2)):-
    need_arity(2),
    var(Var1),
    var(Var2),
    Var1 != Var2.
vars(3,(Var1,Var2,Var3)):-
    need_arity(3),
    var(Var1),
    var(Var2),
    var(Var3),
    Var1 != Var2,
    Var1 != Var3,
    Var2 != Var3.
vars(4,(Var1,Var2,Var3,Var4)):-
    need_arity(4),
    var(Var1),
    var(Var2),
    var(Var3),
    var(Var4),
    Var1 != Var2,
    Var1 != Var3,
    Var1 != Var4,
    Var2 != Var3,
    Var2 != Var4,
    Var3 != Var4.

%% POSITION OF VAR IN VARS
%% TODO: GENERALISE
var_pos(Var1,(Var1,),0):-
    vars(1,(Var1,)).
var_pos(Var1,(Var1,Var2),0):-
    vars(2,(Var1,Var2)).
var_pos(Var2,(Var1,Var2),1):-
    vars(2,(Var1,Var2)).
var_pos(Var1,(Var1,Var2,Var3),0):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var2,(Var1,Var2,Var3),1):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var3,(Var1,Var2,Var3),2):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var1,(Var1,Var2,Var3,Var4),0):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var2,(Var1,Var2,Var3,Var4),1):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var3,(Var1,Var2,Var3,Var4),2):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var4,(Var1,Var2,Var3,Var4),3):-
    vars(4,(Var1,Var2,Var3,Var4)).

mode(P,A):-
    modeh(P,A).
mode(P,A):-
    modeb(P,A).

%% max_arity(N):-
%%     #max{A : mode(_,A)} == N.

%% body_sizes(1..N):-
%%     max_body(N).

%% %% TODO: REFACTOR HORRIBLE CODE
%% max_var(BodySize,Var):-
%%     body_sizes(BodySize),
%%     var(Var),
%%     Var = (BodySize*(A-1)),
%%     max_body(N),
%%     max_arity(A).
%% max_var(BodySize,Var):-
%%     body_sizes(BodySize),
%%     max_arity(A),
%%     not var(BodySize*(A-1)),
%%     max_vars(Var).

%% bounded_vars(MaxVar,Vars):-
%%     vars(_,Vars),
%%     var(MaxVar),
%%     #max{Var : var_member(Var,Vars)} == N,
%%     N <= MaxVar.
