%% ALAN12
#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

#show head_literal/4.
#show body_literal/4.

#include "pi.pl".
#include "types.pl".
#include "recursion.pl".
#include "subsumption.pl".
#include "clauses.pl".
#include "direction.pl".
#include "bias.pl".
#include "vars.pl".

%% GUESS A SINGLE HEAD LITERAL
0 {head_literal(Clause,P,A,Vars) : modeh(P,A), head_vars(A,Vars)} 1:-
    Clause = 0..N-1,
    max_clauses(N).

%% GUESS AT LEAST 1 BUT AT MOST N BODY LITERALS PER CLAUSE
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).

%% THERE IS A CLAUSE IF THERE IS A HEAD LITERAL
clause(Clause):-
    head_literal(Clause,_,_,_).

%% CALC BODY SIZE
%% TODO: RENAME TO BODY_SIZE
%% TODO: IMPROVE THIS IS VERY EXPENSIVE
%% OLD VERSION
clause_size(Clause,N):-
    clause(Clause),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(Clause,P,_,Vars)} = N.

%% %% NEW VERSION
%% clause_size(Clause,BodySize):-
%%     clause(Clause),
%%     max_var(BodySize,MaxVar),
%%     #count{P,Vars :
%%         body_literal(Clause,P,_,Vars),
%%         bounded_vars(MaxVar,Vars)
%%     } == BodySize.

num_clauses(P,N):-
    head_literal(_,P,_,_),
    #count{C : head_literal(C,P,_,_)} == N.

literal(Clause,P,Vars):-
    head_literal(Clause,P,_,Vars).
literal(Clause,P,Vars):-
    body_literal(Clause,P,_,Vars).

%% ENSURE A CLAUSE
:-
    not clause(0).

%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).

%% USE CLAUSES IN ORDER
:-
    clause(Clause),
    Clause > 1,
    not clause(Clause-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(Clause,Var),
    Var > 1,
    not clause_var(Clause,Var-1).

before(C1,C2):-
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    lower(P,Q).

before(C1,C2):-
    head_literal(C1,P,_,_),
    head_literal(C2,P,_,_),
    not recursive_clause(C1,P,A),
    recursive_clause(C2,P,A).

count_lower(P,N):-
    head_literal(_,P,_,_),
    #count{Q : lower(Q,P)} == N.

min_clause(C,N+1):-
    recursive_clause(C,P,A),
    count_lower(P,N).

min_clause(C,N):-
    head_literal(C,P,A,_),
    not recursive_clause(C,P,A),
    count_lower(P,N).

%% #show before/2.
%% #show min_clause/2.
