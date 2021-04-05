%% ORDER BY CLAUSE SIZE
%% p(A)<-q(A),r(A). (CLAUSE1)
%% p(A)<-s(A). (CLAUSE2)
%% V1
:-
    Clause2 > Clause1,
    not recursive_clause(Clause1,P,A),
    not recursive_clause(Clause2,P,A),
    head_literal(Clause1,P,A,Vars),
    head_literal(Clause2,P,A,Vars),
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    N1 > N2.

%% :-
%%     Clause2 > Clause1,
%%     recursive_clause(Clause1,P,A),
%%     recursive_clause(Clause2,P,A),
%%     head_literal(Clause1,P,A,Vars),
%%     head_literal(Clause2,P,A,Vars),
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 > N2.

%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #sum{A,P : body_literal(Clause,P,A,_)} == N.

%% %% ORDER CLAUSES BY NUMBER OF VARS
%% :-
%%     Clause2 > Clause1,
%%     not recursive_clause(Clause1,P,A),
%%     not recursive_clause(Clause2,P,A),
%%     head_literal(Clause1,P,A,Vars),
%%     head_literal(Clause2,P,A,Vars),
%%     clause_size(Clause1,N),
%%     clause_size(Clause2,N),
%%     num_vars(Clause1,NumVars1),
%%     num_vars(Clause2,NumVars2),
%%     NumVars1 > NumVars2.