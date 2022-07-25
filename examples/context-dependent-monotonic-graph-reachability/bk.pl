:- use_module(library(clpfd)).

%first graph
edge(g1,1,2).
edge(g1,2,3).
edge(g1,3,4).

edge(g1,5,6).
edge(g1,6,7).

%second graph
edge(g2,1,2).
edge(g2,2,3).
edge(g2,3,6).
edge(g2,2,4).
edge(g2,4,5).
edge(g2,4,2).
edge(g2,5,2).
edge(g2,6,1).

%triangle graph
edge(g3,1,2).
edge(g3,2,3).
edge(g3,3,1).

%non-context dependent BK
gt(A,B) :- A #> B.
