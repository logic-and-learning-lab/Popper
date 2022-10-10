head_pred(sameclique, 2).
body_pred(edge, 2).

enable_pi.
enable_recursion.
max_body(3).

%solution
%reachable(X,Y) :- edge(X,Y).
%reachable(X,Y) :- edge(X,Z), reachable(Z,Y).
%same_clique(X,Y) :- reachable(X,Y), reachable(Y,X).