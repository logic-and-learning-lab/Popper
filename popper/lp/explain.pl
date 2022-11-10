#show selected/1.
#heuristic size(N). [1000-N,true]

size(N):- #count{ID : selected(ID)} == N.

:- size(0).

%% every head literal must have a body literal
:- head_literal(Rule,_,_,_), not body_literal(Rule,_,_,_).

num_rules(N):- #count{Rule : body_literal(Rule,_,_,_)} == N.