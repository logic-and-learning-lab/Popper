#defined direction/3.
#defined vars/2.
%% #show head_literal/4.
%% #show body_literal/4.
#show selected/1.
%% #show size/1.

%% #heuristic size(N). [1000-N,true]

size(N):- #count{ID : selected(ID)} == N.

%% need to select something
:- size(0).

%% every head literal must have a body literal
:- head_literal(Rule,_,_,_), #count{P,Vars : body_literal(Rule,P,_,Vars)} == 0.