head_pred(ancestor, 2).
body_pred(mother, 2).
body_pred(father, 2).

%% enable_pi.
enable_recursion.

max_body(15).
%% max_vars(3).

% solution
% parent(X,Y) :- mother(X,Y).
% parent(X,Y) :- father(X,Y).
% ancestor(X,Y) :- parent(X,Y).
% ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).