%% python3 popper.py examples/reverse --eval-timeout=0.01
%% f(A,B) :- empty(B),empty(A).
%% f(A,B) :- head(A,C),tail(A,E),f(E,D),my_append(D,C,B).
%% 8.89s user 0.12s system 99% cpu 9.013 total

max_vars(5).
max_body(5).
max_clauses(2).
enable_recursion.

head_pred(f,2).
body_pred(head,2).
body_pred(tail,2).
body_pred(my_append,3).
body_pred(empty,1).

type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(my_append,(list,element,list)).
type(empty,(list,)).

direction(f,(in,out)).
direction(head,(in,out)).
direction(tail,(in,out)).
direction(my_append,(in,in,out)).
direction(empty,(out,)).