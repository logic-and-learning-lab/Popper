# Popper

Popper is an [inductive logic programming](https://arxiv.org/pdf/2008.07912.pdf) (ILP) system. 
Popper is still a **major** work-in-progress, so please notify us of bugs or usability issues.

If you use Popper for research, please cite the paper: Andrew Cropper and Rolf Morel. [Learning programs by learning from failures](https://arxiv.org/abs/2005.02259). Mach. Learn. 110(4): 801-856 (2021)


## Requirements

[SWI-Prolog](https://www.swi-prolog.org)

[Clingo 5.5.0](https://potassco.org/clingo/)

[pyswip](https://pypi.org/project/pyswip/)


# Usage

You can run Popper with the command `python popper.py <input dir>`
For instance, running the command `python popper.py examples/dropk` produces the output:

```prolog
f(A,B,C):-tail(A,C),one(B).
f(A,B,C):-decrement(B,E),f(A,E,D),tail(D,C).
% TP: 10, FN: 0, TN: 10, FP: 0
```

Running the command `python popper.py examples/trains` produces the output:

```prolog
f(A):-long(B),roof_closed(B),has_car(A,B),three_wheels(C),has_car(A,C).
% TP: 5, FN: 0, TN: 5, FP: 0
```

Take a look at the examples folder for examples.


# Example problem


Popper requires three files: 

- an examples file
- a background knowledge (BK) file
- a bias file

An examples file simply contains positive and negative examples of the relation you wish to learn:

```prolog
pos(grandparent(ann,amelia)).
pos(grandparent(steve,amelia)).
pos(grandparent(ann,spongebob)).
pos(grandparent(steve,spongebob)).
pos(grandparent(linda,amelia)).
neg(grandparent(amy,amelia)).
```

Likewise, a BK file contains helpful information about the relation you are trying to learn:

```prolog
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).
```

The bias file contains all the information necessary to restrict the search space of Popper. 

There two main things to add to this file are predicate declarations. These simply inform Popper whether it can use a predicate symbol in the head or body of a rule in a solution, such as:

```prolog
head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).
```

In other words, the above says each role in a program must have the symbol grandparent with arity two in the head and mother and/or father in the body, also with arity two.

Popper needs three parameters to restrict the search space:

- `max_vars(N).` sets the maximum number of variables allowed in a rule to be `N`
- `max_body(N).` sets the maximum number of body literals in a rule to be `N`
- `max_clauses(N).` sets the maximum number of rules/clause to be `N`

These parameters are important as they greatly influence the search space. If they are way too high then Popper will likely take a long time to learn a solution. If the settings are way too low then the search space might be too small to contain a good solution. We are currently working on techniques to automatically deduce these settings. But in the meantime finding the correct values can often be process of trial and error.


In our running example, we will add these three lines to our bias file:
```prolog
max_clauses(4).
max_vars(4).
max_body(3).
```

If we call Popper with these three files, then it will produce the output:

```prolog
grandparent(A,B):-mother(A,C),father(C,B).
grandparent(A,B):-father(A,C),mother(C,B).
grandparent(A,B):-father(A,C),father(C,B).
grandparent(A,B):-mother(A,C),mother(C,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```

# Predicate invention

Popper supports [automatic predicate invention](https://arxiv.org/pdf/2104.14426.pdf) (PI). To enable PI, add the setting `enable_pi.` to the bias file.
With PI enabled, Popper (`python popper.py examples/kinship-pi`) learns the following program:

```prolog
grandparent(A,B):-inv1(C,B),inv1(A,C).
inv1(A,B):-mother(A,B).
inv1(A,B):-father(A,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```

<!-- Popper can invent multiple levels of predicates. For instance, running `python popper.py examples/robots-pi` produces the output:

```prolog

``` -->

Predicate invention is currently very expensive so it is best to avoid it if possible.

# Anytime 

Popper is an anytime algorithm. To see the intermediate solutions use the `--info` flag. For instance, running the command `python popper.py examples/trains2 --info` produces the output:

```prolog
% NEW BEST PROG 1:
f(A):-short(B),has_car(A,B).
% Precision:0.80, Recall:0.86, TP:683, FN:109, TN:33, FP:175

% NEW BEST PROG 2:
f(A):-has_car(A,B),roof_closed(B).
% Precision:0.81, Recall:0.94, TP:745, FN:47, TN:36, FP:172

% NEW BEST PROG 3:
f(A):-roof_open(B),has_car(A,B).
% Precision:0.95, Recall:0.92, TP:731, FN:61, TN:172, FP:36

% NEW BEST PROG 68:
f(A):-has_car(A,C),roof_closed(C),has_car(A,B),roof_open(B).
f(A):-roof_open(C),has_car(A,C),three_load(B),has_load(C,B).
% Precision:1.00, Recall:0.91, TP:721, FN:71, TN:208, FP:0

% NEW BEST PROG 346:
f(A):-has_car(A,C),roof_closed(C),has_car(A,B),roof_open(B).
f(A):-has_load(E,D),rectangle(B),has_car(A,E),triangle(D),has_car(A,C),has_load(C,B).
% Precision:1.00, Recall:0.96, TP:761, FN:31, TN:208, FP:0

% BEST PROG 1252:
f(A):-roof_open(C),has_car(A,C),three_load(B),has_load(C,B).
f(A):-roof_open(C),has_car(A,C),has_car(A,B),roof_closed(B).
f(A):-rectangle(B),has_load(E,B),has_car(A,E),has_car(A,D),has_load(D,C),triangle(C).
% Precision:1.00, Recall:1.00, TP:792, FN:0, TN:208, FP:0
```

# Recursion
To enable recursion add `enable_recursion.` to the bias file.
This allows Popper to learn programs where a predicate symbol appears both in the head and body of a rule, such as to find a duplicate element (`python popper.py examples/find-dupl`) in a list:

```prolog
f(A,B):-head(A,B),tail(A,C),element(C,B).
f(A,B):-tail(A,C),f(C,B).
```

Or to filter (`python popper.py examples/filter`) even elements from a list:

```prolog
f(A,B):-empty(A),empty(B).
f(A,B):-tail(A,D),head(A,C),odd(C),f(D,B).
f(A,B):-head(A,E),even(E),tail(A,C),f(C,D),prepend(E,D,B).

```

# Types
Popper supports optional type annotations, which can be added to a bias file.
A type annotation is of the form `type(p,(t1,t2,...,tk)` for a predicate symbol `p` with arity `k`, such as:

```prolog
type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(empty,(list,)).
type(odd,(element,)).
type(even,(element,)).
type(prepend,(element,list,list)).
```

# Directions 
Prolog often require arguments to be ground.
For instance, when asking Prolog to answer the query:
```prolog
X is 3+K.
```
It throws an error:
```prolog
ERROR: Arguments are not sufficiently instantiated
```
Moreover, there are often cases where we want to reduce the number of answers from a query.
For instance, calling the `length` predicate with only variables leads to an infinite set of answers.

To make things easier, Popper supports optional direction annotations
A direction annotation is of the form `direction(p,(d1,d2,...,dk)` for a predicate symbol `p` with arity `k`, where each `di` is either `in` or `out`.
An `in` variable must be ground when calling the relation.
By contrast, an `out` variable need not be ground.
Here are example directions:

```prolog
direction(head,(in,out)).
direction(tail,(in,out)).
direction(length,(in,out)).
direction(prepend,(in,int,out)).
direction(geq,(in,in)).
```

# Parallelisation
[Coming soon](https://arxiv.org/pdf/2109.07132.pdf)

# Failure explanation
[Coming soon](https://arxiv.org/pdf/2102.12551.pdf)

# Popper settings

To run with statistics use the flag `--stats`.

To run in debug mode use the flag `--debug`.

To run in information mode use the flag `--info`.

To show the full hypothesis space (bounded by `N`) use the flag `--hspace N`.

To run with a maximum learning time use the flag `--timeout`.

To run with a maximum example testing time use the flag `--eval-timeout`.

To allow non-Datalog clauses, where a variable in the head need not appear in the body, add ``non_datalog.` to your bias file.

To allow singleton variables (variables that only appear once in a clause), add `allow_singletons.` to your bias file.
