# POPPER IS STILL A MAJOR WORK-IN-PROGRESS. BE PREPARED FOR BUGS, DEMONS, AND BACKWARDS-BREAKING CHANGES!

# Popper

Popper is an inductive logic programming (ILP) system.
If you use Popper for research, please cite the paper [learning programs by learning from failures](https://arxiv.org/abs/2005.02259).


## Requirements

[SWI-Prolog](https://www.swi-prolog.org)

[Clingo 5.5.0](https://potassco.org/clingo/)

[pyswip](https://pypi.org/project/pyswip/)


# Usage

You can run Popper like so:

`python popper.py examples/dropk/`

which should print

```prolog
f(A,B,C) :- one(B),tail(A,C).
f(A,B,C) :- tail(A,E),decrement(B,D),f(E,D,C).
```

Take a look at the examples folder for examples.

# Popper settings

Popper currently needs three parameters to restrict the language:

`max_vars(N).` sets the maximum number of variables allowed in a rule to be `N`

`max_body(N).` sets the maximum number of body literals in a rule to be `N`

`max_clauses(N).` sets the maximum number of rules/clause to be `N`

To enable predicate invention add `enable_pi.` to the bias file.

To enable recursion add `enable_recursion.` to the bias file.

By default, Popper does not test all the examples during the testing stage. To do so, call Popper with the `--test-all` flag.


```prolog
f(A,B,C) :- one(B),tail(A,C).
f(A,B,C) :- tail(A,E),decrement(B,D),f(E,D,C).
```

To run with a maximum learning time use the flag `--eval-timeout`.

To run in debug mode use the flag `--debug`

To run with statistics use the flag `--stats`


