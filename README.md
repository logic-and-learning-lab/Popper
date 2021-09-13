# POPPER IS STILL A MAJOR WORK-IN-PROGRESS. BE PREPARED FOR BUGS, DEMONS, AND BACKWARDS-BREAKING CHANGES!

# Popper

Popper is an inductive logic programming (ILP) system.
If you use Popper for research, please cite the paper [learning programs by learning from failures](https://arxiv.org/abs/2005.02259).


## Requirements

[SWI-Prolog](https://www.swi-prolog.org)

[Clingo 5.5.0](https://potassco.org/clingo/)

[pyswip](https://pypi.org/project/pyswip/)


# Usage

You can run Popper `python popper.py <input dir>`.

For instance, running the command `python popper.py examples/dropk` produces the output:

```prolog
BEST PROGRAM:
f(A,B,C):-tail(A,C),one(B)
f(A,B,C):-f(A,E,D),tail(D,C),decrement(B,E)
TP: 10, FN: 0, TN: 0+, FP: 0+
```

Running the command `python popper.py examples/trains` produces the output:

```prolog
BEST PROGRAM:
f(A):-three_wheels(B),long(C),roof_closed(C),has_car(A,B),has_car(A,C)
TP: 5, FN: 0, TN: 0+, FP: 0+
```

Take a look at the examples folder for examples.

Note that in the above output, some of the outcomes are `N+`. The reason is that, by default, Popper does not test all the examples during the testing stage, and instead performs *minimal testing*. To test on all the examples, call Popper with the `--test-all` flag.

For instance, running the command `python popper.py examples/trains --test-all` produces the output:

```prolog
BEST PROGRAM:
f(A):-long(B),has_car(A,C),roof_closed(B),has_car(A,B),three_wheels(C)
TP: 5, FN: 0, TN: 5, FP: 0
```

# Popper settings

Popper currently needs three parameters to restrict the language:

`max_vars(N).` sets the maximum number of variables allowed in a rule to be `N`

`max_body(N).` sets the maximum number of body literals in a rule to be `N`

`max_clauses(N).` sets the maximum number of rules/clause to be `N`

To enable predicate invention add `enable_pi.` to the bias file.

To enable recursion add `enable_recursion.` to the bias file.

To run with statistics use the flag `--stats`

To run with a maximum learning time use the flag `--eval-timeout`.

To run in debug mode use the flag `--debug`
