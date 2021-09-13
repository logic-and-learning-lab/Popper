# Popper

Popper is an inductive logic programming (ILP) system.
If you use Popper for research, please cite the paper [learning programs by learning from failures](https://arxiv.org/abs/2005.02259).
Popper is still a major work-in-progress, so please notify us of bugs or usability issues.

## Requirements

[SWI-Prolog](https://www.swi-prolog.org)

[Clingo 5.5.0](https://potassco.org/clingo/)

[pyswip](https://pypi.org/project/pyswip/)


# Usage

You can run Popper `python popper.py <input dir>`.

For instance, running the command `python popper.py examples/dropk` produces the output:

```prolog
f(A,B,C):-tail(A,C),one(B)
f(A,B,C):-decrement(B,E),f(A,E,D),tail(D,C)
TP: 10, FN: 0, TN: 10, FP: 0
```

Running the command `python popper.py examples/trains` produces the output:

```prolog
f(A):-long(B),roof_closed(B),has_car(A,B),three_wheels(C),has_car(A,C)
TP: 5, FN: 0, TN: 5, FP: 0
```

Take a look at the examples folder for examples.

Popper is an anytime algorithm. To see the intermediate solutions use the `--info` flag. For instance, running the command `python popper.py examples/trains2 --info` produces the output:

```prolog
NEW BEST PROG 1:
f(A):-short(B),has_car(A,B)
TP: 683, FN: 109, TN: 33, FP: 175

NEW BEST PROG 2:
f(A):-roof_closed(B),has_car(A,B)
TP: 745, FN: 47, TN: 36, FP: 172

NEW BEST PROG 3:
f(A):-roof_open(B),has_car(A,B)
TP: 731, FN: 61, TN: 172, FP: 36

NEW BEST PROG 95:
f(A):-roof_closed(C),roof_open(B),has_car(A,C),has_car(A,B)
f(A):-has_load(C,B),has_car(A,C),three_load(B),roof_open(C)
TP: 721, FN: 71, TN: 208, FP: 0

NEW BEST PROG 382:
f(A):-has_car(A,C),roof_closed(B),has_car(A,B),roof_open(C)
f(A):-rectangle(D),has_load(C,B),has_car(A,C),has_load(E,D),triangle(B),has_car(A,E)
TP: 761, FN: 31, TN: 208, FP: 0

BEST PROG 1271:
f(A):-roof_closed(C),roof_open(B),has_car(A,C),has_car(A,B)
f(A):-has_car(A,B),three_load(C),roof_open(B),has_load(B,C)
f(A):-rectangle(D),has_car(A,B),has_load(B,C),triangle(C),has_load(E,D),has_car(A,E)
TP: 792, FN: 0, TN: 208, FP: 0
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

To run in information mode use the flag `--debug`
