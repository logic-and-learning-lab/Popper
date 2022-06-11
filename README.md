# Popper

Popper is an [inductive logic programming](https://arxiv.org/pdf/2008.07912.pdf) (ILP) system. 

If you use Popper, please cite the paper: 

Andrew Cropper and Rolf Morel. [Learning programs by learning from failures](https://arxiv.org/abs/2005.02259). Mach. Learn. 110(4): 801-856 (2021)

## Requirements

- [pyswip](https://github.com/yuce/pyswip) (**You _must_ install pyswip from the master branch!**)
    -  use the command: `pip install git+https://github.com/yuce/pyswip@master#egg=pyswip`


- [SWI-Prolog](https://www.swi-prolog.org) (**8.4.2 or above**)

- [Clingo](https://potassco.org/clingo/) (**5.5.0 or above**)

## Installation
The latest release of Popper is [here](https://github.com/logic-and-learning-lab/Popper/releases). 
To install the master branch, run the command:
```pip install git+https://github.com/logic-and-learning-lab/Popper@main``` 

# Command line usage

You can run Popper with the command `python popper.py <input dir>`.
For instance, the command `python popper.py examples/dropk` produces:

```prolog
********** SOLUTION **********
Precision:1.00 Recall:1.00 TP:10 FN:0 TN:10 FP:0 Size:7
f(A,B,C):- tail(A,C),one(B).
f(A,B,C):- decrement(B,E),tail(A,D),f(D,E,C).
******************************
```

The command `python popper.py examples/trains1` produces:

```prolog
********** SOLUTION **********
Precision:1.00 Recall:1.00 TP:394 FN:0 TN:606 FP:0 Size:6
f(A):- has_car(A,C),has_car(A,B),long(B),three_wheels(C),roof_closed(B).
******************************
```

Look at the examples for guidance.

# Library usage

You can import Popper and use it in your Python code like so:

```python
from popper.util import Settings, print_prog_score
from popper.loop import learn_solution

settings = Settings(kbpath='input_dir')
prog, score, stats = learn_solution(settings)
if prog != None:
    print_prog_score(prog, score)
```

# Example problem

Popper requires three files: 

- an examples file
- a background knowledge (BK) file
- a bias file

An examples file contains positive and negative examples of the relation you want to learn:

```prolog
pos(grandparent(ann,amelia)).
pos(grandparent(steve,amelia)).
pos(grandparent(ann,spongebob)).
pos(grandparent(steve,spongebob)).
pos(grandparent(linda,amelia)).
neg(grandparent(amy,amelia)).
```

A BK file contains other information about the problem:

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

A bias file contains information necessary to restrict the search space of Popper.
**Predicate declarations** tell Popper which predicate symbols it can use in the head or body of a rule, such as:

```prolog
head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).
```

These declarations say that each rule in a program must have the symbol *grandparent* with arity two in the head and *mother* and/or *father* in the body, also with arity two. If we call Popper with these three files it will produce the output:

```prolog
grandparent(A,B):-mother(A,C),father(C,B).
grandparent(A,B):-father(A,C),mother(C,B).
grandparent(A,B):-father(A,C),father(C,B).
grandparent(A,B):-mother(A,C),mother(C,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```

## Bias
Popper has three main bias settings:

- `max_vars(N)` sets the maximum number of variables in a rule to `N` (default: 6)
- `max_body(N)` sets the maximum number of body literals in a rule to `N` (default: 6)
- `max_clauses(N)` sets the maximum number of rules/clauses to `N` (default: 1 or 2 if `enable_recursion` is set)

These parameters are important. They greatly influence the search space. If the values are too high then Popper might struggle to learn a solution. If the settings are too low then the search space might be too small to contain a good solution. 
You can set these settings in the bias file or through the command line (see `--help`).

Finding suitable values can often be a process of trial and error. We are trying to automatically set these settings. 

**Do not supply max_clauses if you are learning non-recursive programs.**

## Anytime

Popper is an anytime algorithm.
By default, it shows intermediate solutions.
For instance, the command `python popper.py examples/dropk` produces:

```prolog
08:08:54 Num. pos examples: 10
08:08:54 Num. neg examples: 10
08:08:54 Searching programs of size: 3
08:08:54 Searching programs of size: 4
08:08:54 Searching programs of size: 5
08:08:54 Searching programs of size: 6
08:08:54 ********************
08:08:54 New best hypothesis:
08:08:54 tp:1 fn:9 size:6
08:08:54 f(A,B,C):- tail(E,C),tail(D,F),tail(F,E),even(B),tail(A,D).
08:08:54 ********************
08:08:56 Searching programs of size: 7
08:08:57 ********************
08:08:57 New best hypothesis:
08:08:57 tp:10 fn:0 size:13
08:08:57 f(A,B,C):- tail(A,C),element(A,B).
08:08:57 f(A,B,C):- tail(E,C),tail(D,F),tail(F,E),even(B),tail(A,D).
08:08:57 f(A,B,C):- decrement(B,D),tail(E,C),f(A,D,E).
08:08:57 ********************
08:08:58 ********************
08:08:58 New best hypothesis:
08:08:58 tp:10 fn:0 size:7
08:08:58 f(A,B,C):- tail(A,C),one(B).
08:08:58 f(A,B,C):- f(A,E,D),tail(D,C),decrement(B,E).
08:08:58 ********************
********** SOLUTION **********
Precision:1.00 Recall:1.00 TP:10 FN:0 TN:10 FP:0 Size:7
f(A,B,C):- tail(A,C),one(B).
f(A,B,C):- decrement(B,E),f(A,E,D),tail(D,C).
******************************
```

To supress this information, run Popper with the `--quiet` (`-q`) flag.

## Recursion
To enable recursion add `enable_recursion.` to the bias file.
Recursion allows Popper to learn programs where a predicate symbol appears in both the head and body of a rule, such as to find a duplicate element (`python popper.py examples/find-dupl`) in a list:

```prolog
f(A,B):-head(A,B),tail(A,C),element(C,B).
f(A,B):-tail(A,C),f(C,B).
```
Or to remove (`python popper.py examples/filter`) non-even elements from a list:

```prolog
f(A,B):-empty(A),empty(B).
f(A,B):-tail(A,D),head(A,C),odd(C),f(D,B).
f(A,B):-head(A,E),even(E),tail(A,C),f(C,D),prepend(E,D,B).
```

Recursion is expensive, so it is best to try without it first.

## Types
Popper supports **optional** type annotations in the bias file.
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
These types are **optional** but can substantially reduce learning times.

## Directions 
Prolog often requires arguments to be ground.
For instance, when asking Prolog to answer the query:
```prolog
X is 3+K.
```
It throws an error:
```prolog
ERROR: Arguments are not sufficiently instantiated
```
Moreover, we want to reduce the number of answers from a query. For instance, calling the `length` predicate with only variables leads to an infinite set of answers.

To avoid this issues, Popper supports **optional** direction annotations.
A direction annotation is of the form `direction(p,(d1,d2,...,dk)` for a predicate symbol `p` with arity `k`, where each `di` is either `in` or `out`.
An `in` variable must be ground when calling the relation.
By contrast, an `out` variable need not be ground.
Here are example directions:

```prolog
direction(head,(in,out)).
direction(tail,(in,out)).
direction(length,(in,out)).
direction(prepend,(in,in,out)).
direction(geq,(in,in)).
```

Again, directions are **optional** but can substantially reduce learning times.

## Predicate invention

Popper supports [automatic predicate invention](https://arxiv.org/pdf/2104.14426.pdf) (PI). 
To enable PI, add the setting `enable_pi.` to the bias file.
With PI enabled, Popper (`python popper.py examples/kinship-pi`) learns the following program:

```prolog
grandparent(A,B):-inv1(C,B),inv1(A,C).
inv1(A,B):-mother(A,B).
inv1(A,B):-father(A,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```

Predicate invention is currently very expensive so it is best to avoid it if possible.

<!-- # Functional test
TODO
 -->
<!-- # Non-observational predicate learning

Popper supports non-observational predicate learning, where it must learn definitions for relations not given as examples.
See the example 'non-OPL'.
 -->

## Parallelisation
[Coming soon](https://arxiv.org/pdf/2109.07132.pdf)

## Bias discovery
[Coming soon](https://arxiv.org/pdf/2202.09806.pdf)

## Failure explanation
[Coming soon](https://arxiv.org/pdf/2102.12551.pdf)

# Popper settings

To run with statistics use the flag `--stats` (default: false)

To run in debug mode use the flag `--debug` (default: false)

To run in quiet mode use the flag `--quiet` (default: False)

<!-- To show the full hypothesis space (bounded by `N`) use the flag `--hspace N`. -->

To run with a maximum learning time use the flag `--timeout` (default: 600 seconds)

To run with a maximum example testing (only applies when learning recursive programs) time use the flag `--eval-timeout` (default: 0.001 seconds)

To allow non-Datalog clauses, where a variable in the head need not appear in the body, add ``non_datalog.` to your bias file.

To allow singleton variables (variables that only appear once in a clause), add `allow_singletons.` to your bias file.

To set the maximum number of literals allow in a program use the flag  `--max-literals` (default: 40)

To set the maximum number of body literals allowed in the body of a rule use the flag `--max-body` (default: 6)

To set the maximum number of variables allowed in a rule use the flag `--max-vars` (default: 6)

To set the maximum number of examples to learn from use the flag `--max-examples` (default: 10000)
