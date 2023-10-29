# Popper

Popper is an [inductive logic programming](https://arxiv.org/pdf/2008.07912.pdf) (ILP) system. 

If you use Popper, please cite the paper: Andrew Cropper and Rolf Morel. [Learning programs by learning from failures](https://arxiv.org/abs/2005.02259) (MLJ 2021).

#### Requirements
- [pyswip](https://github.com/yuce/pyswip) (**You must install pyswip from the master branch with  the command: `pip install git+https://github.com/yuce/pyswip@master#egg=pyswip`**)
- [SWI-Prolog](https://www.swi-prolog.org) (9.0.4 or above)
- [Clingo](https://potassco.org/clingo/) (5.6.2 or above)
- [pysat](https://pysathq.github.io) (3.1.0 or above)


#### Installation
To install the master branch, run the command: ```pip install git+https://github.com/logic-and-learning-lab/Popper@main```

#### Command line usage
Run Popper with the command `python popper.py <input dir>`. For instance, the command `python popper.py examples/zend1` produces:
```prolog
11:11:14 Generating programs of size: 3
11:11:14 Generating programs of size: 4
11:11:14 Generating programs of size: 5
11:11:14 Generating programs of size: 6
11:11:14 ********************
11:11:14 New best hypothesis:
11:11:14 tp:19 fn:1 tn:20 fp:0 size:20
11:11:14 zendo(A):- piece(A,B),red(B),coord1(B,C),size(B,C).
11:11:14 zendo(A):- piece(A,C),contact(C,B),blue(B),rhs(C).
11:11:14 zendo(A):- piece(A,C),contact(C,B),red(B),upright(B).
11:11:14 zendo(A):- piece(A,C),contact(C,B),red(B),lhs(B).
11:11:14 ********************
********** SOLUTION **********
Precision:1.00 Recall:1.00 TP:20 FN:0 TN:20 FP:0 Size:6
zendo(A):- piece(A,D),red(D),contact(D,B),size(B,C),small(C).
******************************
```

Look at the examples for guidance.

#### Example problem
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

A bias file contains the information necessary to restrict the search space of Popper. **Predicate declarations** tell Popper which predicate symbols it can use in the head or body of a rule, such as:

```prolog
head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).
```

These declarations say that each rule in a program must have the symbol *grandparent* with arity two in the head and *mother* and/or *father* in the body, also with arity two. If we call Popper with these three files it will produce the output:

#### Noisy examples
Popper can learn from [noisy](https://arxiv.org/pdf/2308.09393.pdf) (misclassified examples). To do so, run Popper with the `--noisy` flag.

#### Recursion
To enable recursion add `enable_recursion.` to the bias file. Recursion allows Popper to learn programs where a predicate symbol appears in both the head and body of a rule, such as to find a duplicate element (`python popper.py examples/find-dupl`) in a list:

```prolog
f(A,B):- tail(A,C),head(A,B),element(C,B).
f(A,B):- tail(A,C),f(C,B).
```

Recursion is expensive, so it is best to try without it first.

#### Types
Popper supports **optional** type annotations in the bias file.A type annotation is of the form `type(p,(t1,t2,...,tk)` for a predicate symbol `p` with arity `k`, such as:

```prolog
type(f,(list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(empty,(list,)).
type(odd,(element,)).
type(even,(element,)).
type(prepend,(element,list,list)).
```
These types are **optional** but can help reduce learning times.

### Directions
Prolog often requires arguments to be ground. For instance, when asking Prolog to answer the query:
```prolog
X is 3+K.
```
It throws an error:
```prolog
ERROR: Arguments are not sufficiently instantiated
```
To avoid theses issues, Popper supports **optional** direction annotations. A direction annotation is of the form `direction(p,(d1,d2,...,dk)` for a predicate symbol `p` with arity `k`, where each `di` is either `in` or `out`.
An `in` variable must be ground when calling the relation. By contrast, an `out` variable need not be ground. Here are example directions:

```prolog
direction(head,(in,out)).
direction(tail,(in,out)).
direction(length,(in,out)).
direction(prepend,(in,in,out)).
direction(geq,(in,in)).
```

**Popper currently cannot learn with partial directions. So if you provide them, you must give them for all relations.**



Directions are **optional** but can substantially reduce learning times.

#### Bias
Popper has three important bias settings:

- `max_vars(N)` sets the maximum number of variables in a rule to `N` (default: 6)
- `max_body(N)` sets the maximum number of body literals in a rule to `N` (default: 6)
- `max_clauses(N)` sets the maximum number of rules/clauses to `N` (default: 1 or 2 if `enable_recursion` is set)

These settings greatly influence performance. If the values are too high then Popper might struggle to learn a solution. If the settings are too low then the search space might be too small to contain a good solution. You can set these settings in the bias file or through the command line (see `--help`).

**IMPORTANT: do not supply max_clauses if you are learning non-recursive programs.**

#### Predicate invention

Popper supports [automatic predicate invention](https://arxiv.org/pdf/2104.14426.pdf) (PI). To enable PI, add the setting `enable_pi.` to the bias file. With PI enabled, Popper (`python popper.py examples/kinship-pi`) learns the following program:

```prolog
grandparent(A,B):-inv1(C,B),inv1(A,C).
inv1(A,B):-mother(A,B).
inv1(A,B):-father(A,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```
Predicate invention is currently very expensive so it is best to avoid it if possible.

#### Popper settings

 -  `--stats` (default: false) shows runtime statistics
 - `--debug` (default: false) runs in debug mode
 - `--quiet` (default: False)  runs in quiet mode
 - `--bkcons` (default: False) allows Popper to [discover constraints from the BK](https://arxiv.org/pdf/2202.09806.pdf). This flag only works with Datalog programs.
 - `--datalog` (default: False) allows Popper to test programs more quickly by ordering the literals on them. This flag only works with Datalog programs.
 - `--timeout` (default: 600 seconds) sets a maximum learning time
 - `--eval-timeout` (default: 0.001 seconds) sets a maximum example testing time. This flag only applies when learning recursive programs.

#### Performance tips
- Transform your BK to Datalog rules and use the `--bkcons` and `--datalog` flags.
- Try not to use more than 6 variables.
- Avoid recursion and predicate invention if possible.

#### Library usage

You can import Popper and use it in your Python code like so:

```python
from popper.util import Settings, print_prog_score
from popper.loop import learn_solution

settings = Settings(kbpath='input_dir')
prog, score, stats = learn_solution(settings)
if prog != None:
    print_prog_score(prog, score)
```
