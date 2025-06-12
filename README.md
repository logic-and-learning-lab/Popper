# Popper

Popper is an [inductive logic programming](https://arxiv.org/pdf/2008.07912.pdf) system.  If you use Popper, please cite the paper [Learning programs by learning from failures](https://arxiv.org/abs/2005.02259) (MLJ 2021).

If you have any questions about Popper, ask us on [Discord](https://discord.gg/Rv5mQCayAp) or email [Andrew Cropper](mailto:andrew.cropper@cs.ox.ac.uk).

#### Requirements
- [SWI-Prolog](https://www.swi-prolog.org) (9.2.0 or above)
- [Clingo](https://potassco.org/clingo/) (5.6.2 or above)
- [Janus-swi](https://github.com/SWI-Prolog/packages-swipy)
- [pysat](https://pysathq.github.io)
- [bitarray](https://github.com/ilanschnell/bitarray)

#### Installation
Install Popper with the command ```pip install git+https://github.com/logic-and-learning-lab/Popper@main```

#### Command line usage
Run Popper with the command `python popper.py <input dir>`. For instance, `python popper.py examples/zendo1` produces:
```prolog
********** SOLUTION **********
Precision:1.00 Recall:1.00 TP:20 FN:0 TN:20 FP:0 Size:6
zendo(V0):- small(V2),piece(V0,V1),red(V1),contact(V1,V3),size(V3,V2).
******************************
```

#### Example problem
Popper requires three input files:
- an examples file
- a background knowledge (BK) file
- a bias file

An examples file contains positive and negative examples of the relation you want to learn:

```prolog
pos(grandparent(ann,amelia)).
pos(grandparent(steve,amelia)).
pos(grandparent(ann,spongebob)).
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

A bias file defines the search space of Popper. Predicate declarations tell Popper which predicate symbols it can use in the head (`head_pred`) or body (`body_pred`) of a rule, such as:

```prolog
head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).
```

These declarations say that Popper can use the symbol *grandparent* with two arguments in the head of a rule and *mother* or *father* in the body, also each with two arguments.

#### Noisy examples
Popper can learn from [noisy](https://arxiv.org/pdf/2308.09393.pdf) data with the `--noisy` flag. Popper learns the minimal description length program.

#### Recursion
Popper can learn recursive programs where a predicate symbol appears in both the head and body of a rule, such as to find a duplicate element (`python popper.py examples/find-dupl`) in a list:
```prolog
f(A,B):- tail(A,C),head(A,B),element(C,B).
f(A,B):- tail(A,C),f(C,B).
```
To enable recursion, add `enable_recursion.` to the bias file. However, recursion is expensive, so it is best to avoid it if possible.

#### Types
Popper supports type annotations in the bias file. A type annotation is of the form `type(p,(t1,t2,...,tk)` for a predicate symbol `p` with arity `k`, such as:

```prolog
type(head,(list,element)).
type(tail,(list,list)).
type(length,(list,int,)).
type(empty,(list,)).
type(prepend,(element,list,list)).
```
Types are **optional** but can substantially reduce learning times.

#### Directions
Prolog often requires arguments to be ground. For instance, when asking Prolog to answer the query:
```prolog
X is 3+K.
```
It throws an error:
```prolog
ERROR: Arguments are not sufficiently instantiated
```
To avoid these issues, Popper supports **optional** direction annotations. A direction annotation is of the form `direction(p,(d1,d2,...,dk)` for a predicate symbol `p` with arity `k`, where each `di` is either `in` or `out`.
An `in` variable must be ground when calling the relation. By contrast, an `out` variable need not be ground. Here are example directions:

```prolog
direction(head,(in,out)).
direction(tail,(in,out)).
direction(length,(in,out)).
direction(prepend,(in,in,out)).
direction(geq,(in,in)).
```

Popper cannot learn with partial directions. If you provide them, you must provide them for all relations.

#### Bias
Popper has two important bias settings:

- `max_vars(N)` sets the maximum number of variables in a rule to `N` (default: 6)
- `max_body(N)` sets the maximum number of body literals in a rule to `N` (default: 6)

These settings greatly influence performance. If the values are too high, Popper might struggle to find a solution. If the settings are too low, the search space might be too small to contain a good solution. 

#### Predicate invention

Popper supports [automatic predicate invention](https://arxiv.org/pdf/2104.14426.pdf) (PI). With PI enabled, Popper (`python popper.py examples/kinship-pi`) learns the program:

```prolog
grandparent(A,B):-inv1(C,B),inv1(A,C).
inv1(A,B):-mother(A,B).
inv1(A,B):-father(A,B).
% Precision:1.00, Recall:1.00, TP:5, FN:0, TN:1, FP:0
```
To enable PI, add the setting `enable_pi.` to the bias file. However, predicate invention is currently *very* expensive so it is best to avoid it if possible.

#### Popper settings
 - `--noisy` (default: false) learn from [noisy](https://arxiv.org/pdf/2308.09393.pdf) (misclassified examples)
 - `--stats` (default: false) shows runtime statistics
 - `--debug` (default: false) runs in debug mode
 - `--quiet` (default: False)  runs in quiet mode
 - `--timeout` (default: 1200 seconds) sets a maximum learning time
 - `--eval-timeout` (default: 0.001 seconds) sets a maximum example testing time. This flag only applies when learning recursive programs.
 - `--solver {rc2,uwr,wmaxcdcl}`(default: `rc2`) which exact solver to use
 - `--anytime-solver {wmaxcdcl,nuwls}`(default: `None`) which anytime solver to use
 - `--anytime-timeout` (default: 10 seconds) sets the maximum time allowed by the anytime solver


#### Solvers
Popper uses various MaxSAT solvers. By default, Popper uses the [RC2](https://alexeyignatiev.github.io/assets/pdf/imms-jsat19-preprint.pdf) exact solver provided by PySAT. Popper also supports these solvers:

- [UWrMaxSat](https://github.com/marekpiotrow/UWrMaxSat) (exact)
- WMaxCDCL (exact)
- [NuWLS](https://ojs.aaai.org/index.php/AAAI/article/view/25505) (anytime)

You can download and compile these solvers from the [MaxSAT 2023 evaluation](https://maxsat-evaluations.github.io/2023/descriptions.html) website.
**We strongly recommend using the anytime NuWLS** solver as it greatly improves the performance of Popper. To use them, ensure that the solver is available on your path.  See the [install solvers](solvers.md) file for help.

#### Performance tips
- Transform your BK to Datalog, which allows Popper to perform preprocessing on the BK
- Try the NuWLS anytime solver.
- Use 6 variables or fewer
- Avoid recursion and predicate invention

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
