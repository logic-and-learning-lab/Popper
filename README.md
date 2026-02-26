
### Popper

Popper is an [inductive logic programming](https://arxiv.org/pdf/2008.07912.pdf) system. Popper learns logical rules from examples and background knowledge.

Ask questions us on [Discord](https://discord.gg/Rv5mQCayAp) or email [Andrew Cropper](mailto:andrew.cropper@helsinki.fi).

If you use Popper, please cite the paper [learning programs by learning from failures](https://arxiv.org/abs/2005.02259) (MLJ 2021).


**Requirements**

- GNU Coreutils `(brew install coreutils)`
- [uv](https://github.com/astral-sh/uv) package manager (`brew install uv`)

**Install and run**

```
git clone https://github.com/logic-and-learning-lab/Popper.git
cd Popper
```
Then run via `uv run popper.py <input dir>` such as `uv run popper.py examples/iggp-rps-next-score`


**Example problem**

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

A bias file defines the hypothesis space. The following statements tell Popper which predicate symbols it can use in the head (`head_pred`) or body (`body_pred`)  of a rule:
```prolog
head_pred(grandparent,2).
body_pred(mother,2).
body_pred(father,2).
```
These say that Popper can use the symbol *grandparent* with two arguments in the head of a rule and *mother* or *father* in the body, also each with two arguments.

**Noise**

Popper can learn from [noisy](https://arxiv.org/pdf/2308.09393.pdf) data with the `--noisy` flag. Popper learns a minimal description length hypothesis.

**Settings**

 - `--noisy` learn from [noisy](https://arxiv.org/pdf/2308.09393.pdf) (misclassified examples) (default: false)
 - `--max_vars` maximum number of variables in a rule to `N` (default: 6)
 - `--max_body` maximum number of body literals in a rule to `N` (default: 10)
 - `--stats` shows runtime statistics (default: false)
 - `--debug` runs in debug mode (default: false)
 - `--quiet` runs in quiet mode (default: False)
 - `--timeout` maximum learning time (default: 3600 seconds)
 - `--eval-timeout` a maximum example testing time. This flag only applies when learning recursive rules (default: 0.001 seconds)
 - `--solver {rc2,uwr,wmaxcdcl}`which exact solver to use (default: `rc2`)
 - `--anytime-solver {wmaxcdcl,nuwls}`which anytime solver to use (default: `None`)
 - `--anytime-timeout` sets the maximum time allowed by the anytime solver (default: 10 seconds)

**Solvers**

Popper uses various MaxSAT solvers. By default, Popper uses the [RC2](https://alexeyignatiev.github.io/assets/pdf/imms-jsat19-preprint.pdf) exact solver provided by PySAT. Popper also supports these solvers:

- [UWrMaxSat](https://github.com/marekpiotrow/UWrMaxSat) (exact)
- [NuWLS](https://ojs.aaai.org/index.php/AAAI/article/view/25505) (anytime)

You can download and compile these solvers from the [MaxSAT 2023 evaluation](https://maxsat-evaluations.github.io/2023/descriptions.html) website. **We strongly recommend using the anytime NuWLS** solver as it greatly improves the performance of Popper. To use them, ensure that the solver is available on your path.  See the [install solvers](solvers.md) file for help.


**Recursion**

Popper can learn recursive rules (where a predicate symbol appears in both the head and body), such as to find a duplicate element (`uv run popper.py examples/find-dupl`) in a list:
```prolog
f(A,B):- tail(A,C),head(A,B),element(C,B).
f(A,B):- tail(A,C),f(C,B).
```
To enable recursion, add `enable_recursion.` to the bias file. However, recursion is expensive, so it is best to avoid it if possible.

**Types**

Popper supports type annotations in the bias file. A type annotation is of the form `type(p,(t1,t2,...,tk)` for a predicate symbol `p` with arity `k`, such as:

```prolog
type(head,(list,element)).
type(tail,(list,list)).
type(length,(list,int,)).
type(empty,(list,)).
type(prepend,(element,list,list)).
```
Types are **optional** but can substantially reduce learning times.

**Directions**

Prolog often requires arguments to be ground. For instance, when asking Prolog to answer the query:
```prolog
X is 3+K.
```
It throws an error:
```prolog
ERROR: Arguments are not sufficiently instantiated
```
To avoid these issues, Popper supports **optional** direction annotations. A direction annotation is of the form `direction(p,(d1,d2,...,dk)` for a predicate symbol `p` with arity `k`, where each `di` is either `in` or `out`. An `in` variable must be ground when calling the relation. By contrast, an `out` variable need not be ground. Here are example directions:

```prolog
direction(head,(in,out)).
direction(tail,(in,out)).
direction(length,(in,out)).
direction(prepend,(in,in,out)).
direction(geq,(in,in)).
```

Popper cannot learn with partial directions. If you provide them, you must provide them for all relations.

**Performance tips**

- Transform your BK to Datalog, which allows Popper to perform preprocessing on the BK
- Try the NuWLS anytime solver
- Use 6 variables or fewer
- Avoid recursion and predicate invention


