# Popper

An Inductive Logic Programming system that learns Prolog/Datalog programs from examples by learning constraints.
See the paper 'Learning programs by learning from failures', on [arXiv](https://arxiv.org/abs/2005.02259).

## Installation

Currently the system expects to be run from the repository folder.

The python interface to `clingo`, an Answer Set Programming environment, is required.
Test if the python library was installed along with clingo by running `import clingo` in your `python` interpreter.

The `pyswip` python library is used as an interface to swi-prolog.

Check the `requirements.txt` file for version numbers of these libraries that are confirmed to work.

## Usage

### Command line

One option is to use the command line interface by invoking `python popper.py`:
```
usage: popper.py [-h] [--no-pruning] [--ground-constraints] [--timeout TIMEOUT] [--eval-timeout EVAL_TIMEOUT]
                 [-n MAX_LITERALS] [--debug]
                 EXAMPLES_FILE MODES_FILE BK_FILE
```

The `EXAMPLES_FILE` consists of ground facts representing the positive and negative examples, respectively wrapped in `pos(FACT).` and `neg(FACT)`.

The `MODES_FILE` is for predicate declarations, as used in the ASP encoding. A mode declaration has the form `modeh(PRED,ARITY)` or `modeb(PRED,ARITY)`, respectively for predicates that may occur in heads and bodies. The predicate `direction(PRED,POSITION,DIRECTION)` is used to indicate a Prolog 'mode' for a predicate. That is, for each argument position `Position` indicates whether it works as an input, `DIRECTION=in`, or as an output, `DIRECTION=out`.
(Parameters for optional features of the ASP encoding should be placed in this file.)

The `BK_FILE` should include Prolog predicate definitions. Any predicate declared to be allowed in bodies should be provided a definition. Currently, predicates allowed in the heads of learned clauses should not be provided definitions.

The `examples` folder contains multiple examples of the `EXAMPLES_FILE`, ASP `MODES_FILE` and Prolog `BK_FILE`.

### Popper as a Python library

The system can also be used as a library:

```
import popper.entry_point
program, context = popper.entry_point.run_experiment('modes.pl', 'bk.pl', 'exampes.pl', MAX_LITERALS, EVAL_TIMEOUT, GROUND_CONSTRAINTS, NO_PRUNING, TIMEOUT, DEBUG)
```

Take a look at the `popper/entry_point.py` and `popper/input.py` files to understand the different parameters.