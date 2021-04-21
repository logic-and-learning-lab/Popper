# Popper

Popper is an inductive logic programming (ILP) system.
For more information, see the paper [Learning programs by learning from failures](https://arxiv.org/abs/2005.02259).
If you use Metagol for research, please cite the relevant paper.


## Requirements

[SWI-Prolog](https://www.swi-prolog.org)
[Clingo 5.5.0](https://potassco.org/clingo/)
[pyswip](https://pypi.org/project/pyswip/)


# Usage

You can run Popper like so:
```
python popper.py examples/dropk/
f(A,B,C) :- one(B),tail(A,C).
f(A,B,C) :- tail(A,E),decrement(B,D),f(E,D,C).
```

Take a look at the examples folder for examples.
