import operator as op
from enum import Enum
from itertools import chain
from dataclasses import dataclass
from typing import Union, Any, FrozenSet, Tuple, Optional, Sequence, Callable

@dataclass(frozen=True, order=True)
class Variable:
    name : Any

    def __str__(self):
        return f'{self.name}'

@dataclass(frozen=True, order=True)
class PredicateSymbol:
    name : str
    arity : Optional[int] = None
    fixed_interpretation : bool = False

@dataclass(frozen=True)
class Atom:
    predicate : PredicateSymbol
    arguments : Tuple

    def __post_init__(self):
        if isinstance(self.predicate, str):
            object.__setattr__(self, 'predicate', PredicateSymbol(self.predicate))
        if self.arguments:
            assert isinstance(self.arguments, Tuple)
        assert self.predicate.arity in (None, len(self.arguments))

    def __str__(self):
        def args_formatter():
            for arg in self.arguments:
                if isinstance(arg, Variable):
                    yield f'{arg.name}'
                elif isinstance(arg, tuple):
                    yield '(' + ','.join(str(a) for a in arg) + ',)'
                else:
                    yield str(arg)
        return f'{self.predicate.name}({",".join(args_formatter())})'

    @property
    def arity(self):
        return len(self.arguments)

    def all_vars(self):
        for argument in self.arguments:
            if isinstance(argument, Variable):
                yield argument
            elif isinstance(argument, tuple):
                for t_argument in argument:
                    if isinstance(t_argument, Variable):
                        yield t_argument

@dataclass(frozen=True)
class Literal(Atom):
    polarity : bool
    naf : bool = True

    def __str__(self):
        if self.polarity: return super().__str__()
        return 'not ' + super().__str__()

    # @NOTE: Direct copy from original. Jk: Rewrite.
    def ground(self, assignment):
        def args():
            for arg in self.arguments:
                if isinstance(arg, Variable):
                    yield assignment[arg]
                elif isinstance(arg, tuple):
                    yield tuple((assignment[a] if isinstance(a, Variable) else a)
                                for a in arg)
                else:
                    yield arg
        return __class__(predicate = self.predicate, arguments = tuple(args()),
                         polarity = self.polarity, naf = self.naf)

class ArgumentMode(Enum):
    Input   = '+'
    Output  = '-'
    Unknown = '?'

@dataclass(frozen=True)
class ModeDeclaration:
    predicate : PredicateSymbol
    arguments : Sequence[ArgumentMode]

    def __str__(self):
        return self.predicate.name + "(" + ",".join(mode.value for mode in self.arguments) + ")"

@dataclass(frozen=True, order=True)
class ProgramLiteral:
    predicate : PredicateSymbol
    arguments : Tuple
    mode : ModeDeclaration
    polarity : bool
    naf : bool = True

    def __str__(self):
        args = (f'V{arg}' for arg in self.arguments)
        return f'{self.predicate}({",".join(args)})'

    # @NOTE: In the orignal Rolf states that this is wrong but useful for
    #        debugging.
    def __repr__(self):
        mode_args = (f'{m.value}V{arg.name}'
                     for arg, m in zip(self.arguments, self.mode.arguments))
        return f'{self.predicate.name}({",".join(mode_args)})'

    @property
    def arity(self):
        return len(self.arguments)

    def to_code(self):
        code_args = ','.join(str(arg) for arg in self.arguments)
        return f'{self.predicate.name}({code_args})'

    def split_arguments(self):
        inputs, outputs, unknowns = set(), set(), set()

        for idx, (mode, arg) in enumerate(zip(self.mode.arguments, self.arguments)):
            if mode == ArgumentMode.Input: inputs.add((idx,arg))
            if mode == ArgumentMode.Output: outputs.add((idx,arg))
            if mode == ArgumentMode.Unknown: unknowns.add((idx,arg))
        return inputs, outputs, unknowns

    # @NOTE: Direct copy from original. Jk: Rewrite.
    def all_vars(self):
        for argument in self.arguments:
            if isinstance(argument, Variable):
                yield argument
            elif isinstance(argument, tuple):
                for t_argument in argument:
                    if isinstance(t_argument, Variable):
                        yield t_argument

    @property
    def code_args(self):
        #return ','.join(map(lambda arg: chr(ord('A') + arg.name), self.arguments))
        return ','.join(str(arg) for arg in self.arguments)

    @property
    def inputs(self):
        # AC: can we not rewrite to:
        # return set(x[1] for x in self.split_arguments()[0])
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[0]))

    @property
    def outputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[1]))

    @property
    def unknowns(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[2]))

@dataclass(frozen=True, order=True)
class Clause:
    head : FrozenSet
    body : FrozenSet

    def __str__(self):
        return ','.join(str(l) for l in self.head) + \
               ':- ' + \
               ','.join(str(l) for l in self.body) + '.'

    def is_constraint(self):
        return len(self.head) == 0

    def is_horn(self):
        return len(self.head) <= 1 and \
               (literal.polarity for literal in chain(self.head, self.body))

    def all_vars(self):
        return set(variable for literal in chain(self.head, self.body)
                            for variable in literal.all_vars())

    def is_definite(self):
        return self.is_horn() and len(self.head) == 1

    def is_recursive(self):
        return set(literal.predicate for literal in self.head) & \
               set(literal.predicate for literal in self.body) != set()

    # @NOTE: Direct copy from original. Jk: Rewrite.
    def ground(self, assignment):
        return __class__(head = tuple(lit.ground(assignment) for lit in self.head),
                         body = tuple(lit.ground(assignment) for lit in self.body))

class UnorderedClause(Clause):
    def __init__(self, head, body, min_num):
        self.min_num = min_num
        super().__init__(head, body)
        assert self.is_definite() # @CarryOver: Sanity check?

    def to_code(self):
        head_ = str(self.head[0].to_code())
        body_ = (atom.to_code() for atom in self.body)
        return f'{head_} :- {{ {",".join(body_)} }}'

    # @NOTE: Straight copy from the original. Revise later.
    def to_ordered(self):
        assert self.is_definite()

        def selection_closure(selected, head_pred, grounded_vars, literals):
            if len(literals) == 0:
                return selected

            rec_lits, nonrec_lits = [], []
            for lit in literals:
                if lit.inputs.issubset(grounded_vars):
                    if lit.predicate == head_pred:
                        rec_lits.append(lit)
                    else:
                        nonrec_lits.append(lit)

            selected_lit = next(chain(nonrec_lits, rec_lits), None)
            if selected_lit == None:
                message = f'literals {literals} in clause {self} could not be grounded'
                raise ValueError(message)
            selected.append(selected_lit)
            return selection_closure(selected,
                                     head_pred,
                                     grounded_vars.union(selected_lit.outputs),
                                     literals.difference({selected_lit}))

        ordered_body = tuple(selection_closure([], self.head[0].predicate,
                                               self.head[0].inputs,
                                               set(self.body)))

        return OrderedClause(self.head, ordered_body, self.min_num)

class OrderedClause(Clause):
    def __init__(self, head, body, min_num):
        self.min_num = min_num
        super().__init__(head, body)

    def to_code(self):
        head_ = str(self.head[0].to_code())
        body_ = (atom.to_code() for atom in self.body)
        return f'{head_} :- {",".join(body_)}'

class UnorderedProgram:
    def __init__(self, clauses, before):
        self.clauses : FrozenSet[Clause] = clauses
        self.before : Dict[int, Set[int]] = before

    def __iter__(self):
        return iter(self.clauses)

    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'

    def to_ordered(self):
        ordered_clauses = []
        for clause in self.clauses:
            ordered_clauses.append(clause.to_ordered())

        return OrderedProgram(tuple(ordered_clauses), self.before)

class OrderedProgram:
    def __init__(self, clauses, before):
        self.clauses = clauses
        self.before : Dict[int, Set[int]] = before

    def __iter__(self):
        return iter(self.clauses)

    def __len__(self):
        return len(self.clauses)

    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'

# -----------------------------------------------------------------------------
# Constraint related. @NOTE: Copied from original without modificatoin. Jk,
# refactor. Has implications for calls in constrain.py.

# @NOTE: This can be implemented in a way that doesn't require the creation of
#        an empty class.
@dataclass(frozen=True, order=True)
class VarVariable(Variable):
    pass # Subclass serves as tag to indicate it ranges over variable indices

@dataclass(frozen=True, order=True)
class ClauseVariable(Variable):
    pass # Subclass serves as tag to indicate it ranges over clause indices

@dataclass(frozen=True)
class ConstraintSymbol(PredicateSymbol):
    operator : Callable = None
    fixed_interpretation : bool = True

@dataclass(frozen=True)
class ConstraintLiteral(Literal):
    predicate : ConstraintSymbol
    naf : bool = False

    def __str__(self):
        if self.predicate.arity == 2:
            assert self.polarity, 'need to think about negated operator literals'
            arg0, arg1 = self.arguments[0], self.arguments[1]
            return f'{arg0}{self.predicate.name}{arg1}'
        return super().__str__()

    def evaluate(self, var_assignment: dict):
        args = ((var_assignment[arg] if isinstance(arg, Variable) else arg)
                for arg in self.arguments)
        eval_ = self.predicate.operator(*args)
        return self.polarity == eval_  # take into accout whether literal was negative

    @classmethod
    def pos(cls, *args):
        return cls(cls.predicate, arguments=args, polarity=True)

    @classmethod
    def neg(cls, *args):
        return cls(cls.predicate, arguments=args, polarity=False)

@dataclass(frozen=True)
class LT(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '<', arity = 2, operator = op.lt)

@dataclass(frozen=True)
class GT(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '>', arity = 2, operator = op.gt)

@dataclass(frozen=True)
class EQ(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '==', arity = 2, operator = op.eq)

@dataclass(frozen=True)
class NEQ(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '!=', arity = 2, operator = op.ne)

@dataclass(frozen=True)
class GTEQ(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '>=', arity = 2, operator = op.ge)

@dataclass(frozen=True)
class LTEQ(ConstraintLiteral):
    predicate = ConstraintSymbol(name = '<=', arity = 2, operator = op.le)