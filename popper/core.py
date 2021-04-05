from enum import Enum
from itertools import chain
from dataclasses import dataclass
from typing import Union, Any, FrozenSet, Tuple, Optional, Sequence, Callable

@dataclass(frozen=True)
class Variable:
    name : Any
    
    def __str__(self):
        return f'{self.name}'

@dataclass(frozen=True)
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
        args = (str(arg) for arg in self.arguments)
        return f'{self.predicate.name}({",".join(args)})'
    
    @property
    def arity(self):
        return len(self.arguments)

class ArgumentMode(Enum):
    Input   = '+'
    Output  = '-'
    Unknown = '?'

@dataclass(frozen=True)
class ModeDeclaration:
    predicate : PredicateSymbol
    arguments : Sequence[ArgumentMode]

@dataclass(frozen=True)
class ProgramLiteral:
    predicate : PredicateSymbol
    arguments : Tuple
    mode : ModeDeclaration
    polarity : bool

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

    @property
    def code_args(self):
        #return ','.join(map(lambda arg: chr(ord('A') + arg.name), self.arguments))
        return ','.join(str(arg) for arg in self.arguments)

    @property
    def inputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[0]))

    @property
    def outputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[1]))

    @property
    def unknowns(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[2]))

@dataclass(frozen=True)
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
    
    def is_definite(self):
        return self.is_horn() and len(self.head) == 1
    
    def is_recursive(self):
        return set(literal.predicate for literal in self.head) & \
               set(literal.predicate for literal in self.body) != set()

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
        self.clauses = clauses
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
    
    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'