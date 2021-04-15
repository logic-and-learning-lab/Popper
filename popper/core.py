from itertools import chain
from collections import namedtuple, defaultdict
from dataclasses import dataclass


ConstVar = namedtuple('ConstVar', ['name', 'type'])
ConstOpt = namedtuple('ConstOpt', ['operator', 'arguments', 'operation'])

class Literal:
    def __init__(self, predicate, arguments, modes = None, positive = True):
        self.predicate = predicate
        self.arguments = arguments
        self.arity = len(arguments)
        self.modes = modes
        self.positive = positive

    def __str__(self):
        # Mode is None for constraint literals.
        if self.modes:
            vmodes = (varmode + var for var, varmode in zip(self.arguments, self.modes))
            x = f'{self.predicate}({",".join(vmodes)})'
            if not self.positive:
                x = 'not ' + x
            return x
        else:
            args = []
            for arg in self.arguments:
                if isinstance(arg, ConstVar):
                    args.append(arg.name)
                elif isinstance(arg, tuple):
                    t_args = []
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            t_args.append(t_arg.name)
                        else:
                            t_args.append(str(t_arg))
                    args.append(f'({",".join(t_args)})')
                else:
                    args.append(str(arg))

            x = f'{self.predicate}({",".join(args)})'
            if not self.positive:
                x = 'not ' + x
            return x

    # AC: @ALL, why?
    def __repr__(self):
        return self.__str__()

    def inputs(self):
        return set(arg for mode, arg in zip(self.modes, self.arguments) if mode == '+')

    def outputs(self):
        return set(arg for mode, arg in zip(self.modes, self.arguments) if mode == '-')

    def to_code(self):
        return f'{self.predicate}({",".join(self.arguments)})'

    def all_vars(self):
        return self.arguments

    # @profile
    def ground(self, assignment):
        ground_args = []
        for arg in self.arguments:
            if arg in assignment:
                ground_args.append(assignment[arg])
            elif isinstance(arg, tuple):
                ground_t_args = []
                for t_arg in arg:
                    if t_arg in assignment:
                        ground_t_args.append(assignment[t_arg])
                    else:
                        ground_t_args.append(t_arg)
                ground_args.append(tuple(ground_t_args))
            else:
                ground_args.append(arg)

        return Literal(self.predicate, tuple(ground_args), self.modes, self.positive)

class Clause:
    def __init__(self, head, body, min_num = 0):
        self.head = head
        self.body = body
        self.min_num = min_num
        self.ordered = False
        # self.body_size = len(body)

    def __str__(self):
        if self.head:
            return f'{str(self.head)} :- {", ".join(str(literal) for literal in self.body)}'
        return f':- {", ".join(str(literal) for literal in self.body)}'

    def to_ordered(self):
        if self.ordered:
            return

        ordered_body = []
        grounded_variables = self.head.inputs()
        body_literals = set(self.body)

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                # AC: could cache for a micro-optimisation
                if literal.inputs().issubset(grounded_variables):
                    if literal.predicate != self.head.predicate:
                        # find the first ground non-recursive body literal and stop
                        selected_literal = literal
                        break
                    else:
                        # otherwise use the recursive body literal
                        selected_literal = literal

            if selected_literal == None:
                message = f'{selected_literal} in clause {self} could not be grounded'
                raise ValueError(message)

            ordered_body.append(selected_literal)
            grounded_variables = grounded_variables.union(selected_literal.outputs())
            body_literals = body_literals.difference({selected_literal})

        self.body = tuple(ordered_body)
        self.ordered = True

    def to_code(self):
        return (
            f'{self.head.to_code()} :- '
            f'{",".join([blit.to_code() for blit in self.body])}'
        )

    def is_recursive(self):
        return self.head.predicate in set(literal.predicate for literal in self.body)

    def all_vars(self):
        return set(variable for literal in chain([self.head], self.body)
                            for variable in literal.all_vars())

    # @profile
    def ground(self, assignment):
        ground_body = tuple(literal.ground(assignment) for literal in self.body)
        if self.head:
            return Clause(self.head.ground(assignment), ground_body, self.min_num)
        else:
            return Clause(None, ground_body, self.min_num)

class Program:
    def __init__(self, clauses, before = defaultdict(set)):
        self.clauses = clauses
        self.before = before
        self.ordered = False
        self.num_clauses = len(clauses)

    def __iter__(self):
        return iter(self.clauses)

    # AC: this method changes the program
    def to_ordered(self):
        if self.ordered:
            return
        # AC: do we need to also :reorder the clauses?
        for clause in self.clauses:
            clause.to_ordered()
        self.ordered = True

    # AC: this method returns new objects - confusing given the above
    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'

class Constraint:
    def __init__(self, ctype, head, body):
        self.ctype = ctype
        self.head = head
        self.body = body

    def __str__(self):
        constraint_literals = []
        for constobj in self.body:
            if isinstance(constobj, Literal):
                constraint_literals.append(str(constobj))
            elif isinstance(constobj, ConstOpt):
                arga, argb = constobj.arguments
                if isinstance(arga, ConstVar):
                    arga = arga.name
                else:
                    arga = str(arga)
                if isinstance(argb, ConstVar):
                    argb = argb.name
                else:
                    argb = str(argb)
                constraint_literals.append(f'{arga}{constobj.operation}{argb}')

        if self.head:
            return f'{self.head} :- {", ".join(constraint_literals)}'
        else:
            return f':- {", ".join(constraint_literals)}'

    # @profile
    def all_vars(self):
        vars = set()
        for constobj in self.body:
            for arg in constobj.arguments:
                if isinstance(arg, ConstVar):
                    vars.add(arg)
                if isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            vars.add(t_arg)

        return vars