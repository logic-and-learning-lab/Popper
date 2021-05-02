from collections import namedtuple, defaultdict

ConstVar = namedtuple('ConstVar', ['name', 'type'])
ConstOpt = namedtuple('ConstOpt', ['operator', 'arguments', 'operation'])

class Literal:
    def __init__(self, predicate, arguments, modes = [], positive = True):
        self.predicate = predicate
        self.arguments = arguments
        self.arity = len(arguments)
        self.modes = modes
        self.positive = positive

        self.inputs = set(arg for mode, arg in zip(self.modes, self.arguments) if mode == '+')
        self.outputs = set(arg for mode, arg in zip(self.modes, self.arguments) if mode == '-')

    # AC: TODO - REFACTOR
    def __str__(self):
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
                    if len(t_args) > 1:
                        args.append(f'({",".join(t_args)})')
                    else:
                        args.append(f'({",".join(t_args)},)')
                else:
                    args.append(str(arg))
            x = f'{self.predicate}({",".join(args)})'
            if not self.positive:
                x = 'not ' + x
            return x

    def __hash__(self):
        return hash((self.predicate, self.arguments))

    def my_hash(self):
        return hash((self.predicate, self.arguments))

    def to_code(self):
        return f'{self.predicate}({",".join(self.arguments)})'

    def ground(self, assignment):
        ground_args = []
        for arg in self.arguments:
            if arg in assignment:
                ground_args.append(assignment[arg])
            # handles tuples of ConstVars
            elif isinstance(arg, tuple):
                ground_t_args = []
                # AC: really messy
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

        # compute all the 'vars' in the program
        self.all_vars = set()
        if head:
            self.all_vars.update(head.arguments)
        if body:
            for blit in body:
                self.all_vars.update(blit.arguments)

        # AC: simplistic definition of recursive
        if head and body:
            self.recursive = head.predicate in set(blit.predicate for blit in body)
        else:
            self.recursive = False

    def __str__(self):
        x = f':- {", ".join(str(literal) for literal in self.body)}.'
        if self.head:
            return f'{str(self.head)}{x}'
        return x

    # AC: nasty
    def to_ordered(self):

        ordered_body = []
        grounded_variables = self.head.inputs
        body_literals = set(self.body)

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                # AC: could cache for a micro-optimisation
                if literal.inputs.issubset(grounded_variables):
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
            grounded_variables = grounded_variables.union(selected_literal.outputs)
            body_literals = body_literals.difference({selected_literal})

        self.body = tuple(ordered_body)

    def to_code(self):
        return (
            f'{self.head.to_code()}:- '
            f'{",".join([blit.to_code() for blit in self.body])}'
        )

    def ground(self, assignment):
        ground_body = tuple(blit.ground(assignment) for blit in self.body)
        if self.head:
            return Clause(self.head.ground(assignment), ground_body, self.min_num)
        return Clause(None, ground_body, self.min_num)

    def my_hash(self):
        if self.head:
            xs = (self.head.my_hash(), ) + tuple(lit.my_hash() for lit in self.body)
            return hash((self.head.my_hash(), ) + tuple(lit.my_hash() for lit in self.body))
        else:
            return hash(tuple(lit.my_hash for lit in self.body))

class Program:
    def __init__(self, clauses, before = defaultdict(set)):
        self.clauses = clauses
        self.before = before
        self.num_clauses = len(clauses)
        # AC: this method changes the program!
        # AC: no mutating please, it is the devils work!
        for clause in self.clauses:
            clause.to_ordered()

    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'

    def my_hash(self):
        return hash(tuple(clause.my_hash() for clause in self.clauses))

class Constraint:
    def __init__(self, ctype, head, body):
        self.ctype = ctype
        self.head = head
        self.body = body

        # AC: also includes constants, so the name is incorrect
        self.all_vars = set()
        for lit in body:
            for arg in lit.arguments:
                if isinstance(arg, ConstVar):
                    self.all_vars.add(arg)
                if isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            self.all_vars.add(t_arg)

    def myhash(self):
        cons = set()
        for lit in self.body:
            if not isinstance(lit, ConstOpt):
                continue
            cons.add((lit.operation, lit.arguments))
        return hash((frozenset(self.all_vars),frozenset(cons)))

    def __str__(self):
        constraint_literals = []
        for constobj in self.body:
            if isinstance(constobj, Literal):
                constraint_literals.append(str(constobj))
            elif isinstance(constobj, ConstOpt):
                if constobj.operation == 'AllDifferent':
                    # print(f'ALLDIFF:{constobj.arguments}')
                    # AC: TODO!!!
                    continue
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

        x = f':- {", ".join(constraint_literals)}.'
        if self.head:
            return f'{self.head} {x}'
        return x
