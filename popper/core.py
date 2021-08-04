from collections import namedtuple, defaultdict

ConstVar = namedtuple('ConstVar', ['name', 'type'])
ConstOpt = namedtuple('ConstOpt', ['operator', 'arguments', 'operation'])

class Grounding:
    @staticmethod
    def ground_literal(literal, assignment):
        ground_args = []
        for arg in literal.arguments:
            if arg in assignment:
                ground_args.append(assignment[arg])
            # handles tuples of ConstVars
            # TODO: AC: EXPLAIN BETTER
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
        return (literal.positive, literal.predicate, tuple(ground_args))

    @staticmethod
    def ground_rule(head, body, assignment):
        ground_head = None
        if head:
            ground_head = Grounding.ground_literal(head, assignment)
        ground_body = frozenset(Grounding.ground_literal(literal, assignment) for literal in body)
        return (ground_head, ground_body)

    # AC: When grounding constraint rules, we only care about the vars and the constraints, not the actual literals
    @staticmethod
    def grounding_hash(body, all_vars):
        cons = set()
        for lit in body:
            if not isinstance(lit, ConstOpt):
                continue
            cons.add((lit.operation, lit.arguments))
        return hash((frozenset(all_vars), frozenset(cons)))

    @staticmethod
    def find_all_vars(body):
        all_vars = set()
        for literal in body:
            for arg in literal.arguments:
                if isinstance(arg, ConstVar):
                    all_vars.add(arg)
                elif isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            all_vars.add(t_arg)
        return all_vars

class Literal:
    def __init__(self, predicate, arguments, directions = [], positive = True):
        self.predicate = predicate
        self.arguments = arguments
        self.arity = len(arguments)
        self.directions = directions
        self.positive = positive
        self.inputs = set(arg for direction, arg in zip(self.directions, self.arguments) if direction == '+')
        self.outputs = set(arg for direction, arg in zip(self.directions, self.arguments) if direction == '-')

    # AC: TODO - REFACTOR
    def __str__(self):
        if self.directions:
            vdirections = (var_dir + var for var, var_dir in zip(self.arguments, self.directions))
            x = f'{self.predicate}({",".join(vdirections)})'
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
        return self.my_hash()

    def __eq__(self, other):
        if other == None:
            return False
        return self.my_hash() == other.my_hash()

    def my_hash(self):
        return hash((self.predicate, self.arguments))

    def to_code(self):
        return f'{self.predicate}({",".join(self.arguments)})'

class Clause:
    def __init__(self, head, body, min_num = 0):
        self.head = head
        self.body = body
        self.min_num = min_num

    def __str__(self):
        x = f':- {", ".join(str(literal) for literal in self.body)}.'
        if self.head:
            return f'{str(self.head)}{x}'
        return x

    def is_recursive(self):
        if self.head:
            return self.head.predicate in set(literal.predicate for literal in self.body if isinstance(literal, Literal))
        return False

    def all_vars(self):
        xs = set()
        if self.head:
            xs.update(self.head.arguments)
        for literal in self.body:
            for arg in literal.arguments:
                if isinstance(arg, ConstVar):
                    xs.add(arg)
                elif isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            xs.add(t_arg)
        return xs

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

    def my_hash(self):
        h = None
        if self.head:
            h = (self.head.my_hash(),)
        b = frozenset(literal.my_hash() for literal in self.body)
        return hash((h,b))

class Program:
    def __init__(self, clauses, before = defaultdict(set)):
        self.clauses = clauses
        self.before = before
        self.num_clauses = len(clauses)
        for clause in self.clauses:
            clause.to_ordered()

    def to_code(self):
        for clause in self.clauses:
            yield clause.to_code() + '.'