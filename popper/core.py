from collections import namedtuple, defaultdict

ConstVar = namedtuple('ConstVar', ['name', 'type'])

class Constrainer:
    def __init__(self, settings):
        self.elim_cons = set()
        self.gen_cons = set()
        self.spec_cons = {x: set() for x in settings.pos}

    def add_elimination(self, con):
        self.elim_cons.add(con)

    def add_generalisation(self, con):
        self.gen_cons.add(con)

    def add_specialisation(self, con, e):
        self.spec_cons[e].add(con)


class Literal:
    def __init__(self, predicate, arguments, directions = [], positive = True, meta=False):
        self.predicate = predicate
        self.arguments = arguments
        self.arity = len(arguments)
        self.directions = directions
        self.positive = positive
        self.meta = meta
        self.inputs = frozenset(arg for direction, arg in zip(self.directions, self.arguments) if direction == '+')
        self.outputs = frozenset(arg for direction, arg in zip(self.directions, self.arguments) if direction == '-')

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