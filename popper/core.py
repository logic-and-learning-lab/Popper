from collections import namedtuple
# from dataclasses import dataclass
import dataclasses


# ConstVar = namedtuple('ConstVar', ['name', 'type'])
# ConstVar2 = namedtuple('ConstVar2', ['rule', 'name', 'type'])
# ConstVar = namedtuple('ConstVar', ['name', 'type'])

@dataclasses.dataclass(frozen=True)
class Var:
    name: str
    pass

@dataclasses.dataclass(frozen=True)
class RuleVar(Var):
    pass
    # name: str
    # def __init__(self, name):
        # self.name = name

@dataclasses.dataclass(frozen=True)
class VarVar(Var):
    # pass
    rule: RuleVar
    # def __init__(self, rule, name):
        # self.rule = rule
        # self.name = name

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

    # TODO: REFACTOR
    def __str__(self):
        if self.directions:
            vdirections = (var_dir + var for var, var_dir in zip(self.arguments, self.directions))
            x = f'{self.predicate}({",".join(vdirections)})'
            if not self.positive:
                x = 'not ' + x
            return x

        args = []
        for arg in self.arguments:
            if isinstance(arg, Var):
                args.append(arg.name)
            elif isinstance(arg, tuple):
                t_args = []
                for t_arg in arg:
                    if isinstance(t_arg, Var):
                        t_args.append(t_arg.name)
                    else:
                        t_args.append(str(t_arg))
                if len(t_args) > 1:
                    args.append(f'({",".join(str(x) for x in t_args)})')

                else:
                    args.append(f'({",".join(t_args)},)')
            else:
                args.append(str(arg))
        x = f'{self.predicate}({",".join(args)})'
        if not self.positive:
            x = 'not ' + x
        return x