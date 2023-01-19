import dataclasses

@dataclasses.dataclass(frozen=True)
class Var:
    name: str

@dataclasses.dataclass(frozen=True)
class RuleVar(Var):
    pass

@dataclasses.dataclass(frozen=True)
class VarVar(Var):
    rule: RuleVar

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