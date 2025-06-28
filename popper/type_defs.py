from typing import FrozenSet, NamedTuple, Tuple


class Literal(NamedTuple):
    predicate: str
    arguments: tuple


Rule = Tuple[Literal, FrozenSet[Literal]]
RuleBase = FrozenSet[Rule]
