import abc
from typing import FrozenSet, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from . util import Literal, Settings

Rule = Tuple['Literal', FrozenSet['Literal']]
RuleBase = FrozenSet[Rule]

class Generator(abc.ABC):
    settings: 'Settings'

    # @profile
    def get_prog(self) -> RuleBase:
        pass

    def gen_symbol(self, literal, backend):
        pass

    def update_solver(self, size):
        pass
        self.update_number_of_literals(size)

    def update_number_of_literals(self, size):
        pass

    def update_number_of_vars(self, size):
        pass

    def update_number_of_rules(self, size):
        pass

    def prune_size(self, size):
        pass

    # @profile
    def get_ground_rules(self, rule):
        pass

    def parse_handles(self, new_handles):
        pass

    def constrain(self, tmp_new_cons):
        pass