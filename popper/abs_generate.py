import abc
from collections import defaultdict
from typing import Set, TYPE_CHECKING, List

import clingo

from . type_defs import Literal, RuleBase


if TYPE_CHECKING:
    from . util import Settings


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

    @abc.abstractmethod
    def build_encoding(self, bkcons: List, settings: "Settings") -> str:
        """Build and return a string for an ASP solver, used to generate hypotheses."""
        pass

    @abc.abstractmethod
    def init_solver(self, encoding: str) -> clingo.Control:
        """Incorporate the `encoding` into a new solver (`clingo.Control`) and return it."""
        pass