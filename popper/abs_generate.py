import abc
from collections import defaultdict
from typing import Set, TYPE_CHECKING, List, Optional

import clingo

from . type_defs import Literal, RuleBase


if TYPE_CHECKING:
    from . util import Settings


class Generator(abc.ABC):
    settings: 'Settings'
    solver: clingo.Control
    handle: Optional[clingo.SolveHandle]
    model: Optional[clingo.Model]

    @abc.abstractmethod
    def get_prog(self) -> RuleBase:
        pass

    # @abc.abstractmethod
    # def gen_symbol(self, literal, backend):
    #     pass

    @abc.abstractmethod
    def update_solver(self, size):
        pass

    # @abc.abstractmethod
    # def update_number_of_literals(self, size):
    #     pass

    # @abc.abstractmethod
    # def update_number_of_vars(self, size):
    #     pass

    # @abc.abstractmethod
    # def update_number_of_rules(self, size):
    #     pass

    @abc.abstractmethod
    def prune_size(self, size):
        pass

    # @abc.abstractmethod
    # def get_ground_rules(self, rule):
    #     pass

    # @abc.abstractmethod
    # def parse_handles(self, new_handles):
    #     pass

    @abc.abstractmethod
    def constrain(self, tmp_new_cons):
        pass

    @abc.abstractmethod
    def build_encoding(self, bkcons: List, settings: "Settings") -> str:
        """Build and return a string for an ASP solver."""

    @abc.abstractmethod
    def init_solver(self, encoding: str) -> clingo.Control:
        """Incorporate the `encoding` into a new solver and return it."""

    def parse_model_pi(self, model) -> RuleBase:
        settings = self.settings
        # directions = defaultdict(lambda: defaultdict(lambda: '?'))
        rule_index_to_body = defaultdict(set)
        rule_index_to_head = {}
        # rule_index_ordering = defaultdict(set)

        for atom in model:
            args = atom.arguments
            name = atom.name

            if name == 'body_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = settings.cached_atom_args[tuple(atom_args)]
                arity = len(atom_args)
                body_literal = (predicate, atom_args, arity)
                rule_index_to_body[rule_index].add(body_literal)

            elif name == 'head_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = settings.cached_atom_args[tuple(atom_args)]
                arity = len(atom_args)
                head_literal = (predicate, atom_args, arity)
                rule_index_to_head[rule_index] = head_literal

        prog = []

        for rule_index in rule_index_to_head:  # pylint: ignore=C0206
            head_pred, head_args, _head_arity = rule_index_to_head[rule_index]
            head = Literal(head_pred, head_args)
            body: Set[Literal] = set()
            for (body_pred, body_args, _body_arity) in rule_index_to_body[rule_index]:
                body.add(Literal(body_pred, body_args))
            rule = head, frozenset(body)
            prog.append((rule))

        return frozenset(prog)
