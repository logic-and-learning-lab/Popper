import os
import time
import numpy as np
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive

import clingo
import clingo.script
import pkg_resources
from . core import Literal
from . generate import parse_model
from collections import defaultdict

class Tester():

    def query(self, query, key):
        result = next(self.prolog.query(query))[key]
        result = map(lambda s: s.replace("'", "") if type(s) == str else s, result)
        return set(result)

    def bool_query(self, query,):
        return len(list(self.prolog.query(query))) > 0

    # TODO: COULD PUSH TO CLINGO TO SAVE PROLOG FROM HAVING TO INDEX STUFF
    def get_examples(self):
        pos = set()
        neg = set()

        pos = self.query('findall(X,pos(X),Xs)', 'Xs')

        if self.bool_query('current_predicate(neg/1)'):
            neg = self.query('findall(X,neg(X),Xs)', 'Xs')

        self.settings.stats.logger.info(f'Num. pos examples: {len(pos)}')
        self.settings.stats.logger.info(f'Num. neg examples: {len(neg)}')

        if self.settings.max_examples < len(pos):
            self.settings.stats.logger.info(f'Sampling {self.settings.max_examples} pos examples')
            pos = np.random.choice(list(pos), self.settings.max_examples)
        if self.settings.max_examples < len(neg):
            self.settings.stats.logger.info(f'Sampling {self.settings.max_examples} neg examples')
            neg = np.random.choice(list(neg), self.settings.max_examples)

        self.cached_explain = set()
        self.seen_prog = set()

        return pos, neg

    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            self.prolog.consult(x)

        self.pos_index = {}
        self.neg_index = {}

        pos, neg = self.get_examples()
        self.num_pos = len(pos)
        self.num_neg = len(neg)

        for i, atom in enumerate(pos):
            k = i+1
            self.prolog.assertz(f'pos_index({k},{atom})')
            self.pos_index[k] = atom

        for i, atom in enumerate(neg):
            k = -(i+1)
            self.prolog.assertz(f'neg_index({k},{atom})')
            self.neg_index[k] = atom

        self.settings.pos = frozenset(self.pos_index.values())
        self.settings.neg = frozenset(self.neg_index.values())

        if self.settings.recursion_enabled:
            self.prolog.assertz(f'timeout({self.settings.eval_timeout})')

    def test_prog(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            pos_covered = frozenset(self.pos_index[i] for i in pos_covered)
            inconsistent = False
            if len(self.neg_index) > 0:
                inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
        return pos_covered, inconsistent

    def is_inconsistent(self, prog):
        if len(self.neg_index) == 0:
            return False
        with self.using(prog):
            return len(list(self.prolog.query("inconsistent"))) > 0

    def is_complete(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            return len(pos_covered) == self.num_pos

    def get_pos_covered(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            pos_covered = frozenset(self.pos_index[i] for i in pos_covered)
            return pos_covered

    @contextmanager
    def using(self, prog):
        if self.settings.recursion_enabled:
            prog = order_prog(prog)
        current_clauses = set()
        try:
            for rule in prog:
                head, _body = rule
                x = format_rule(order_rule(rule))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

    def is_non_functional(self, prog):
        with self.using(prog):
            return self.bool_query('non_functional')

    def reduce_inconsistent(self, program):
        if len(program) < 3:
            return program
        for i in range(len(program)):
            subprog = program[:i] + program[i+1:]
            if not prog_is_recursive(subprog):
                continue
            with self.using(subprog):
                if self.is_inconsistent(subprog):
                    return self.reduce_inconsistent(subprog)
        return program

    def is_sat(self, prog):
        with self.using(prog):
            return self.bool_query('sat')

    def is_body_sat(self, body):
        try:
            body_str = ','.join(format_literal(literal) for literal in body)
            query = f'sat2:- {body_str}'
            self.prolog.assertz(query)
            return self.bool_query('sat2')
        finally:
            self.prolog.retractall('sat2')

    def check_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
            res = list(self.prolog.query(f'redundant_literal({c})'))
            if res:
                yield rule

    def has_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
            res = list(self.prolog.query(f'redundant_literal({c})'))
            if res:
                return True
        return False


    def has_redundant_rule_(self, prog):
        prog_ = []
        for head, body in prog:
            c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        # print(prog_)
        return len(list(self.prolog.query(f'redundant_clause({prog_})'))) > 0
            # print(prog_)
            # return True
        # return False


    def has_redundant_rule(self, prog):
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses

        base = []
        step = []
        for rule in prog:
            if rule_is_recursive(rule):
                step.append(rule)
            else:
                base.append(rule)
        if len(base) > 1 and self.has_redundant_rule_(base):
            return True
        if len(step) > 1 and self.has_redundant_rule_(step):
            return True
        return False

    # WE ASSUME THAT THERE IS A REUNDANT RULE
    def find_redundant_rule_(self, prog):
        prog_ = []
        for i, (head, body) in enumerate(prog):
            c = f"{i}-[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        # print(prog_)
        q = f'find_redundant_rule({prog_},K1,K2)'
        res = list(self.prolog.query(q))
        k1 = res[0]['K1']
        k2 = res[0]['K2']
        return prog[k1], prog[k2]
        # return len(res) > 0

    def find_redundant_rules(self, prog):
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
        base = []
        step = []
        for rule in prog:
            if rule_is_recursive(rule):
                step.append(rule)
            else:
                base.append(rule)
        if len(base) > 1 and self.has_redundant_rule(base):
            return self.find_redundant_rule_(base)
        if len(step) > 1 and self.has_redundant_rule(step):
            return self.find_redundant_rule_(step)
        return None


