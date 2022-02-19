from pyswip import Prolog

import os
import sys
import time
import pkg_resources
from contextlib import contextmanager
from . core import Clause, Literal
from . util import format_program
from datetime import datetime

class Tester():
    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()
        self.eval_timeout = settings.eval_timeout
        self.cached_redundant_literals = {}
        self.seen_tests = {}
        self.seen_prog = {}

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            self.prolog.consult(x)

        # load examples
        list(self.prolog.query('load_examples'))

        # self.pos = [x['I'] for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,_)')]
        # self.neg = [x['I'] for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,_)')]

        self.pos = []
        self.neg = []
        self.pos_atoms = []
        self.neg_atoms = []

        for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,Atom)'):
            self.pos.append(x['I'])
            self.pos_atoms.append(x['Atom'])

        for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,Atom)'):
            self.neg.append(x['I'])
            self.neg_atoms.append(x['Atom'])

        self.prolog.assertz(f'timeout({self.eval_timeout})')

    def first_result(self, q):
        return list(self.prolog.query(q))[0]

    @contextmanager
    def using(self, rules):
        current_clauses = set()
        try:
            for rule in rules:
                (head, body) = rule
                self.prolog.assertz(Clause.to_code(Clause.to_ordered(rule)))
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

    # def check_redundant_literal(self, program):
    #     for clause in program:
    #         k = Clause.clause_hash(clause)
    #         if k in self.cached_redundant_literals:
    #             continue
    #         self.cached_redundant_literals.add(k)
    #         (head, body) = clause
    #         C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
    #         res = list(self.prolog.query(f'redundant_literal({C})'))
    #         if res:
    #             yield clause

    def rule_has_redundant_literal(self, rule):
        k = hash(rule)
        if k in self.cached_redundant_literals:
            return self.cached_redundant_literals[k]

        head, body = rule
        C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
        has_redundant_literal = len(list(self.prolog.query(f'redundant_literal({C})'))) > 0
        self.cached_redundant_literals[k] = has_redundant_literal
        return has_redundant_literal

    # def check_redundant_clause(self, program):
    #     # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
    #     prog = []
    #     for (head, body) in program:
    #         C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
    #         prog.append(C)
    #     prog = f"[{','.join(prog)}]"
    #     return list(self.prolog.query(f'redundant_clause({prog})'))

    def is_non_functional(self, program):
        try:
            with self.using(program):
                return len(list(self.prolog.query(f'non_functional.'))) > 0
        except:
            print('ERROR!!!!!!!!!')
            print(format_program(program))
            return True

    def is_functional(self, program):
        with self.using(program):
            return len(list(self.prolog.query(f'functional.'))) > 0

    def success_set(self, rules):
        k = hash(frozenset(rules))

        if k in self.seen_prog:
            return self.seen_prog[k]

        if len(rules) == 1 or not all(Clause.is_separable(rule) for rule in rules):
            with self.using(rules):
                xs = set(next(self.prolog.query('success_set(Xs)'))['Xs'])
                self.seen_prog[k] = xs
                return xs

        xs = set()
        for rule in rules:
            xs.update(self.success_set([rule]))
        self.seen_prog[k] = xs
        return xs

    def pos_covered(self, rules):
        k = hash(frozenset(rules))

        if k in self.seen_prog:
            return self.seen_prog[k]

        if len(rules) == 1 or not all(Clause.is_separable(rule) for rule in rules):
            with self.using(rules):
                xs = set(next(self.prolog.query('pos_covered(Xs)'))['Xs'])
                self.seen_prog[k] = xs
                return xs

        xs = set()
        for rule in rules:
            xs.update(self.success_set([rule]))
        self.seen_prog[k] = xs
        return xs

    def is_inconsistent(self, rules):
        with self.using(rules):
            return len(list(self.prolog.query('inconsistent'))) > 0

    def my_test(self, stats, rules):
        with self.using(rules):
            with stats.duration('pos_covered'):
                coverage = set(next(self.prolog.query('pos_covered(Xs)'))['Xs'])
            if len(coverage) == 0:
                return None, coverage
            with stats.duration('inconsistent'):
                inconsistent = len(list(self.prolog.query('inconsistent'))) > 0
            return inconsistent, coverage

    def find_redundant_clauses(self, rules):
        prog = []
        for i, (head, body) in enumerate(rules):
            C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
            C = f'{i}-{C}'
            prog.append(C)
        prog = f"[{','.join(prog)}]"
        res = self.prolog.query(f'find_redundant_clauses({prog},R0,R1)')

        for dic in res:
            r0 = dic['R0']
            r1 = dic['R1']
            yield rules[r0], rules[r1]

    def test(self, rules):
        covered = self.success_set(rules)

        tp, fn, tn, fp = 0, 0, 0, 0

        for p in self.pos:
            if p in covered:
                tp +=1
            else:
                fn +=1
        for n in self.neg:
            if n in covered:
                fp +=1
            else:
                tn +=1

        return tp, fn, tn, fp

    def is_complete(self, rules):
        return all(x in self.success_set(rules) for x in self.pos)

    def is_consistent(self, rules):
        return all(x not in self.success_set(rules) for x in self.neg)

    def is_incomplete(self, rules):
        return any(x not in self.success_set(rules) for x in self.pos)

    def is_totally_incomplete(self, rules):
        return all(x not in self.success_set(rules) for x in self.pos)

