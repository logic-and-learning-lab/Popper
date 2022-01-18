from pyswip import Prolog

import os
import sys
import time
import pkg_resources
from contextlib import contextmanager
from . core import Clause, Literal
from datetime import datetime

class Tester():
    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()
        self.eval_timeout = settings.eval_timeout
        self.already_checked_redundant_literals = set()
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

        self.pos = [x['I'] for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,_)')]
        self.neg = [x['I'] for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,_)')]

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

    def check_redundant_literal(self, program):
        for clause in program:
            k = Clause.clause_hash(clause)
            if k in self.already_checked_redundant_literals:
                continue
            self.already_checked_redundant_literals.add(k)
            (head, body) = clause
            C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
            res = list(self.prolog.query(f'redundant_literal({C})'))
            if res:
                yield clause

    def check_redundant_clause(self, program):
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
        prog = []
        for (head, body) in program:
            C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
            prog.append(C)
        prog = f"[{','.join(prog)}]"
        return list(self.prolog.query(f'redundant_clause({prog})'))

    def is_non_functional(self, program):
        with self.using(program):
            return list(self.prolog.query(f'non_functional.'))

    def success_set(self, rules):
        prog_hash = frozenset(rule for rule in rules)
        if prog_hash not in self.seen_prog:
            with self.using(rules):
                self.seen_prog[prog_hash] = set(next(self.prolog.query('success_set(Xs)'))['Xs'])
        return self.seen_prog[prog_hash]

    def test(self, rules):
        if all(Clause.is_separable(rule) for rule in rules):
            covered = set()
            for rule in rules:
                covered.update(self.success_set([rule]))
        else:
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

    def is_totally_incomplete(self, rule):
        if not Clause.is_separable(rule):
            return False
        return not any(x in self.success_set([rule]) for x in self.pos)

    def is_inconsistent(self, rule):
        if not Clause.is_separable(rule):
            return False
        return any(x in self.success_set([rule]) for x in self.neg)
