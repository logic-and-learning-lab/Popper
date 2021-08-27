from pyswip import Prolog

import re
import os
import sys
from . core import Clause, Literal
from contextlib import contextmanager

class Tester():
    def __init__(self, settings):
        self.prolog = Prolog()
        self.eval_timeout = settings.eval_timeout
        self.test_all = settings.test_all
        self.num_pos = 0
        self.num_neg = 0
        self.load_basic(settings)
        self.seen_clause = set()

    def first_result(self, q):
        return list(self.prolog.query(q))[0]

    def load_basic(self, settings):
        bk_pl_path = os.path.join(settings.bk_file)
        exs_pl_path = os.path.join(settings.ex_file)
        test_pl_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'test.pl')

        for x in [bk_pl_path, exs_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            self.prolog.consult(x)

        self.num_pos = int(self.first_result('count_pos(N)')['N'])
        self.num_neg = int(self.first_result('count_neg(N)')['N'])
        self.prolog.assertz(f'timeout({self.eval_timeout})')
        self.prolog.assertz(f'num_pos({self.num_pos})')
        self.prolog.assertz(f'num_neg({self.num_neg})')

    @contextmanager
    def using(self, program):
        current_clauses = set()
        try:
            for clause in program:
                (head, body) = clause
                self.prolog.assertz(Clause.to_code(Clause.to_ordered(clause)))
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

    def check_redundant_literal(self, program):
        for clause in program:
            k = Clause.clause_hash(clause)
            if k in self.seen_clause:
                continue
            self.seen_clause.add(k)
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

    def test(self, program):
        with self.using(program):
            try:
                if self.test_all:
                    res = self.first_result('do_test(TP,FN,TN,FP)')
                else:
                    # AC: TN is not calculated when performing minmal testing
                    res = self.first_result('do_test_minimal(TP,FN,TN,FP)')
                return (res['TP'], res['FN'], res['TN'], res['FP'])
            except:
                print("A Prolog error occurred when testing the program:")
                for clause in program:
                    print('\t' + Clause.to_code(clause))
                raise