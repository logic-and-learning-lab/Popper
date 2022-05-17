import os
import sys
import time
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . core import Literal
from . util import format_rule, order_rule, order_prog

class Tester():

    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()
        # self.prolog.retractall(f'pos_index(_,_)')
        # self.prolog.retractall(f'neg_index(_,_)')

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file

        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            self.prolog.consult(x)

        list(self.prolog.query('load_examples'))

        self.pos_index = {}
        self.neg_index = {}

        # for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,Atom)'):
        #     index = x['I']
        #     atom = x['Atom']
        #     self.pos_index[index] = atom
        # for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,Atom)'):
        #     index = x['I']
        #     atom = x['Atom']
        #     self.neg_index[index] = atom

        for i, atom in enumerate(next(self.prolog.query('findall(Atom,pos_index(I,Atom),Xs)'))['Xs']):
            self.pos_index[i+1] = atom
        for i, atom in enumerate(next(self.prolog.query('findall(Atom,neg_index(I,Atom),Xs)'))['Xs']):
            i = i+1
            self.neg_index[-i] = atom

        self.settings.pos = frozenset(self.pos_index.values())
        self.settings.neg = frozenset(self.neg_index.values())

        if self.settings.recursion_enabled:
            self.prolog.assertz(f'timeout({self.settings.eval_timeout})')

    def test_prog(self, prog):
        with self.using(prog):
            pos_covered = frozenset(next(self.prolog.query('pos_covered(Xs)'))['Xs'])
            pos_covered = frozenset(self.pos_index[i] for i in pos_covered)
            # neg_covered = frozenset(next(self.prolog.query('neg_covered(Xs)'))['Xs'])
            # neg_covered = frozenset(self.neg_index[i] for i in neg_covered)
            inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
        return pos_covered, inconsistent

    @contextmanager
    def using(self, prog):
        if self.settings.recursion_enabled:
            prog = order_prog(prog)
        current_clauses = set()
        try:
            for rule in prog:
                head, body = rule
                x = format_rule(order_rule(rule))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')



    # def is_non_functional(self, program):
    #     try:
    #         with self.using(program):
    #             return len(list(self.prolog.query(f'non_functional.'))) > 0
    #     except:
    #         return True

    # def is_functional(self, program):
    #     with self.using(program):
    #         return len(list(self.prolog.query(f'functional.'))) > 0