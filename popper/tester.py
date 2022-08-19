import os
import numpy as np
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog

class Tester():

    def query(self, query, key):
        return set(next(self.prolog.query(query))[key])

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


    # neg_covered = frozenset(next(self.prolog.query('neg_covered(Xs)'))['Xs'])
    # neg_covered = frozenset(self.neg_index[i] for i in neg_covered)

    def test_prog(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            pos_covered = frozenset(self.pos_index[i] for i in pos_covered)
            inconsistent = False
            if len(self.neg_index):
                inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
        return pos_covered, inconsistent

    def is_inconsistent(self, prog):
        if len(self.neg_index) == 0:
            return False
        with self.using(prog):
            return len(list(self.prolog.query("inconsistent"))) > 0

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

    def reduce_solution(self, prog):
        if len(prog) < 3:
            return prog
        pos_covered, _inconsistent = self.test_prog(prog)
        return self.reduce_solution_aux(prog, pos_covered)

    def reduce_solution_aux(self, prog, orignal_covered):
        if len(prog) < 3:
            return prog
        # print('HELLO')
        for i in range(len(prog)):
            subprog = prog[:i] + prog[i+1:]
            if not prog_is_recursive(subprog):
                continue
            # for rule in subprog:
                # print('\t', format_rule(rule))
            with self.using(subprog):
                pos_covered, inconsistent = self.test_prog(subprog)
                # print(inconsistent, len(pos_covered), len(orignal_covered))
                if inconsistent:
                    continue
                if pos_covered == orignal_covered:
                    return self.reduce_solution_aux(subprog, orignal_covered)

        return prog