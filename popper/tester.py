from pyswip import Prolog

import re
import os
from contextlib import contextmanager
from . constrain import Outcome

class Tester():
    def __init__(self, kbpath, eval_timeout = 0.1, minimal_testing = True):
        self.prolog = Prolog()
        self.eval_timeout = eval_timeout
        self.minimal_testing = minimal_testing
        self.num_pos = 0
        self.num_neg = 0
        self.load_basic(kbpath)

    def load_basic(self, kbpath):
        # Consult background and test file
        self.prolog.consult(kbpath + 'bk.pl')
        self.prolog.consult(os.path.abspath('popper') + '/test.pl')

        # Read example file
        with open(kbpath + 'exs.pl') as f:
            for line in f:
                if line.startswith('%'):
                    continue
                # Assert negative and positive examples
                for x in re.findall("pos\((.*)\)\.", line):
                    self.prolog.assertz(f'pos({x})')
                    self.num_pos += 1
                for x in re.findall("neg\((.*)\)\.", line):
                    self.prolog.assertz(f'neg({x})')
                    self.num_neg += 1

        self.prolog.assertz(f'num_pos({self.num_pos})')
        self.prolog.assertz(f'num_neg({self.num_neg})')
        self.prolog.assertz(f'timeout({self.eval_timeout})')

    @contextmanager
    def using(self, program):
        current_clauses = set()
        try:
            for clause in program.clauses:
                self.prolog.assertz(clause.to_code())
                current_clauses.add((clause.head.predicate, clause.head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

    def test(self, program):
        with self.using(program):
            if self.minimal_testing:
                res = list(self.prolog.query('do_test_minimal(TP,FN,TN,FP)'))[0]
            else:
                res = list(self.prolog.query('do_test(TP,FN,TN,FP)'))[0]
            TP, FN, TN, FP = res['TP'], res['FN'], res['TN'], res['FP']

        # @NOTE: Andrew to clean up at some point.
        # complete (FN=0)
        if TP == self.num_pos:
            positive_outcome = Outcome.ALL
        # totally incomplete (use TP==0 rather than TP=|E+| because of minimal testing)
        elif TP == 0 and FN > 0:
            positive_outcome = Outcome.NONE
        # incomplete
        else:
            positive_outcome = Outcome.SOME

        # consistent (FP=0)
        if FP == 0:
            negative_outcome = Outcome.NONE
        # totally inconsistent
        # AC: THIS LINE DOES NOT WORK WITH MINMAL TESTING
        # elif TN == 0:
            # negative_outcome = Outcome.All
        # inconsistent
        else:
            negative_outcome = Outcome.SOME

        return {program:(positive_outcome, negative_outcome)}