from pyswip import Prolog

import re
import os
from enum import Enum
from contextlib import contextmanager

class Tester():
    def __init__(self, kbpath, eval_timeout = 0.1, minimal_testing = True):
        self.prolog = Prolog()
        self.eval_timeout = eval_timeout
        self.minimal_testing = minimal_testing

        # AC: no need to maintain these
        self.pos_examples = []
        self.neg_examples = []
        self.num_pos = 0
        self.num_neg = 0

        self.load_basic(kbpath)

    def load_basic(self, kbpath):
        # Consult background and test file
        self.prolog.consult(kbpath + 'bk.pl')
        testfile_path = os.path.abspath('popper') + '/test.pl'
        self.prolog.consult(testfile_path)

        # Read example file
        with open(kbpath + 'exs.pl') as f:
            for line in f:
                if line.startswith('%'):
                    continue
                self.pos_examples.extend(re.findall("pos\((.*)\)\.", line))
                self.neg_examples.extend(re.findall("neg\((.*)\)\.", line))

        # Assert negative and positive examples
        for example in self.pos_examples:
            self.prolog.assertz(f'pos({example})')
        for example in self.neg_examples:
            self.prolog.assertz(f'neg({example})')

        # Assert evaluation timeout
        self.prolog.assertz(f'timeout({self.eval_timeout})')

        self.num_pos = len(self.pos_examples)
        self.num_neg = len(self.neg_examples)

        self.prolog.assertz(f'num_pos({self.num_pos})')
        self.prolog.assertz(f'num_neg({self.num_neg})')

    @contextmanager
    def using(self, program):
        current_clauses = set()
        try:
            for clause in program:
                self.prolog.assertz(clause.to_code())
                current_clauses.add(clause)
            yield
        finally:            
            for clause in current_clauses:
                clause_head = clause.head
                args = ','.join(['_'] * clause_head.arity)
                self.prolog.retractall(f'{clause_head.predicate}({args})')
                self.prolog.retractall(f'{clause_head.predicate}({args},_,_)')

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
            positive_outcome = 'all'
        # totally incomplete (use TP==0 rather than TP=|E+| because of minimal testing)
        elif TP == 0 and FN > 0:
            positive_outcome = 'none'
        # incomplete
        else:
            positive_outcome = 'some'

        # consistent (FP=0)
        if FP == 0:
            negative_outcome = 'none'
        # totally inconsistent
        # AC: THIS LINE DOES NOT WORK WITH MINMAL TESTING
        # elif TN == 0:
            # negative_outcome = Outcome.All
        # inconsistent
        else:
            negative_outcome = 'some'

        return {program:(positive_outcome, negative_outcome)}