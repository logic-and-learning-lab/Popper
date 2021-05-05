from pyswip import Prolog

import re
import os
import sys
from contextlib import contextmanager
from . constrain import Outcome

class Tester():
    def __init__(self, experiment):
        self.prolog = Prolog()
        self.eval_timeout = experiment.args.eval_timeout
        self.test_all = experiment.args.test_all
        self.num_pos = 0
        self.num_neg = 0
        self.load_basic(experiment.args.kbpath)
        self.seen_clause = set()

    def load_basic(self, kbpath):
        # Consult background and test file
        bk_pl_path = os.path.join(kbpath, 'bk.pl')
        test_pl_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'test.pl')
        if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
            bk_pl_path = bk_pl_path.replace('\\', '\\\\')
            test_pl_path = test_pl_path.replace('\\', '\\\\')
        self.prolog.consult(bk_pl_path)
        self.prolog.consult(test_pl_path)

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

    def check_redundant_literal(self, program):
        for clause in program.clauses:
            k = clause.my_hash()
            if k in self.seen_clause:
                continue
            self.seen_clause.add(k)
            C = f"[{','.join(('not_'+ clause.head.to_code(),) + tuple(lit.to_code() for lit in clause.body))}]"
            res = list(self.prolog.query(f'redundant_literal({C})'))
            if res:
                yield clause

    # AC: THE OVERHEAD OF THIS CHECK IS MINIMAL
    # AC: IF IT BECOMES TO HIGH, SUCH AS WHEN LEARNING PROGRAMS WITH LOTS OF CLAUSES, THEN WE CAN IMPROVE IT BY NOT COMPARING ALREADY COMPARED CLAUSES
    def check_redundant_clause(self, program):
        prog = []
        for clause in program.clauses:
            C = f"[{','.join(('not_'+ clause.head.to_code(),) + tuple(lit.to_code() for lit in clause.body))}]"
            prog.append(C)
        prog = f"[{','.join(prog)}]"
        res = list(self.prolog.query(f'redundant_clause({prog})'))
        return res

    def test(self, program):
        with self.using(program):
            try:
                if self.test_all:
                    res = list(self.prolog.query('do_test(TP,FN,TN,FP)'))[0]
                else:
                    # AC: TN is not calculated when performing minmal testing
                    res = list(self.prolog.query('do_test_minimal(TP,FN,TN,FP)'))[0]
            except:
                print("A Prolog error occurred when testing the program:")
                for clause in program.clauses:
                    print('\t' + clause.to_code())
                raise
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

        return ((positive_outcome, negative_outcome), (TP,FN,TN,FP))