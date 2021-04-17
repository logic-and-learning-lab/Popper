import clingo
import re
import os
from contextlib import contextmanager
from . constrain import Outcome

class ASPTester():
    def __init__(self, kbpath):
        with open(kbpath + 'bk.pl') as f:
            self.bk = f.read()
        with open(kbpath + 'exs.pl') as f:
            for line in f:
                if line.startswith('%'):
                    continue
                self.pos = re.findall("pos\((.*)\)\.", line)
                self.neg = re.findall("neg\((.*)\)\.", line)
        self.num_pos = len(self.pos)
        self.num_neg = len(self.neg)
        self.cnt = 0

    # @profile
    def test(self, program):
        self.solver = clingo.Control(['--rand-freq=0'])
        self.solver.add('bk', [], self.bk)
        # self.solver.add('bk', [], '#show f/1.')
        self.solver.ground([('bk', [])])

        # import time
        self.cnt+=1

        key = f'prog{self.cnt}'
        # print('--')
        code = '\n'.join(program.to_code())
        code = code.replace('f(',f'{key}(')
        # print(code)
        # t1 = time.time()
        self.solver.add(key, [], code)
        # t2 = time.time()
        # d = t2-t1

        # t1 = time.time()
        self.solver.ground([(key, [])])
        # t2 = time.time()
        # e = t2-t1

        with self.solver.solve(yield_ = True) as handle:
            # t1 = time.time()
            m = handle.model()
            # t2 = time.time()
            # f = t2-t1

            # t1 = time.time()
            m.symbols(shown = True)
            # t2 = time.time()
            # g = t2-t1

            # print(self.cnt, d,e,f,g)

            # if m:
                # for x in m.symbols(shown = True):
                    # print(x)

            # None indicates no model could be found (could try with more allowed literals)
            # model = handle.get()
            # NB: as long as we are doing datalog there is only going to be one model (at most)
            # model.symbols(atoms=True)
            # atom_strs = set(map(str, self.atoms))
        TP = 0
        FN = 1
        TN = 0
        FP = 1

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