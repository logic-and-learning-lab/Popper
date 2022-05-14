from collections import defaultdict
from . util import format_rule
import clingo
import clingo.script
# from pyswip import Prolog
import time
# from . core import Literal


class Tester:
    def __init__(self, settings):
        self.settings = settings

        solver = clingo.Control()
        solver.add('bk', [], self.settings.bk)
        t1 = time.time()
        solver.ground([('bk', [])])
        t2 = time.time()
        print('g bk', t2-t1)

        prog = []
        hash_to_ex = {}
        # prog.append(self.settings.bk)
        for ex in self.settings.pos:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            x = f'pos({k},{ex}).'
            prog.append(x)

        for ex in self.settings.neg:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            x = f'neg({k},{ex}).'
            prog.append(x)

        solver.add('exs', [], '\n'.join(prog))
        t1 = time.time()
        solver.ground([('exs', [])])
        t2 = time.time()
        print('g exs', t2-t1)

        self.solver = solver
        self.hash_to_ex = hash_to_ex

        self.count = 0

    def test_rule0(self, rule):
        # xs = list(test_rules_clingo(self.settings, self.settings.bk, self.settings.pos, self.settings.neg, [rule]))
        # _, inconsistent, pos_covered = xs[0]
        # print(pos_covered

        TEST_PROG = """
            #defined pos_covers/2.
            #defined inconsistent/1.
            #defined pos/2.
            #defined neg/2.
            #defined holds/2.
            #show pos_covers/2.
            #show inconsistent/1.
            pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
            inconsistent(R):- neg(E,Atom), holds(R,Atom).
        """

        # prog = []
        prog = [TEST_PROG]

        HASH_RULE = f'"{hash(rule)}"'
        rule = format_rule(rule)
        rule = rule.replace(self.settings.head_str, f'holds({HASH_RULE},{self.settings.head_str})')
        prog.append(rule)
        prog = '\n'.join(prog)
        # print(prog)
        prog_key = f'prog_{self.count}'

        self.solver.add(prog_key, [], prog)

        t1 = time.time()
        self.solver.ground([(prog_key, [])])
        t2 = time.time()
        print('g prog', t2-t1)
        pos_covered = set()
        inconsistent = False
        t1 = time.time()
        with self.solver.solve(yield_=True) as handle:
            for m in handle:
                atoms = m.symbols(shown = True)
                for atom in atoms:
                    # print('ATOM', atom)
                    if atom.name == 'pos_covers':
                        rule_hash = str(atom.arguments[0])
                        if rule_hash != HASH_RULE:
                            continue
                        example_hash = str(atom.arguments[1])
                        # rule = hash_to_rule[rule_hash]
                        example = self.hash_to_ex[example_hash]
                        pos_covered.add(example)
                    elif atom.name == 'inconsistent':
                        rule_hash = str(atom.arguments[0])
                        # rule = hash_to_rule[rule_hash]
                        if rule_hash != HASH_RULE:
                            continue
                        inconsistent = True

        t2 = time.time()
        print('solving', t2-t1)
        return inconsistent, pos_covered


    def test_rule(self, rule):

        TEST_PROG = """
        #defined pos_covers/1.
        #defined inconsistent/0.
        #defined pos/2.
        #defined neg/2.
        #show pos_covers/1.
        #show neg_covers/1.
        #show inconsistent/0.
        pos_covers(E):- pos(E,Atom), holds(Atom).
        neg_covers(E):- neg(E,Atom), holds(Atom).
        inconsistent:- neg(E,Atom), holds(Atom).
        """


        # xs = list(test_rules_clingo(self.settings, self.settings.bk, self.settings.pos, self.settings.neg, [rule]))
        # _, inconsistent, pos_covered = xs[0]
        # print(pos_covered)

        solver = clingo.Control()
        solver.add('bk', [], self.settings.bk)
        t1 = time.time()
        solver.ground([('bk', [])])
        t2 = time.time()
        # print('g bk', t2-t1)

        prog = []
        hash_to_ex = {}
        # prog.append(self.settings.bk)
        for ex in self.settings.pos:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            x = f'pos({k},{ex}).'
            prog.append(x)

        for ex in self.settings.neg:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            x = f'neg({k},{ex}).'
            prog.append(x)

        solver.add('exs', [], '\n'.join(prog))
        t1 = time.time()
        solver.ground([('exs', [])])
        t2 = time.time()
        # print('g exs', t2-t1)

        # prog = []
        prog = [TEST_PROG]
        rule = format_rule(rule)
        rule = rule.replace(self.settings.head_str, f'holds({self.settings.head_str})')
        prog.append(rule)
        # prog.append(self.settings.bk)
        # prog = '\n'.join(prog)

        solver.add('prog', [], '\n'.join(prog))

        t1 = time.time()
        solver.ground([('prog', [])])
        t2 = time.time()
        # print('g prog', t2-t1)
        pos_covered = set()
        inconsistent = False
        t1 = time.time()
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms = m.symbols(shown = True)
                for atom in atoms:
                    if atom.name == 'pos_covers':
                        example_hash = str(atom.arguments[0])
                        example = hash_to_ex[example_hash]
                        pos_covered.add(example)
                    elif atom.name == 'inconsistent':
                        # rule_hash = str(atom.arguments[0])
                        # rule = hash_to_rule[rule_hash]
                        inconsistent = True
        t2 = time.time()
        # print('solving', t2-t1)
        return inconsistent, pos_covered

