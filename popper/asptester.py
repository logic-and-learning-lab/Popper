from . util import format_rule
import clingo
import clingo.script
import time

TEST_PROG = """
#defined pos_covered/1.
#defined inconsistent/0.
#defined pos/2.
#defined neg/2.
#defined holds/1.
#show pos_covered/1.
#show inconsistent/0.
pos_covered(E):- pos(E,Atom), holds(Atom).
inconsistent:- neg(E,Atom), holds(Atom).
"""

class Tester:

    def __init__(self, settings):
        self.settings = settings
        self.parse_exs()
        self.parse_bk()

    def parse_exs(self):
        pos = set()
        neg = set()

        solver = clingo.Control()
        with open(self.settings.ex_file, 'r') as f:
            solver.add('base', [], f.read())
        solver.ground([('base', [])])

        with solver.solve(yield_=True) as handle:
            for m in handle:
                for atom in m.symbols(shown = True):
                    arg = str(atom.arguments[0])
                    if atom.name == 'pos':
                        pos.add(arg)
                    elif atom.name == 'neg':
                        neg.add(arg)
        self.pos_index = {i+1:v for i, v in enumerate(pos)}
        self.neg_index = {-(i+1):v for i, v in enumerate(neg)}

        self.num_pos = len(self.pos_index)
        self.num_neg = len(self.neg_index)

        self.settings.pos = frozenset(self.pos_index.values())
        self.settings.neg = frozenset(self.neg_index.values())

        example_encoding = []
        for i, x in self.pos_index.items():
            example_encoding.append(f'pos({i},{x}).')
        for i, x in self.neg_index.items():
            example_encoding.append(f'neg({i},{x}).')
        self.example_encoding = '\n'.join(example_encoding)

    def parse_bk(self):
        with open(self.settings.bk_file, 'r') as f:
            self.bk = f.read()

    def test_prog(self, prog):
        prog_encoding = set()
        for rule in prog:
            head, body = rule
            p = head.predicate
            prog_encoding.add(format_rule(rule))
            if len(head.arguments) == 1:
                prog_encoding.add(f'holds({p}(A)):-{p}(A).')
            elif len(head.arguments) == 2:
                prog_encoding.add(f'holds({p}(A,B)):-{p}(A,B).')
            elif len(head.arguments) == 3:
                prog_encoding.add(f'holds({p}(A,B,C)):-{p}(A,B,C).')
            elif len(head.arguments) == 3:
                prog_encoding.add(f'holds({p}(A,B,C,D)):-{p}(A,B,C,D).')

        prog_encoding = '\n'.join(prog_encoding)
        prog = [TEST_PROG,self.bk,self.example_encoding,prog_encoding]
        prog = '\n'.join(prog)

        with self.settings.stats.duration('tmp'):
            solver = clingo.Control()
            solver.add('base', [], prog)
            solver.ground([('base', [])])

            inconsistent = False
            pos_covered = set()
            with solver.solve(yield_=True) as handle:
                for m in handle:
                    atoms = m.symbols(shown = True)
                    # print(atoms)
                    for atom in atoms:
                        if atom.name == 'pos_covered':
                            i = atom.arguments[0].number
                            pos_covered.add(self.pos_index[i])
                        elif atom.name == 'inconsistent':
                            inconsistent = True
        # print(pos_covered)
        return frozenset(pos_covered), inconsistent