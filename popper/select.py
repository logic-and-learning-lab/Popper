import clingo
from . util import format_rule, prog_size, format_prog

FIND_SUBSET_PROG = """
#show rule/1.
{rule(R)}:-size(R,_).
size(N):- #sum{K,R : rule(R), size(R,K)} == N.
covered(E):- covers(R,E), rule(R).
:- example(E), not covered(E).
#minimize{X : size(X)}.
different(R1,R2):- R1 != R2, size(R1,_), size(R2,_), covers(R1,E), not covers(R2,E).
same(R1,R2):- R1 < R2, size(R1,_), size(R2,_), not different(R1,R2), not different(R2,R1).
dominates(R1,R2):- different(R1,R2), not different(R2,R1).
:- same(R1,R2), rule(R1), rule(R2).
:- same(R1,R2), size(R1,K1), size(R2,K2), K1 >= K2, rule(R1).
:- dominates(R1,R2), size(R1,K1), size(R2,K2), K1 <= K2, rule(R2).
"""

class Selector:
    def __init__(self, settings):
        self.settings = settings
        self.prog_coverage = {}
        self.index_to_prog = {}
        self.max_size = None
        self.prog_encoding = ''
        self.example_to_hash = {}
        self.prog_count = 0
        self.build_example_encoding()

    def build_example_encoding(self):
        example_prog = []
        for i, x in enumerate(self.settings.pos):
            self.example_to_hash[x] = i
            example_prog.append(f'example({i}).')
        self.example_prog = '\n'.join(example_prog)

    def build_prog_encoding(self, prog):
        self.prog_count += 1
        self.index_to_prog[self.prog_count] = prog
        size = prog_size(prog)
        prog_builder = []
        prog_builder.append(f'size({self.prog_count},{size}).')
        for ex in self.prog_coverage[prog]:
            i = self.example_to_hash[ex]
            prog_builder.append(f'covers({self.prog_count},{i}).')
        self.prog_encoding += '\n'.join(prog_builder) + '\n'

    def update_best_prog(self, prog):
        self.build_prog_encoding(prog)
        encoding = [FIND_SUBSET_PROG, self.example_prog, self.prog_encoding]
        if self.max_size != None:
            encoding.append(f':- size(N), N >= {self.max_size}.')
        encoding = '\n'.join(encoding)

        solver = clingo.Control()
        solver.add('base', [], encoding)
        solver.ground([('base', [])])

        out = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms = m.symbols(shown = True)
                out = [atom.arguments[0].number for atom in atoms]

        new_solution = [self.index_to_prog[k] for k in out]
        if len(new_solution) > 0:
            print('*'*20)
            size = 0
            for sub_prog in new_solution:
                for rule in sub_prog:
                    head, body = rule
                    size += len(body) + 1
                    print(format_rule(rule))
            print(f'NEW SOLUTION OF SIZE: {size}')
            print('*'*20)
            self.max_size = size
            self.best_program = new_solution
