import clingo
from . util import format_rule

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
        self.rule_coverage = {}
        self.rule_size = {}
        self.index_to_rule = {}
        # self.added_rules = set()
        # self.loop_count = 0
        self.max_size = None

        self.example_to_hash = {}
        example_prog = []
        for x in self.settings.pos:
            k = f'"{hash(x)}"'
            self.example_to_hash[x] = k
            example_prog.append(f'example({k}).')
        self.EXAMPLE_PROG = '\n'.join(example_prog)

    def update_best_solution(self, new_rules):
        prog = [FIND_SUBSET_PROG, self.EXAMPLE_PROG]

        for rule in new_rules:
            k = f'"{hash(rule)}"'
            assert(k not in self.index_to_rule)
            self.index_to_rule[str(hash(rule))] = rule
            size = self.rule_size[rule]
            prog.append(f'size({k},{size}).')
            for ex in self.rule_coverage[rule]:
                ex = self.example_to_hash[ex]
                prog.append(f'covers({k},{ex}).')

        if self.max_size != None:
            prog.append(f':- size(N), N >= {self.max_size}.')

        prog = '\n'.join(prog)

        # with open('sat-prob.pl', 'w') as f:
            # f.write(prog)

        solver = clingo.Control()
        solver.add('base', [], prog)
        solver.ground([('base', [])])

        out = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                xs = m.symbols(shown = True)
                out = [atom.arguments[0].string for atom in xs]

        new_solution = [self.index_to_rule[k] for k in out]
        if len(new_solution) > 0:
            print('*'*20)
            size = 0
            for rule in new_solution:
                head, body = rule
                size += len(body) + 1
            print(f'NEW SOLUTION OF SIZE: {size}')
            for rule in new_solution:
                print('S',format_rule(rule))
            print('*'*20)
            self.max_size = size
