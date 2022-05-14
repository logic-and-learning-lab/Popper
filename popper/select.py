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

        # prog = [FIND_SUBSET_PROG]

        # self.example_to_hash = {}
        # for x in settings.pos:
        #     k = f'"{hash(x)}"'
        #     self.example_to_hash[x] = k
        #     prog.append(f'example({k}).')
        # prog = '\n'.join(prog)

        # self.tmp = prog
        # solver = clingo.Control()
        # solver.add('base', [], prog)
        # # solver.ground([('base', [])])
        # # solver.solve([('base', [])])

        # self.solver = solver
        self.rule_coverage = {}
        self.rule_size = {}
        self.index_to_rule = {}
        self.added_rules = set()
        self.loop_count = 0

    def update_best_solution(self, new_rules):

        solver = clingo.Control()

        prog = [FIND_SUBSET_PROG]

        self.example_to_hash = {}
        for x in self.settings.pos:
            k = f'"{hash(x)}"'
            self.example_to_hash[x] = k
            prog.append(f'example({k}).')
        # prog = '\n'.join(prog)

        # self.tmp = prog
        # solver = clingo.Control()
        # solver.add('base', [], prog)
        # solver.ground([('base', [])])
        # solver.solve([('base', [])])

        # self.solver = solver

        # print('ASDA!!!!')
        # print(len(new_rules))
        # prog = []
        for rule in new_rules:
            # print('LOOP RULE!!', format_rule(rule))
            k = f'"{hash(rule)}"'
            assert(k not in self.index_to_rule)
            self.index_to_rule[str(hash(rule))] = rule
            size = self.rule_size[rule]
            prog.append(f'size({k},{size}).')
            # print('rule_coverage', self.rule_coverage[rule])
            for ex in self.rule_coverage[rule]:
                ex = self.example_to_hash[ex]
                prog.append(f'covers({k},{ex}).')

        prog = '\n'.join(prog)

        print(f'CANDIDATE RULES: {len(new_rules)}')

        with open('sat-prob.pl', 'w') as f:
            f.write(prog)

        prog_key = f'prog_{self.loop_count}'


        solver.add('base', [], prog)
        # self.solver.add(prog_key, [], prog)
        solver.ground([('base', [])])
        # self.solver.ground([(prog_key, [])])

        out = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                # print('M!!!')
                xs = m.symbols(shown = True)
                # print('xs',xs)
                out = [atom.arguments[0].string for atom in xs]

        tmp = [self.index_to_rule[k] for k in out]
        print('*'*20)
        print('SOLUTION')
        for x in tmp:
            print(format_rule(x))
        print('*'*20)
