import clingo
from . util import format_rule, prog_size, format_prog, flatten, reduce_prog

FIND_SUBSET_PROG = """
#show rule/1.
#show incomplete/0.
{rule(R)}:-size(R,_).
covered(E):- covers(R,E), rule(R).
different(R1,R2):- R1 != R2, size(R1,_), size(R2,_), covers(R1,E), not covers(R2,E).
same(R1,R2):- R1 < R2, size(R1,_), size(R2,_), not different(R1,R2), not different(R2,R1).
dominates(R1,R2):- different(R1,R2), not different(R2,R1).
incomplete:- example(E), not covered(E).
:- same(R1,R2), rule(R1), rule(R2).
:- same(R1,R2), size(R1,K1), size(R2,K2), K1 >= K2, rule(R1).
:- dominates(R1,R2), size(R1,K1), size(R2,K2), K1 <= K2, rule(R2).
:~ example(E), not covered(E). [1@2, (E,)]
:~ rule(R),size(R,K). [K@1]
"""

class Selector:
    def __init__(self, settings, tester):
        self.settings = settings
        self.prog_coverage = {}

        self.solution_found = False
        self.best_prog = None

        self.num_covered = 0

        self.index_to_prog = {}
        self.max_size = None

        self.prog_encoding = ''
        self.example_to_hash = {}
        self.prog_count = 0
        self.build_example_encoding()
        self.tester = tester
        self.constraints = []

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

    def select_solution(self):
        encoding = [FIND_SUBSET_PROG, '\n'.join(self.constraints), self.example_prog, self.prog_encoding]

        if self.solution_found:
            # add size constraint
            encoding.append(':- #sum{K,R : rule(R), size(R,K)} >= ' + f'{self.max_size}.')
            # add hard constraint on example coverage
            encoding.append(':- incomplete.')
        else:
            encoding.append(':- #sum{1,E : covered(E)} <= ' + f'{self.num_covered}.')

        encoding = '\n'.join(encoding)

        # with open('sat-problem.pl', 'w') as f:
            # f.write(encoding)
        # print(encoding)
        solver = clingo.Control()
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        out = []
        incomplete = None

        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms = m.symbols(shown = True)
                rules = set()
                incomplete = False
                for atom in atoms:
                    if atom.name == 'rule':
                        rules.add(atom.arguments[0].number)
                    elif atom.name == 'incomplete':
                        incomplete = True
                out = rules
                # print('model', incomplete)
                if incomplete:
                    continue
                model_prog = flatten([self.index_to_prog[k] for k in rules])
                _, inconsistent = self.tester.test_prog(model_prog)
                if inconsistent:
                    con = ':-' + ','.join(f'rule({i})' for i in out) + '.'
                    self.constraints.append(con)
                    return self.select_solution()
        return flatten([self.index_to_prog[k] for k in out]), incomplete

    def update_best_prog(self, prog, pos_covered):
        self.prog_coverage[prog] = pos_covered
        self.build_prog_encoding(prog)
        new_solution, incomplete = self.select_solution()

        # if there is no new better solution, do nothing
        if len(new_solution) == 0:
            return False

        new_solution = reduce_prog(new_solution)
        self.settings.solution = new_solution
        size = 0
        for rule in new_solution:
            head, body = rule
            size += len(body) + 1

        tn = self.tester.num_neg
        fp = 0

        if incomplete:
            covered, _ = self.tester.test_prog(new_solution)
            tp = len(covered)
            fn = self.tester.num_pos - tp
            if fn > 0:
                self.num_covered = tp
                # print(f'NEW SOLUITON IS INCOMPLETE WITH TP:{tp} and FN{fn}:')
                self.settings.print_incomplete_solution(new_solution, tp, fn, size)
                self.settings.best_prog_score = (tp, fn, tn, fp, size)
                return False

        self.settings.print_incomplete_solution(new_solution, self.tester.num_pos, 0, size)
        self.solution_found = True
        self.max_size = size
        self.best_prog = new_solution
        self.settings.best_prog_score = (self.tester.num_pos, 0, tn, fp, size)
        return True

# % Precision:0.71, Recall:1.00, TP:5, FN:0, TN:3, FP:2