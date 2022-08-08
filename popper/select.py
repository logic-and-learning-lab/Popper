import clingo
import time
from z3 import *
from . util import format_rule, prog_size, format_prog, flatten, reduce_prog

# for when we do not yet have a complete solution
FIND_SUBSET_PROG1 = """
#show rule/1.
#show incomplete/0.
{rule(R)}:-size(R,_).
covered(E):- covers(R,E), rule(R).
incomplete:- example(E), not covered(E).
:~ example(E), not covered(E). [1@2, (E,)]
:~ rule(R),size(R,K). [K@1, (R,)]
"""

# for when we have a complete solution
# same as above but no weak constraint over examples covered
FIND_SUBSET_PROG2 = """
#show rule/1.
{rule(R)}:-size(R,_).
covered(E):- covers(R,E), rule(R).
:- example(E), not covered(E).
:~ rule(R),size(R,K). [K@1, (R,)]
"""

class Selector:
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.build_example_encoding()

        self.prog_coverage = {}
        self.prog_sizes = {}

        self.solution_found = False
        self.best_prog = None
        self.num_covered = 0
        self.max_size = None

        self.constraints = []

        self.success_sets = {}
        self.shit_progs = set()

    def build_example_encoding(self):
        self.example_to_hash = {}
        example_prog = []
        for i, x in enumerate(self.settings.pos):
            self.example_to_hash[x] = i
            example_prog.append(f'example({i}).')
        self.example_prog = '\n'.join(example_prog)

    # @profile
    def update_shit_progs(self, prog, coverage, size):
        if coverage in self.success_sets:
            is_shit = False
            for prog2, size2 in self.success_sets[coverage]:
                if size2 <= size:
                    self.shit_progs.add(prog)
                    is_shit = True
                    break
                else:
                    self.shit_progs.add(prog2)
            if not is_shit:
                self.success_sets[coverage].add((prog, size))
        else:
            self.success_sets[coverage] = set([(prog, size)])

    # @profile
    def select_solution(self):
        encoding = []

        if self.solution_found:
            encoding.append(FIND_SUBSET_PROG2)
            # add size constraint - although I am unsure whether this constraint helps in practice
            encoding.append(':- #sum{K,R : rule(R), size(R,K)} >= ' + f'{self.max_size}.')
        else:
            encoding.append(FIND_SUBSET_PROG1)
            # encoding.append(':- #sum{1,E : covered(E)} <= ' + f'{self.num_covered}.')

        # add constraints to prune inconsistent recursive programs
        encoding.extend(self.constraints)

        self.index_to_prog = {}
        prog_count = 0
        for prog, examples_covered in self.prog_coverage.items():
            if prog in self.shit_progs:
                continue
            k = prog_count
            self.index_to_prog[k] = prog
            size = self.prog_sizes[prog]
            encoding.append(f'size({k},{size}).')
            for ex in self.prog_coverage[prog]:
                i = self.example_to_hash[ex]
                encoding.append(f'covers({k},{i}).')
            prog_count+=1

        encoding.append(self.example_prog)
        encoding = '\n'.join(encoding)

        solver = clingo.Control(["--opt-strategy=usc"])
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
                if not self.settings.recursion_enabled and not self.settings.pi_enabled:
                    continue

                model_prog = flatten([self.index_to_prog[k] for k in rules])
                _, inconsistent = self.tester.test_prog(model_prog)
                if inconsistent:
                    con = ':-' + ','.join(f'rule({i})' for i in out) + '.'
                    self.constraints.append(con)
                    return self.select_solution(settings)
        return flatten([self.index_to_prog[k] for k in out]), incomplete

    def update_best_prog(self, prog, pos_covered):
        size = prog_size(prog)
        self.prog_coverage[prog] = pos_covered
        self.prog_sizes[prog] = size
        self.update_shit_progs(prog, pos_covered, size)

        # self.build_prog_encoding(prog)
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