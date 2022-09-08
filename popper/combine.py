import clingo
import time
import itertools
from . util import format_rule, prog_size, format_prog, flatten, reduce_prog, prog_is_recursive, rule_size, rule_is_recursive, order_rule

# for when we have a complete solution
# same as above but no weak constraint over examples covered
FIND_SUBSET_PROG2 = """
#defined recursive/0.
#show rule/1.
{rule(R)}:-size(R,_).
:- example(E), not covered(E).
:~ rule(R),size(R,K). [K@1, (R,)]
:- recursive, not base.
:- not uses_new.
"""

# for when we do not yet have a complete solution
FIND_SUBSET_PROG1 = """
#defined recursive/0.
#show rule/1.
{rule(R)}:-size(R,_).
:~ example(E), not covered(E). [1@2, (E,)]
:~ rule(R),size(R,K). [K@1, (R,)]
:- recursive, not base.
:- not uses_new.
"""

def get_rule_hash(rule):
    head, body = rule
    head = (head.predicate, head.arguments)
    body = frozenset((literal.predicate, literal.arguments) for literal in body)
    return hash((head, body))

class Combiner:
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester

        self.example_to_id = {}
        self.build_example_encoding()

        self.prog_coverage = {}

        self.solution_found = False
        self.best_prog = None
        self.num_covered = 0
        self.max_size = None

        self.constraints = set()
        self.rulehash_to_id = {}
        self.ruleid_to_rule = {}
        self.ruleid_to_size = {}

        self.inconsistent = set()
        self.debug_count = 0

    def build_example_encoding(self):
        example_prog = []
        for i, x in enumerate(self.settings.pos):
            self.example_to_id[x] = i
            example_prog.append(f'example({i}).')
        self.example_prog = '\n'.join(example_prog)

    def update_prog_index(self, prog, pos_covered):
        self.prog_coverage[prog] = pos_covered

        for rule in prog:
            rule_hash = get_rule_hash(rule)
            if rule_hash not in self.rulehash_to_id:
                k = len(self.rulehash_to_id) +1
                self.rulehash_to_id[rule_hash] = k
                self.ruleid_to_rule[k] = rule
                self.ruleid_to_size[k] = rule_size(rule)

    def add_inconsistent(self, prog):
        self.inconsistent.add(prog)

    def find_combination(self, encoding):
        str_encoding = '\n'.join(encoding)
        # with open(f'sat/{self.debug_count}') as f:
            # f.write(str_encoding)

        best_prog = []
        best_fn = False

        while True:
            solver = clingo.Control([])
            solver.add('base', [], str_encoding)
            solver.ground([('base', [])])

            model_found = False
            model_inconsistent = False

            with solver.solve(yield_=True) as handle:
                for m in handle:
                    model_found = True
                    model_incomplete = False

                    # cost has two elements when we have not yet found a complete model
                    if len(m.cost) == 2:
                        fn = m.cost[0]
                    # once we have a complete model, the cost is only the size of the solution
                    else:
                        fn = 0

                    atoms = m.symbols(shown = True)
                    rules = [atom.arguments[0].number for atom in atoms]

                    if not self.settings.recursion_enabled and not self.settings.pi_enabled:
                        best_prog = rules
                        best_fn = fn
                        continue

                    # check whether recursive program is inconsistent
                    model_prog = [self.ruleid_to_rule[k] for k in rules]
                    model_inconsistent = self.tester.is_inconsistent(model_prog)
                    if not model_inconsistent:
                        best_prog = rules
                        best_fn = fn
                        continue

                    with self.settings.stats.duration('subcheck'):
                        # if program is inconsistent, then find the smallest inconsistent subprogram and prune it
                        # TODO: we could add the constraints for the intermediate solutions
                        smaller = self.tester.reduce_inconsistent(model_prog)
                        con = ':-' + ','.join(f'rule({self.rulehash_to_id[get_rule_hash(rule)]})' for rule in smaller) + '.'
                        str_encoding += con + '\n'
                        self.constraints.add(con)
                    # break to not consider no more models as we need to take into account the new constraint
                    break

            if not model_found or not model_inconsistent:
                return best_prog, best_fn
        return best_prog, best_fn

    def select_solution(self, new_prog):
        encoding = set()

        if self.solution_found:
            # this encoding has a hard constraint to ensure the program is complete
            encoding.add(FIND_SUBSET_PROG2)
            # add size constraint to only find programs smaller than the best one so far
            encoding.add(':- #sum{K,R : rule(R), size(R,K)} >= ' + f'{self.max_size}.')
        else:
            # this encoding has a soft constraint to cover as many positive examples as possible
            encoding.add(FIND_SUBSET_PROG1)
            # add a constraint to ensure more examples are covered than previously
            encoding.add(':- #sum{1,E : covered(E)} <= ' + f'{self.num_covered}.')

        # any better solution must use at least one new rule
        for rule in new_prog:
            rule_hash = get_rule_hash(rule)
            rule_id = self.rulehash_to_id[rule_hash]
            encoding.add(f'uses_new:- rule({rule_id}).')

        for prog, examples_covered in self.prog_coverage.items():
            prog_rules = set()
            for rule in prog:
                rule_hash = get_rule_hash(rule)
                rule_id = self.rulehash_to_id[rule_hash]
                rule_size = self.ruleid_to_size[rule_id]
                prog_rules.add(rule_id)
                encoding.add(f'size({rule_id},{rule_size}).')
                if rule_is_recursive(rule):
                    encoding.add(f'recursive:- rule({rule_id}).')
                else:
                    encoding.add(f'base:- rule({rule_id}).')

            prog_rules = ','.join(f'rule({i})' for i in prog_rules)
            for ex in self.prog_coverage[prog]:
                i = self.example_to_id[ex]
                encoding.add(f'covered({i}):- {prog_rules}.')

        # add example atoms
        encoding.add(self.example_prog)

        # add constraints to prune inconsistent recursive programs
        encoding.update(self.constraints)

        if len(self.inconsistent) > 0:
            # TODO: improve as there is no need to build the constraints each time
            # with self.settings.stats.duration('inconsistent thingy'):
            for prog in self.inconsistent:
                if all(get_rule_hash(rule) in self.rulehash_to_id for rule in prog):
                    ids = [self.rulehash_to_id[get_rule_hash(rule)] for rule in prog]
                    con = ':-' + ','.join(f'rule({x})' for x in ids) + '.'
                    encoding.add(con)

        model_rules, fn = self.find_combination(encoding)

        return [self.ruleid_to_rule[k] for k in model_rules], fn

    def update_best_prog(self, prog, pos_covered):
        self.update_prog_index(prog, pos_covered)
        new_solution, fn = self.select_solution(prog)

        # if there is no new better solution, do nothing
        if len(new_solution) == 0:
            return False

        new_solution = reduce_prog(new_solution)
        self.settings.solution = new_solution
        size = prog_size(new_solution)

        tn = self.tester.num_neg
        fp = 0

        if fn > 0:
            tp = self.tester.num_pos - fn
            self.num_covered = tp
            self.settings.print_incomplete_solution(new_solution, tp, fn, size)
            self.settings.best_prog_score = (tp, fn, tn, fp, size)
            return False

        self.settings.print_incomplete_solution(new_solution, self.tester.num_pos, 0, size)
        self.solution_found = True
        self.max_size = size
        self.best_prog = new_solution
        self.settings.best_prog_score = (self.tester.num_pos, 0, tn, fp, size)
        return True