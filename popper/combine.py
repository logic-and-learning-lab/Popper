import clingo
import time
import pickle
import itertools
from .util import format_rule, calc_prog_size, format_prog, flatten, reduce_prog, prog_is_recursive, rule_size, \
    rule_is_recursive, order_rule, prog_is_recursive, prog_has_invention

FIND_SUBSET_PROG3 = """
#defined recursive/0.
#show rule/1.
{rule(R)}:-size(R,_).
:~ rule(R),size(R,K). [K@1, (R,)]
:- not uses_new.
#show covered/1.
"""


def get_rule_hash(rule):
    head, body = rule
    head = (head.predicate, head.arguments)
    body = frozenset((literal.predicate, literal.arguments) for literal in body)
    return hash((head, body))


class Combiner:
    def __init__(self, settings, tester):
        self.prog_pos_covered = {}
        self.prog_neg_covered = {}
        self.settings = settings
        self.tester = tester

        self.build_example_encoding()
        self.rulehash_to_id = {}
        self.ruleid_to_rule = {}
        self.ruleid_to_size = {}

        self.inconsistent = set()
        self.big_encoding = set()
        self.programs_seen = 0

        if self.settings.nonoise and (self.settings.recursion_enabled or self.settings.pi_enabled):
            self.big_encoding.add(':- recursive, not base.')

        # add example atoms
        self.big_encoding.add(self.example_prog)

    def build_example_encoding(self):
        example_prog = []
        for i in self.settings.pos_index:
            example_prog.append(f'pos_example({i}).')
        if not self.settings.nonoise:
            for i in self.settings.neg_index:
                example_prog.append(f'neg_example({i}).')
        self.example_prog = '\n'.join(example_prog)

    def update_prog_index(self, prog, pos_covered, neg_covered):
        self.prog_pos_covered[prog] = pos_covered
        if not self.settings.nonoise:
            self.prog_neg_covered[prog] = neg_covered

        for rule in prog:
            rule_hash = get_rule_hash(rule)
            if rule_hash not in self.rulehash_to_id:
                k = len(self.rulehash_to_id) + 1
                self.rulehash_to_id[rule_hash] = k
                self.ruleid_to_rule[k] = rule
                self.ruleid_to_size[k] = rule_size(rule)

    def add_inconsistent(self, prog):
        should_add = True
        ids = []
        for rule in prog:
            k = get_rule_hash(rule)
            if k not in self.rulehash_to_id:
                should_add = False
                break
            ids.append(k)
        if not should_add:
            self.inconsistent.add(prog)
            return
        ids = [self.rulehash_to_id[k] for k in ids]
        con = ':-' + ','.join(f'rule({x})' for x in ids) + '.'
        self.big_encoding.add(con)

    def find_combination(self, encoding):
        # with self.settings.stats.duration('combine.build.string'):
        str_encoding = '\n'.join(encoding)

        # with open(f'sat/{self.programs_seen}', 'w') as f:
            # f.write(str_encoding)

        best_prog = []
        best_fp = False
        best_fn = False
        best_size = False

        while True:
            solver = clingo.Control([])
            # with self.settings.stats.duration('combine.add'):
            solver.add('base', [], str_encoding)
            # with self.settings.stats.duration('combine.ground'):
            solver.ground([('base', [])])

            model_found = False
            model_inconsistent = False

            with solver.solve(yield_=True) as handle:
                handle = iter(handle)
                while True:
                    # loop over the models
                    # with self.settings.stats.duration('combine.solve'):
                    m = next(handle, None)

                    if m is None:
                        break

                    model_found = True

                    pos_covered, neg_covered, rules = set(), set(), []
                    atoms = m.symbols(shown=True)
                    for a in atoms:
                        if a.name == 'covered':
                            if a.arguments[0].number >= 0:
                                pos_covered.add(a.arguments[0].number)
                            else:
                                neg_covered.add(a.arguments[0].number)
                        elif a.name == 'rule':
                            rules += [a.arguments[0].number]

                    fn = len(self.settings.pos_index) - len(pos_covered)
                    fp = len(neg_covered)
                    size = sum([self.ruleid_to_size[r] for r in rules])

                    # print(f'COST fn:{fn} fp:{fp} size:{size}')

                    # build a program from the model
                    model_prog = [self.ruleid_to_rule[k] for k in rules]

                    # is_recursive =
                    # has_invention = settings.pi_enabled and prog_has_invention(prog)

                    if not (self.settings.recursion_enabled and prog_is_recursive(model_prog)) and not (self.settings.pi_enabled and prog_has_invention(model_prog)):
                        # if there is noise or no recursion nor predicate invention then update the best solution
                        best_prog = rules
                        best_fp = fp
                        best_fn = fn
                        best_size = size
                        continue
                    
                    model_inconsistent = self.tester.is_inconsistent(model_prog)
                    # check whether recursive program is inconsistent
                    if not model_inconsistent:
                        best_prog = rules
                        best_fp = fp
                        best_fn = fn
                        best_size = size
                        continue

                    # if program is inconsistent then find the smallest inconsistent subprogram and prune it
                    # TODO: we could add the constraints for the intermediate solutions
                    # TODO: this reduce code is pants
                    # only check inconsistent programs when there is no noise, programs may be inconsistent for 
                    # other cost functions
                    smaller = self.tester.reduce_inconsistent(model_prog)
                    con = ':-' + ','.join(f'rule({self.rulehash_to_id[get_rule_hash(rule)]})' for rule in smaller) + '.'
                    str_encoding += con + '\n'
                    self.big_encoding.add(con)
                    # break to not consider no more models as we need to take into account the new constraint
                    break

            if not model_found or not model_inconsistent:
                break

        return best_prog, best_fn, best_fp, best_size

    def build_encoding(self, new_progs):
        self.programs_seen += 1
        # print("programs seen", self.programs_seen)
        this_encoding = set()
        this_encoding.add(FIND_SUBSET_PROG3)

        # ugly current cost function that defines a lexicographical ordering
        # we want to maximum positive coverage (tp), minimise negative coverage (tn), and then optimise program size

        if self.settings.best_prog_score:
            # if we have already found a solution, add constraints based on the best program seen
            tp, fn, tn, fp, size = self.settings.best_prog_score
            if fn == 0:
                # if the best solution covers all the positives then any new solution must also do so
                this_encoding.add(':- pos_example(E), not covered(E).')
                # if we know there is no noise no need to consider negative examples
                if not self.settings.nonoise:
                    if fp == 0:
                        # if the best solution does not cover any negative examples
                        # then add a hard constraint over neg coverage and a weak constraint over program size
                        this_encoding.add(':- neg_example(E), covered(E).')
                        this_encoding.add(':- #sum{K,R : rule(R), size(R,K)} >= ' + f'{size}.')
                    else:
                        # otherwise add a weak over neg coverage and a hard constraint over neg coverage bound
                        this_encoding.add(':~ neg_example(E), covered(E). [1@2, (E,)]')
                        this_encoding.add(':- #sum{1,E : neg_example(E), covered(E)} >= ' + f'{fp}.')
                else:
                    this_encoding.add(':- #sum{K,R : rule(R), size(R,K)} >= ' + f'{size}.')
            else:
                # if the best solution does not cover all the positives
                # then add weak constraints for pos and neg coverage and a hard constraint over pos coverage bound
                this_encoding.add(':~ pos_example(E), not covered(E). [1@3, (E,)]')
                this_encoding.add(':- #sum{1,E : pos_example(E), not covered(E)} >= ' + f'{fn}.')
                if not self.settings.nonoise:
                    this_encoding.add(':~ neg_example(E), covered(E). [1@2, (E,)]')
        else:
            # otherwise add weak constraints for the pos and neg coverage
            this_encoding.add(':~ pos_example(E), not covered(E). [1@3, (E,)]')
            if not self.settings.nonoise:
                this_encoding.add(':~ neg_example(E), covered(E). [1@2, (E,)]')

        for [new_prog, _, _] in new_progs:
            pos_examples_covered = self.prog_pos_covered[new_prog]
            if not self.settings.nonoise:
                neg_examples_covered = self.prog_neg_covered[new_prog]

            # add new rules for the new program
            prog_rules = set()
            for rule in new_prog:
                rule_hash = get_rule_hash(rule)
                rule_id = self.rulehash_to_id[rule_hash]
                # any better solution must use at least one new rule
                this_encoding.add(f'uses_new:- rule({rule_id}).')
                rule_size = self.ruleid_to_size[rule_id]
                prog_rules.add(rule_id)
                self.big_encoding.add(f'size({rule_id},{rule_size}).')
                if self.settings.nonoise and self.settings.recursion_enabled:
                    if rule_is_recursive(rule):
                        self.big_encoding.add(f'recursive:- rule({rule_id}).')
                    else:
                        self.big_encoding.add(f'base:- rule({rule_id}).')

            prog_rules = ','.join(f'rule({i})' for i in prog_rules)
            for ex in pos_examples_covered:
                self.big_encoding.add(f'covered({ex}):- {prog_rules}.')
            if not self.settings.nonoise:
                for ex in neg_examples_covered:
                    self.big_encoding.add(f'covered({ex}):- {prog_rules}.')

        return self.big_encoding.union(this_encoding)

    def select_solution(self, saved_progs):
        encoding = self.build_encoding(saved_progs)
        model_rules, fp, fn, size = self.find_combination(encoding)
        return [self.ruleid_to_rule[k] for k in model_rules], fp, fn, size

    def update_best_prog(self, saved_progs):
        # add the new prog to the set of seen programs
        for [prog, pos_covered, neg_covered] in saved_progs:
            self.update_prog_index(prog, pos_covered, neg_covered)

        # try to find a better solution
        new_solution, fn, fp, size = self.select_solution(saved_progs)

        # if the solution is empty then return false
        if len(new_solution) == 0:
            return None

        # there are weird cases when a program can contain redundant rules so logically reduce it
        new_solution = reduce_prog(new_solution)
        tp = self.tester.num_pos - fn
        tn = self.tester.num_neg - fp
        size = calc_prog_size(new_solution)
        return new_solution, (tp, fn, tn, fp, size)