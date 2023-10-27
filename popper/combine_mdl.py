import clingo
import time
import pickle
import itertools
import collections
from . util import format_rule, calc_prog_size, format_prog, flatten, reduce_prog, prog_is_recursive, prog_has_invention, \
    rule_size, rule_is_recursive, order_rule
from clingo import Function, Number, Tuple_

FIND_SUBSET_PROG3 = """
#show rule/1.
{rule(R)}:-size(R,_).
:~ rule(R),size(R,K). [K@1, (R,)]
:- not uses_new.
:~ pos_example(E), not covered(E). [1@1, (E,)]
:~ neg_example(E), covered(E). [1@1, (E,)]
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

        self.big_encoding = set()
        self.programs_seen = 0
        self.saved_progs = collections.defaultdict(set)
        self.rule_to_prog = collections.defaultdict(list)
        self.debug = 0
        self.best_cost = None

        # add example atoms
        self.big_encoding.add(self.example_prog)

        # self.best_cost = None

    def update_deleted_progs(self, old_score, new_score):
        with self.settings.stats.duration('delete'):
            to_delete_progs = set().union(*[self.saved_progs[k] for k in range(new_score, old_score+1)])
            if to_delete_progs:
                for prog in to_delete_progs:
                    del self.prog_pos_covered[frozenset([self.ruleid_to_rule[r] for r in prog])]
                    self.settings.stats.deleted_count += 1
                to_delete_rules = set().union(*to_delete_progs)

                # if a rule is used only within to-delete-programs, can simply delete it from the combine stage and from saved_progs
                to_delete = set()
                for rule in to_delete_rules:
                    if all([prog in to_delete_progs for prog in self.rule_to_prog[rule]]):
                        to_delete.add(rule)

                # now delete it from big encoding
                if to_delete:
                    to_delete_lines = []
                    for line in self.big_encoding:
                        if line.startswith('size'):
                            if int(line.split('(')[1].split(',')[0]) in to_delete:
                                to_delete_lines.append(line)
                        elif line.startswith('covered'):
                            if int(line.split('rule(')[1].split(')')[0]) in to_delete:
                                to_delete_lines.append(line)
                    for k in to_delete_lines:
                        self.big_encoding.remove(k)

                # now delete from saved_progs
                other = set()
                for k in range(new_score, old_score + 1):
                    del_k = []
                    for ids in self.saved_progs[k]:
                        if set(ids).issubset(to_delete):
                            del_k.append(ids)
                        elif set(ids).issubset(to_delete_rules):
                            # otherwise add a constraint to not use the to-delete-program
                            other.add(ids)
                            self.big_encoding.add(":-" + ','.join([f"rule({i})" for i in ids]) + ". \n")
                    for ids in del_k:
                        self.saved_progs[k].remove(ids)

    def build_example_encoding(self):
        example_prog = []
        for i in self.settings.pos_index:
            example_prog.append(f'pos_example({i}).')
        for i in self.settings.neg_index:
            example_prog.append(f'neg_example({i}).')
        self.example_prog = '\n'.join(example_prog)

    def update_prog_index(self, prog, pos_covered, neg_covered):
        self.prog_pos_covered[prog] = pos_covered
        self.prog_neg_covered[prog] = neg_covered

        ids = []
        for rule in prog:
            rule_hash = get_rule_hash(rule)
            if rule_hash not in self.rulehash_to_id:
                k = len(self.rulehash_to_id) +1
                self.rulehash_to_id[rule_hash] = k
                self.ruleid_to_rule[k] = rule
                self.ruleid_to_size[k] = rule_size(rule)
            else:
                k = self.rulehash_to_id[rule_hash]
            ids.append(k)
        for rule in prog:
            self.rule_to_prog[self.rulehash_to_id[get_rule_hash(rule)]].append(tuple(ids))
        self.saved_progs[len(neg_covered)+calc_prog_size(prog)].add(tuple(ids))


    def find_combination(self, encoding):
        with self.settings.stats.duration('combine.build.string'):
            str_encoding = '\n'.join(encoding)

        # with open('sat/mdl.pl','w') as f:
        #     f.write(str_encoding)

        best_prog = []
        best_cost = None

        # solver = clingo.Control([f'-t{self.settings.threads}'])
        solver = clingo.Control([])
        # with self.settings.stats.duration('combine.add'):
        solver.add('base', [], str_encoding)
        # with self.settings.stats.duration('combine.ground'):
        solver.ground([('base', [])])

        with solver.solve(yield_ = True) as handle:
            handle = iter(handle)
            while True:
                # loop over the models
                with self.settings.stats.duration('combine.solve'):
                    m = next(handle, None)

                if m is None:
                    break

                # horrible code
                # the arity of the cost result determines the cost function
                # print('m.cost', m.cost)

                dl = m.cost[0]

                # print(len(m.cost))
                # print(f'COST description length (fp + fn + size): {dl}')

                # build a program from the model
                atoms = m.symbols(shown = True)
                rules = [atom.arguments[0].number for atom in atoms if atom.name == 'rule']

                best_prog = rules
                best_cost = dl

        return best_prog, best_cost

    def build_encoding(self, new_progs):
        self.programs_seen += 1
        #print("programs seen", self.programs_seen)
        this_encoding = set()
        this_encoding.add(FIND_SUBSET_PROG3)

        for [new_prog, _, _] in new_progs:
            pos_examples_covered = self.prog_pos_covered[new_prog]
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

            prog_rules = ','.join(f'rule({i})' for i in prog_rules)
            for ex in pos_examples_covered:
                self.big_encoding.add(f'covered({ex}):- {prog_rules}.')
            for ex in neg_examples_covered:
                self.big_encoding.add(f'covered({ex}):- {prog_rules}.')

        return self.big_encoding.union(this_encoding)

    def select_solution(self, saved_progs):
        encoding = self.build_encoding(saved_progs)
        model_rules, best_cost = self.find_combination(encoding)
        return [self.ruleid_to_rule[k] for k in model_rules], best_cost

    def update_best_prog(self, saved_progs):
        # add the new prog to the set of seen programs
        for [prog, pos_covered, neg_covered] in saved_progs:
            self.update_prog_index(prog, pos_covered, neg_covered)

        # try to find a better solution
        new_solution, best_cost = self.select_solution(saved_progs)

        # assert(False)

        # if the solution is empty then return false
        if len(new_solution) == 0:
            return None

        if self.best_cost == None:
            self.best_cost = best_cost
        else:
            if best_cost >= self.best_cost:
                return None




        self.best_cost = best_cost

        # there are weird cases when a program can contain redundant rules so logically reduce it
        new_solution = reduce_prog(new_solution)
        pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        # print('pos_covered',pos_covered)
        # print('neg_covered',neg_covered)
        tp = len(pos_covered)
        fp = len(neg_covered)
        tn = self.tester.num_neg - fp
        fn = self.tester.num_pos - tp
        size = calc_prog_size(new_solution)

        return new_solution, (tp, fn, tn, fp, size)