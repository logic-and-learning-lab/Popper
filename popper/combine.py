from z3 import *
import clingo
import time
import itertools
from collections import defaultdict
from . util import format_rule, prog_size, format_prog, flatten, reduce_prog, prog_is_recursive, rule_size, rule_is_recursive, order_rule

# for when we have a complete solution
# same as above but no weak constraint over examples covered
FIND_SUBSET_PROG2 = """
#defined recursive/0.
#defined base/0.
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

        # self.example_to_id = {}
        # # self.build_example_encoding()

        # self.prog_coverage = {}

        self.solution_found = False
        # self.best_prog = None
        # self.num_covered = 0
        # self.max_size = None

        # self.constraints = set()
        # self.rulehash_to_id = {}
        # self.ruleid_to_rule = {}
        # self.ruleid_to_size = {}

        # self.inconsistent = set()
        # self.debug_count = 0

        # encoding = []
        # encoding.append(FIND_SUBSET_PROG1)

        # if self.settings.recursion_enabled or self.settings.pi_enabled:
        #     encoding.add(':- recursive, not base.')
        #     # encoding.add(':- recursive, #count{R : rule(R)} > ' + f'{self.settings.max_rules}.')

        # for i, x in enumerate(self.settings.pos):
        #     self.example_to_id[x] = i
        #     encoding.append(f'example({i}).')

        # encoding = '\n'.join(encoding)


        # self.sol = Solver()


        # self.solver = clingo.Control([])
        # self.solver.add('base', [], encoding)
        # self.solver.ground([('base', [])])
        self.seen_rules = {}
        self.rule_count = 0
        self.covered_by = {ex : set() for ex in settings.pos}

    # def update_prog_index(self, prog, pos_covered):
    #     self.prog_coverage[prog] = pos_covered

    #     # added = False
    #     for rule in prog:
    #         rule_hash = get_rule_hash(rule)
    #         if rule_hash not in self.rulehash_to_id:
    #             k = len(self.rulehash_to_id) +1
    #             self.rulehash_to_id[rule_hash] = k
    #             self.ruleid_to_rule[k] = rule
    #             self.ruleid_to_size[k] = rule_size(rule)
    #             # added = True
    #     # if not added:
    #         # print('WTF!!?')
    #         # exit()

    # def add_inconsistent(self, prog):
    #     self.inconsistent.add(prog)

    # # @profile
    # def find_combination(self, encoding):
    #     str_encoding = '\n'.join(encoding)
    #     self.debug_count += 1
    #     # with open(f'sat/{self.debug_count}', 'w') as f:
    #         # f.write(str_encoding)

    #     best_prog = []
    #     best_fn = False

    #     while True:


    #         model_found = False
    #         model_inconsistent = False


    #         with solver.solve(yield_ = True) as handle:
    #             handle = iter(handle)

    #             while True:

    #                 # GENERATE A PROGRAM
    #                 with self.settings.stats.duration('combine_solve'):
    #                     # get the next model from the solver
    #                     m = next(handle, None)
    #                     if m is None:
    #                         break

    #                     # print('COMBINE TIME', self.debug_count, t2-t1)
    #                     model_found = True
    #                     model_incomplete = False

    #                     # cost has two elements when we have not yet found a complete model
    #                     if len(m.cost) == 2:
    #                         fn = m.cost[0]
    #                     # once we have a complete model, the cost is only the size of the solution
    #                     else:
    #                         fn = 0

    #                     atoms = m.symbols(shown = True)
    #                     rules = [atom.arguments[0].number for atom in atoms]

    #                     if not self.settings.recursion_enabled and not self.settings.pi_enabled:
    #                         best_prog = rules
    #                         best_fn = fn
    #                         continue

    #                     # check whether recursive program is inconsistent
    #                     model_prog = [self.ruleid_to_rule[k] for k in rules]
    #                     model_inconsistent = self.tester.is_inconsistent(model_prog)
    #                     if not model_inconsistent:
    #                         best_prog = rules
    #                         best_fn = fn
    #                         if fn > 0 and self.tester.is_complete(model_prog):
    #                             best_fn = 0
    #                         continue

    #                     with self.settings.stats.duration('subcheck'):
    #                         # if program is inconsistent, then find the smallest inconsistent subprogram and prune it
    #                         # TODO: we could add the constraints for the intermediate solutions
    #                         smaller = self.tester.reduce_inconsistent(model_prog)
    #                         con = ':-' + ','.join(f'rule({self.rulehash_to_id[get_rule_hash(rule)]})' for rule in smaller) + '.'
    #                         str_encoding += con + '\n'
    #                         self.constraints.add(con)
    #                         print('-- RUBBISH --', self.debug_count, best_fn, prog_size(model_prog))
    #                         for rule in model_prog:
    #                             print('\t',format_rule(order_rule(rule)))
    #                         print('subprog')
    #                         for rule in smaller:
    #                             print('\t',format_rule(order_rule(rule)))
    #                     # break to not consider no more models as we need to take into account the new constraint
    #                     break
    #                 break
    #             # print('COMBINE TIME', self.debug_count, t2-t1)

    #         if not model_found or not model_inconsistent:
    #             return best_prog, best_fn
    #     return best_prog, best_fn

    # # @profile
    # def select_solution(self, new_prog):
    #     s = Solver()
    #     self.solver_vars = {}
    #     self.covered_buy = {}
    #     self.rule_id = 0


    #     # for prog, covergae in self.prog_coverage.items():


    #     covered = self.prog_coverage[new_prog]
    #     for rule in new_prog:
    #         # rule_hash = get_rule_hash(rule)
    #         self.rule_id += 1
    #         rule_var = Bool(self.rule_id)
    #         # self.solver_vars[rule_hash] = rule_var
    #         # print(rule_size(rule))
    #         # self.solver.add_soft(rule_var, -rule_size(rule))
    #         # prog_var = rule_var

    #     for ex in covered:
    #         if ex not in self.covered_buy:
    #             self.covered_buy[ex] = set()
    #         self.covered_buy[ex].add(rule_var)
    #         self.solver.add(Or(self.covered_buy[ex]))

    #     print(self.solver.sexpr())


    #     print('asda')
    #     t1 = time.time()
    #     print(self.solver.check())
    #     print(self.solver.model())
    #     t2 = time.time()
    #     print(t2-t1)

    #     t1 = time.time()
    #     print(self.solver.check())
    #     print(self.solver.model())
    #     t2 = time.time()
    #     print(t2-t1)


    #     return [], 0


        # # add example atoms
        # encoding.add(self.example_prog)

        # # add constraints to prune inconsistent recursive programs
        # encoding.update(self.constraints)

        # if len(self.inconsistent) > 0:
        #     # TODO: improve as there is no need to build the constraints each time
        #     # with self.settings.stats.duration('inconsistent thingy'):
        #     for prog in self.inconsistent:
        #         if all(get_rule_hash(rule) in self.rulehash_to_id for rule in prog):
        #             ids = [self.rulehash_to_id[get_rule_hash(rule)] for rule in prog]
        #             con = ':-' + ','.join(f'rule({x})' for x in ids) + '.'
        #             encoding.add(con)

        # model_rules, fn = self.find_combination(encoding)

        # return [self.ruleid_to_rule[k] for k in model_rules], fn

    # @profile
    def update_best_prog(self, prog, pos_covered):
        # TODO: tmp
        rule = list(prog)[0]

        if rule not in self.seen_rules:
            self.rule_count+=1
            rule_id = self.seen_rules[rule] = self.rule_count
        rule_id = self.seen_rules[rule]

        for e in pos_covered:
            self.covered_by[e].add(rule_id)


        rule_vars = {}
        for rule, rule_id in self.seen_rules.items():
            rule_vars[rule_id] = Bool(rule_id)

        solver = Solver()
        for e, xs in self.covered_by.items():
            xs_vars = [rule_vars[rule_id] for rule_id in xs] + [False]
            solver.add(Or(xs_vars))

        print(solver.check())
