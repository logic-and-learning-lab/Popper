# code written by Andreas Niskanen (andreas.niskanen@helsinki.fi)

import clingo
import time
import pickle
import itertools
from . util import format_rule, calc_prog_size, reduce_prog, prog_is_recursive, prog_has_invention, calc_rule_size, rule_is_recursive, format_prog
import collections

import sys
from . import maxsat
from pysat.formula import IDPool

NEW_IDEAS = False
NEW_IDEAS = True

def get_rule_hash(rule):
    head, body = rule
    return hash((head, body))

class Combiner:
    def __init__(self, settings, tester):
        self.prog_pos_covered = {}
        self.prog_neg_covered = {}
        self.settings = settings
        self.tester = tester

        self.pos_example_weight = 1
        self.neg_example_weight = 1

        self.vpool = IDPool()

        self.rule_size_sum = 0
        self.hard_clauses = []
        self.best_cost = None

        self.saved_progs = []
        self.added = []

        self.base_rules = []
        self.recursive_rules = []

        self.inconsistent = set()
        # self.solution_found = False
        self.max_size = None

        self.rulehash_to_id = {}
        self.ruleid_to_rule = {}
        self.ruleid_to_size = {}
        self.rule_var = {}
    
        self.build_example_encoding()

        self.programs_seen = 0

        self.to_delete = set()

    def delete_it(self, prog):
        self.to_delete.add(prog)

    def build_encoding(self):

        # with self.settings.stats.duration('combine.add'):
        encoding = []

        # if we deleted some programs, we must rebuild the encoding
        if self.settings.noisy and self.deleted != 0:
            self.rulehash_to_id = {}
            self.ruleid_to_rule = {}
            self.ruleid_to_size = {}
            self.rule_var = {}
        
            self.build_example_encoding()

            all_programs = self.saved_progs + self.added

            self.programs_seen = 0
        else:
            for i in range(0, self.programs_seen):
                for clause in self.program_clauses[i]:
                    encoding.append(clause)

            for clause in self.hard_clauses:
                encoding.append(clause)

            all_programs = self.added


        bads = set()

        if NEW_IDEAS and self.settings.noisy:
            with self.settings.stats.duration('space idea'):
                # Space remaining idea
                # Assume a best MDL score O, a new program P, where mdl_score(P) > O
                # Then for P to be in a solution with MDL score < O there must be another program Q s.t. fp(P | Q) + size(P) + size(Q) < O
                xs = self.saved_progs + self.added
                for i in range(len(xs)):
                    bad = True
                    prog, pos_covered, neg_covered = xs[i]
                    size1 = calc_prog_size(prog)
                    space_remaining = self.settings.best_mdl - calc_prog_size(prog) - 1
                    for j in range(i, len(xs)):
                        if i==j:
                            continue
                        prog2, pos_covered2, neg_covered2 = xs[j]
                        if prog2 in bads:
                            continue
                        size2 = calc_prog_size(prog2)
                        fp = len(neg_covered|neg_covered2)
                        if fp + size2 + size1 < self.settings.best_mdl:
                            if len(pos_covered|pos_covered2) > len(pos_covered):
                                bad = False
                                break
                    if bad:
                        bads.add(prog)

        if NEW_IDEAS:
            all_programs = [(prog, pos_covered, neg_covered) for (prog, pos_covered, neg_covered) in all_programs if prog not in bads]

        all_programs = all_programs[:100]

        for [prog, pos_covered, neg_covered] in all_programs:


            # UNCOMMENT TO SHOW PROGRAMS ADDED TO THE SOLVER
            # tp = len(pos_covered)
            # fp = len(neg_covered)
            # size = calc_prog_size(prog)
            # fn = self.tester.num_pos - tp
            # print(f'size: {size} fp:{fp} tp:{tp} mdl:{size + fp + fn} {format_prog(prog)}')
            # print(sorted(pos_covered))

            for ex in pos_covered:
                self.programs_covering_example[ex].append(self.programs_seen)

            # if self.settings.nonoise:
                # assert(len(self.prog_neg_covered[prog]) == 0)
            if self.settings.noisy:
                for ex in neg_covered:
                    self.programs_covering_example[ex].append(self.programs_seen)

            rule_vars = []
            ids = []
            for rule in prog:
                rule_hash = get_rule_hash(rule)
                if rule_hash not in self.rulehash_to_id:
                    k = len(self.rulehash_to_id) + 1
                    self.rulehash_to_id[rule_hash] = k
                    self.ruleid_to_rule[k] = rule
                    self.ruleid_to_size[k] = calc_rule_size(rule)
                    ids.append(k)
                else:
                    ids.append(self.rulehash_to_id[rule_hash])
            for rule in prog:
                rule_hash = get_rule_hash(rule)
                rule_id = self.rulehash_to_id[rule_hash]
                rule_size = self.ruleid_to_size[rule_id]

                if rule_id not in self.rule_var:
                    self.rule_var[rule_id] = self.vpool.id("rule({0}))".format(rule_id))
                    self.rule_size_sum += rule_size
                rule_vars.append(self.rule_var[rule_id])
                if self.settings.lex and self.settings.recursion_enabled:
                    if rule_is_recursive(rule):
                        self.recursive_rules.append(rule_id)
                    else:
                        self.base_rules.append(rule_id)

            if len(rule_vars) == 1:
                self.program_var[self.programs_seen] = rule_vars[0]
                self.program_clauses[self.programs_seen] = []
            else:
                self.program_var[self.programs_seen] = self.vpool.id("program({0})".format(self.programs_seen))
                pvar = self.program_var[self.programs_seen]

                clause = [pvar] + [-var for var in rule_vars]
                encoding.append(clause)
                self.program_clauses[self.programs_seen] = [clause]
                for var in rule_vars:
                    clause = [-pvar, var]
                    encoding.append(clause)
                    self.program_clauses[self.programs_seen].append(clause) 

            self.programs_seen += 1
        if self.settings.lex and self.settings.recursion_enabled:
            encoding.append([self.rule_var[rule_id] for rule_id in self.base_rules])

        for ex in self.settings.pos_index:
            encoding.append([-self.example_covered_var[ex]] + [self.program_var[p] for p in self.programs_covering_example[ex]])
            #for p in self.programs_covering_example[ex]:
            #    encoding.append([self.example_covered_var[ex], -self.program_var[p]])
        if not self.settings.nonoise:
            for ex in self.settings.neg_index:
                #encoding.append([-self.example_covered_var[ex]] + [self.program_var[p] for p in self.programs_covering_example[ex]])
                for p in self.programs_covering_example[ex]:
                    encoding.append([self.example_covered_var[ex], -self.program_var[p]])

        return encoding

    def build_example_encoding(self):
        self.example_covered_var = {}
        self.programs_covering_example = {}
        self.program_var = {}
        self.program_clauses = {}
        for i in self.settings.pos_index:
            self.example_covered_var[i] = self.vpool.id("example_covered({0})".format(i))
            self.programs_covering_example[i] = []
        if not self.settings.nonoise:
            for i in self.settings.neg_index:
                self.example_covered_var[i] = self.vpool.id("example_covered({0})".format(i))
                self.programs_covering_example[i] = []
        self.rule_var = {}

    def update_prog_index(self, new_progs):

        if self.settings.noisy:
            new_saved_progs = []
            new_programs_to_add = []

            # this variable checks whether any previously seen program has been deleted, in which case we rebuild the encoding
            self.deleted = 0

            min_size = None
            if len(self.saved_progs+self.added) > 0:
                min_size = min(calc_prog_size(prog) for (prog, pos_covered, neg_covered) in self.saved_progs+self.added)

        # we can only delete programs from the combine stage with the mdl cost function
        if self.settings.noisy:                
            for [prog, pos_covered, neg_covered] in self.saved_progs+self.added:
                if len(neg_covered)+calc_prog_size(prog) >= self.settings.best_mdl:
                    # print('delete', format_prog(prog))
                    # AC: PUSH THIS CHECK TO THE MAIN LOOP SO WE CAN REMOVE ITEMS FROM SUCCESS_SETS_NOISE
                    self.deleted += 1
                    continue

                if prog in self.to_delete:
                    self.deleted += 1
                    continue

                if len(neg_covered)+calc_prog_size(prog) >= self.settings.best_mdl-min_size:
                    self.deleted += 1
                    continue

                self.prog_pos_covered[prog] = pos_covered
                self.prog_neg_covered[prog] = neg_covered
                new_saved_progs.append([prog, pos_covered, neg_covered])

            for [prog, pos_covered, neg_covered] in new_progs:
                if self.settings.noisy and len(neg_covered)+calc_prog_size(prog) >= self.settings.best_mdl:
                    continue

                if prog in self.to_delete:
                    self.deleted += 1
                    continue

                if min_size and len(neg_covered)+calc_prog_size(prog) >= self.settings.best_mdl-min_size:
                    self.deleted += 1
                    continue

                self.prog_pos_covered[prog] = pos_covered
                self.prog_neg_covered[prog] = neg_covered
                new_programs_to_add.append([prog, pos_covered, neg_covered])

        else:
            for [prog, pos_covered, neg_covered] in new_progs:
                self.prog_pos_covered[prog] = pos_covered
                self.prog_neg_covered[prog] = neg_covered

        if self.settings.noisy:
            # we save already seen programs and new programs separately
            # we will use both if we need to rebuild the encoding
            # otherwise we only add self.added
            # print(f"number of programs in the combine stage {len(new_saved_progs)} + {len(new_programs_to_add)} deleted {self.deleted}")
            self.saved_progs = new_saved_progs
            self.added = new_programs_to_add
        else:
            self.added = new_progs

        return 

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
        clause = [-self.rule_var[k] for k in ids]
        self.hard_clauses.append(clause)

    def find_combination(self, encoding, timeout):
        soft_clauses = []
        weights = []

        best_prog = []
        best_fp = False
        best_fn = False
        best_size = False

        if self.settings.best_prog_score:
            tp_, fn_, tn_, fp_, size_ = self.settings.best_prog_score
        if self.settings.best_mdl:
            mdl_ = self.settings.best_mdl

        # with self.settings.stats.duration('combine.add'):
        if self.settings.lex:
            soft_lit_groups = []
            rule_soft_lits = []
            for rule_id in self.rule_var:
                if self.rule_var[rule_id] is not None:
                    rule_soft_lits.append(-self.rule_var[rule_id])
                    weights.append(self.ruleid_to_size[rule_id])
            if self.settings.best_prog_score:
                if fn_ == 0:
                    for i in self.settings.pos_index:
                        encoding.append([self.example_covered_var[i]])
                    if fp_ == 0:
                        if not self.settings.nonoise:
                            for i in self.settings.neg_index:
                                encoding.append([-self.example_covered_var[i]])
                        soft_lit_groups = [[lit for lit in rule_soft_lits]]
                    else:
                        assert(not self.settings.nonoise)
                        soft_lit_groups = [[-self.example_covered_var[i] for i in self.settings.neg_index]]
                        soft_lit_groups.append([lit for lit in rule_soft_lits])
                else:
                    soft_lit_groups = [[self.example_covered_var[i] for i in self.settings.pos_index]]
                    if not self.settings.nonoise:
                        soft_lit_groups.append([-self.example_covered_var[i] for i in self.settings.neg_index])
                    soft_lit_groups.append([lit for lit in rule_soft_lits])
            else:
                soft_lit_groups = [[self.example_covered_var[i] for i in self.settings.pos_index]]
                if not self.settings.nonoise:
                    soft_lit_groups.append([-self.example_covered_var[i] for i in self.settings.neg_index])
                soft_lit_groups.append([lit for lit in rule_soft_lits])
        else:
            for rule_id in self.rule_var:
                if self.rule_var[rule_id] is not None:
                    soft_clauses.append([-self.rule_var[rule_id]])
                    weights.append(self.ruleid_to_size[rule_id])
            for i in self.settings.pos_index:
                soft_clauses.append([self.example_covered_var[i]])
                weights.append(self.pos_example_weight)
            if not self.settings.nonoise:
                for i in self.settings.neg_index:
                    soft_clauses.append([-self.example_covered_var[i]])
                    weights.append(self.neg_example_weight)

        while True:
            model_found = False
            model_inconsistent = False

            # with self.settings.stats.duration('combine.solve'):
            if not self.settings.lex:
                if timeout is None or self.settings.last_combine_stage:
                    cost, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights, self.settings)
                else:
                    cost, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings, timeout)
            else:
                if timeout is None or self.settings.last_combine_stage:
                    cost, model = maxsat.exact_lex_solve(encoding, soft_lit_groups, weights, self.settings)
                else:
                    cost, model = maxsat.anytime_lex_solve(encoding, soft_lit_groups, weights, self.settings, timeout)

            if model is None:
                print("WARNING: No solution found, exit combiner.")
                break

            fn = sum([1 for i in self.settings.pos_index if model[self.example_covered_var[i]-1] < 0])
            fp = 0
            if not self.settings.nonoise:
                fp = sum([1 for i in self.settings.neg_index if model[self.example_covered_var[i]-1] > 0])
            size = sum([self.ruleid_to_size[rule_id] for rule_id in self.ruleid_to_size if model[self.rule_var[rule_id]-1] > 0])

            if self.settings.lex:
                if self.settings.best_prog_score:
                    if fn_ < fn or (fn_ == fn and fp_ < fp) or (fn_ == fn and fp_ == fp and size_ <= size):
                        # print("WARNING: No better solution found, exit combiner.")
                        break
            else:
                if self.settings.best_prog_score:
                    if mdl_ <= self.pos_example_weight * fn + self.neg_example_weight * fp + size:
                        # print("WARNING: No better solution found, exit combiner.")
                        break

            model_found = True
            model_incomplete = False

            rules = [rule_id for rule_id in self.ruleid_to_rule if model[self.rule_var[rule_id]-1] > 0]
            model_prog = [self.ruleid_to_rule[k] for k in rules]

            if not prog_is_recursive(model_prog) and not prog_has_invention(model_prog):
                best_prog = rules
                best_fp = fp
                best_fn = fn
                best_size = size
                break

            else:
                if not self.settings.lex:
                    print("ERROR: Combining rec or pi programs not supported with MDL objective. Exiting.")
                    sys.exit(1)

                model_inconsistent = self.tester.test_prog_inconsistent(model_prog)
                if not model_inconsistent:
                    best_prog = rules
                    best_fp = fp
                    best_fn = fn
                    best_size = size
                    break

                smaller = self.tester.reduce_inconsistent(model_prog)
                ids = [self.rulehash_to_id[get_rule_hash(rule)] for rule in smaller]
                clause = [-self.rule_var[k] for k in ids]
                encoding.append(clause)

        if self.settings.lex:
            return best_prog, (best_fn, best_fp, best_size)
        return best_prog, best_fn + best_fp + best_size

    def select_solution(self, timeout):
        encoding = self.build_encoding()
        model_rules, cost = self.find_combination(encoding, timeout)
        return [self.ruleid_to_rule[k] for k in model_rules], cost

    def update_best_prog(self, new_progs, timeout=None):
        if timeout is None:
            timeout = self.settings.maxsat_timeout

        self.update_prog_index(new_progs)
        new_solution, cost = self.select_solution(timeout)

        if len(new_solution) == 0:
            return None

        if self.best_cost is None:
            self.best_cost = cost
        else:
            if cost >= self.best_cost:
                return None

        self.best_cost = cost

        new_solution = reduce_prog(new_solution)
        pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        tp = len(pos_covered)
        fp = len(neg_covered)
        tn = self.tester.num_neg - fp
        fn = self.tester.num_pos - tp
        size = calc_prog_size(new_solution)

        return new_solution, (tp, fn, tn, fp, size)
