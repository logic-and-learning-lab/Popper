# code written by Andreas Niskanen (andreas.niskanen@helsinki.fi)

import clingo
import time
import pickle
import itertools
from . util import format_rule, calc_prog_size, format_prog, reduce_prog, prog_is_recursive, prog_has_invention, \
    rule_size, rule_is_recursive
import collections

import sys
from . import maxsat
from pysat.formula import IDPool

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

        self.pos_example_weight = 1
        self.neg_example_weight = 1

        self.vpool = IDPool()
        self.programs_seen = 0
        self.rule_size_sum = 0
        self.hard_clauses = []
        self.rule_var_softs = set()
        self.best_cost = None

        self.build_example_encoding()
        self.rulehash_to_id = {}
        self.ruleid_to_rule = {}
        self.ruleid_to_size = {}

        self.saved_progs = collections.defaultdict(set)
        self.rule_to_prog = collections.defaultdict(list)

        self.base_rules = []
        self.recursive_rules = []

        self.inconsistent = set()
        # self.solution_found = False
        self.max_size = None

    def update_deleted_progs(self, old_score, new_score):
        pass # TODO
        with self.settings.stats.duration('delete'):
            to_delete_progs = set().union(*[self.saved_progs[k] for k in range(new_score, old_score+1)])
            if to_delete_progs:
                for prog in to_delete_progs:
                    del self.prog_pos_covered[frozenset([self.ruleid_to_rule[r] for r in prog])]
                    del self.prog_neg_covered[frozenset([self.ruleid_to_rule[r] for r in prog])]
                    self.settings.stats.deleted_count += 1
                to_delete_rules = set().union(*to_delete_progs)

                to_delete = set()
                for rule in to_delete_rules:
                    if all([prog in to_delete_progs for prog in self.rule_to_prog[rule]]):
                        to_delete.add(rule)

                if to_delete:
                    for rule in to_delete:
                        self.rule_var[rule] = None

                other = set()
                for k in range(new_score, old_score + 1):
                    del_k = []
                    for ids in self.saved_progs[k]:
                        if set(ids).issubset(to_delete):
                            del_k.append(ids)
                        elif set(ids).issubset(to_delete_rules):
                            other.add(ids)
                            self.hard_clauses.append([-self.rule_var[i] for i in ids])
                    for ids in del_k:
                        self.saved_progs[k].remove(ids)

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
                ids.append(k)
            else:
                ids.append(self.rulehash_to_id[rule_hash])
        for rule in prog:
            self.rule_to_prog[self.rulehash_to_id[get_rule_hash(rule)]].append(tuple(ids))
        self.saved_progs[len(neg_covered)+calc_prog_size(prog)].add(tuple(ids))

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
                self.hard_clauses.append(clause)
                encoding.append(clause)

        if self.settings.lex:
            return best_prog, (best_fn, best_fp, best_size)
        return best_prog, best_fn + best_fp + best_size

    def build_encoding(self, new_progs):
        # with self.settings.stats.duration('combine.add'):
        encoding = []

        for i in range(1, self.programs_seen+1):
            for clause in self.program_clauses[i]:
                encoding.append(clause)

        for clause in self.hard_clauses:
            encoding.append(clause)
        all_rule_vars = []

        for [new_prog, _, _] in new_progs:
            self.programs_seen += 1

            for ex in self.prog_pos_covered[new_prog]:
                self.programs_covering_example[ex].append(self.programs_seen)

            if self.settings.nonoise:
                assert(len(self.prog_neg_covered[new_prog]) == 0)
            for ex in self.prog_neg_covered[new_prog]:
                self.programs_covering_example[ex].append(self.programs_seen)

            prog_rules = set()
            rule_vars = []
            for rule in new_prog:
                rule_hash = get_rule_hash(rule)
                rule_id = self.rulehash_to_id[rule_hash]
                rule_size = self.ruleid_to_size[rule_id]
                prog_rules.add(rule_id)
                if rule_id not in self.rule_var:
                    self.rule_var[rule_id] = self.vpool.id("rule({0}))".format(rule_id))
                    self.rule_var_softs.add((self.rule_var[rule_id], rule_size))
                    self.rule_size_sum += rule_size
                rule_vars.append(self.rule_var[rule_id])
                if self.settings.lex and self.settings.recursion_enabled:
                    if rule_is_recursive(rule):
                        self.recursive_rules.append(rule_id)
                    else:
                        self.base_rules.append(rule_id)
            all_rule_vars += rule_vars

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

        #encoding.append(all_rule_vars)
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

    def select_solution(self, new_progs, timeout):
        encoding = self.build_encoding(new_progs)
        model_rules, cost = self.find_combination(encoding, timeout)
        return [self.ruleid_to_rule[k] for k in model_rules], cost

    def update_best_prog(self, new_progs, timeout=None):
        if timeout is None:
            timeout = self.settings.maxsat_timeout
        for [prog, pos_covered, neg_covered] in new_progs:
            self.update_prog_index(prog, pos_covered, neg_covered)

        new_solution, cost = self.select_solution(new_progs, timeout)

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
