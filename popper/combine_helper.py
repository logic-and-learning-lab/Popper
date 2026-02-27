# code originally written by Andreas Niskanen (andreas.niskanen@helsinki.fi)
from . util import calc_prog_size, reduce_prog, prog_is_recursive, prog_has_invention, calc_rule_size, rule_is_recursive, format_prog
from collections import defaultdict
from . import maxsat
from pysat.formula import IDPool
import time
import bitarray

POS_EXAMPLE_WEIGHT = 1
NEG_EXAMPLE_WEIGHT = 1

import time
from collections import defaultdict
from bitarray.util import subset, any_and, ones
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations

class CombineHelper:

    def __init__(self, settings, tester, state, generator):
        self.settings = settings
        self.tester = tester
        self.state = state
        self.generator = generator
        self.covered_by_pos = defaultdict(set)
        self.covered_by_neg = defaultdict(set)
        self.coverage_pos = {}
        self.coverage_neg = {}
        self.cached_prog_size = {}
        self.prog_lookup = {}
        self.scores = {}
        self.to_combine = set()
        self.uncovered = ones(self.tester.num_pos)
        self.saved_progs = set()
        self.inconsistent = set()
        self.load_solver()

    def combine(self, prog, prog_size, test_result, subsumed, noisy_subsumed,add_gen, pruned_more_general, is_recursive, has_invention, size_change, last_combine_stage=False):

        tp, fn, fp, tn =  test_result.tp, test_result.fn, test_result.fp, test_result.tn
        pos_covered, neg_covered = test_result.pos_covered, test_result.neg_covered
        inconsistent = test_result.inconsistent
        too_few_tp, too_many_fp = test_result.too_few_tp, test_result.too_many_fp

        add_to_combiner = self.decide_whether_to_combine(prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed, add_gen, tp, fp, fn, pruned_more_general, too_few_tp, too_many_fp, is_recursive, has_invention)

        if add_to_combiner:
            self.to_combine.add(hash(prog))

        call_combine = len(self.to_combine) > 0
        call_combine = call_combine and (self.settings.noisy or self.settings.solution_found)
        call_combine = call_combine and (len(self.to_combine) >= self.settings.batch_size or size_change)

        if self.settings.recursion_enabled:
            call_combine = len(self.to_combine) > 0

        combine_result1 = None
        if add_to_combiner and (not self.settings.noisy) and (not self.settings.solution_found) and (not self.settings.recursion_enabled):

            if any_and(self.uncovered, pos_covered):
                if self.settings.solution:
                    tmp = self.settings.solution | prog
                else:
                    tmp = prog
                self.uncovered = self.uncovered & ~pos_covered
                tp2 = self.tester.num_pos - self.uncovered.count(1)
                hypothesis_size = calc_prog_size(tmp)
                combine_result1 = tmp, hypothesis_size, (tp2, self.uncovered.count(1), self.tester.num_neg, 0)
                call_combine = not self.uncovered.any()

        if call_combine:
            if self.settings.noisy:
                self.filter_combine_programs(self.to_combine)

            with self.settings.stats.duration('combine'):
                combine_result2 = self.update_best_prog(self.to_combine, last_combine_stage=last_combine_stage)

            self.to_combine.clear()

            if combine_result2:
                return combine_result2
        return combine_result1

    def decide_whether_to_combine(self, prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed,add_gen, tp, fp, fn, pruned_more_general, too_few_tp, too_many_fp, is_recursive, has_invention):
        add_to_combiner = False
        if self.settings.noisy and (not too_few_tp) and (not too_many_fp) and (not is_recursive) and (not has_invention) and tp > prog_size + fp and fp + prog_size < self.settings.best_mdl and (not noisy_subsumed):
            local_delete = set()
            ignore_this_prog = (pos_covered, neg_covered) in self.state.success_sets_noise

            if not ignore_this_prog:
                s_pos = set.intersection(*(self.covered_by_pos[ex] for ex, ex_cov in enumerate(pos_covered) if ex_cov == 1))
                for prog1 in s_pos:
                    n1 = self.coverage_neg[prog1]
                    if subset(n1, neg_covered):
                        ignore_this_prog = True
                        break

            if not ignore_this_prog and (inconsistent or fp > 0):
                s_neg = set.intersection(*(self.covered_by_neg[ex] for ex, ex_cov in enumerate(neg_covered) if ex_cov == 1))
                for prog1 in s_neg:
                    if subset(self.coverage_pos[prog1], pos_covered):
                        size1, tp1, fp1 = self.scores[prog1]
                        if size1 == prog_size:
                            local_delete.add(prog1)
                            continue

                        if fp == fp1 and (tp - prog_size) < (tp1 - size1):
                            ignore_this_prog = True
                            break

                        if tp == tp1 and (fp + prog_size) >= (fp1 + size1):
                            ignore_this_prog = True
                            break

                        if (tp - fp - prog_size) <= (tp1 - fp1 - size1):
                            ignore_this_prog = True
                            break

            if not inconsistent:
                not_covered = self.tester.pos_examples_ ^ pos_covered
                progs_not_subsumed = set.union(*(self.covered_by_pos[ex] for ex, ex_cov in enumerate(not_covered) if ex_cov == 1))
                all_progs = set.union(*(self.covered_by_pos[ex] for ex, ex_cov in enumerate(self.tester.pos_examples_) if ex_cov == 1))
                s_pos2 = all_progs.difference(progs_not_subsumed)
                for prog1 in s_pos2:
                    size1, tp1, fp1 = self.scores[prog1]
                    if size1 >= prog_size:
                        local_delete.add(prog1)

            for k in local_delete:
                assert k in (self.saved_progs | self.to_combine)
                if k in self.saved_progs:
                    self.saved_progs.remove(k)
                elif k in self.to_combine:
                    self.to_combine.remove(k)

                k_pos, k_neg = self.coverage_pos[k], self.coverage_neg[k]
                del self.state.success_sets_noise[(k_pos, k_neg)]
                for ex, ex_cov in enumerate(k_pos):
                    if ex_cov == 1:
                        self.covered_by_pos[ex].remove(k)
                for ex, ex_cov in enumerate(k_neg):
                    if ex_cov == 1:
                        self.covered_by_neg[ex].remove(k)
                del self.coverage_pos[k]
                del self.coverage_neg[k]
                del self.scores[k]
                del self.cached_prog_size[k]
                del self.prog_lookup[k]

            if not ignore_this_prog:
                self.state.success_sets_noise[(pos_covered, neg_covered)] = (prog, prog_size, fn, fp, tp)
                add_to_combiner = True
                k = hash(prog)

                for ex, x in enumerate(pos_covered):
                    if x == 1:
                        self.covered_by_pos[ex].add(k)
                for ex, x in enumerate(neg_covered):
                    if x == 1:
                        self.covered_by_neg[ex].add(k)

                self.coverage_pos[k] = pos_covered
                self.coverage_neg[k] = neg_covered
                self.cached_prog_size[k] = prog_size
                self.scores[k] = (prog_size, tp, fp)
                self.prog_lookup[k] = prog

                if fp == 0:
                    self.state.success_sets[pos_covered] = prog_size
                    for p, s in self.state.success_sets.items():
                        if p == pos_covered:
                            continue
                        self.state.paired_success_sets[s + prog_size].add(p | pos_covered)

        elif not self.settings.noisy:
            if (not inconsistent) and (not subsumed) and (not add_gen) and tp > 0 and (not pruned_more_general):
                add_to_combiner = True

                if not self.settings.recursion_enabled:
                    to_delete = []
                    for pos_covered2, prog_size2 in self.state.success_sets.items():
                        if prog_size > prog_size2:
                            continue
                        if subset(pos_covered2, pos_covered):
                            to_delete.append(self.state.success_sets_aux[pos_covered2])

                    for prog2_hash in to_delete:
                        pos_covered2 = self.coverage_pos[prog2_hash]
                        if prog2_hash in self.to_combine:
                            self.to_combine.remove(prog2_hash)
                        elif prog2_hash in self.saved_progs:
                            self.saved_progs.remove(prog2_hash)
                        else:
                            assert False
                        del self.state.success_sets[pos_covered2]
                        del self.state.success_sets_aux[pos_covered2]
                        del self.coverage_pos[prog2_hash]
                        del self.coverage_neg[prog2_hash]
                        del self.prog_lookup[prog2_hash]

                k = hash(prog)
                self.state.success_sets[pos_covered] = prog_size
                self.state.success_sets_aux[pos_covered] = k
                self.coverage_pos[k] = pos_covered
                self.coverage_neg[k] = neg_covered
                self.prog_lookup[k] = prog

                for p, s in self.state.success_sets.items():
                    if p == pos_covered:
                        continue
                    self.state.paired_success_sets[s + prog_size].add(p | pos_covered)

                if self.settings.min_size is None:
                    self.settings.min_size = prog_size

        return add_to_combiner

    def filter_combine_programs(self, to_combine_set):
        xs = self.saved_progs | to_combine_set
        min_sz = min(self.cached_prog_size[prog] for prog in xs)
        must_beat = self.settings.best_mdl - min_sz

        to_delete = set()
        for prog_hash in xs:
            size, tp, fp = self.scores[prog_hash]
            if fp + size >= must_beat:
                to_delete.add(prog_hash)

        for prog_hash in to_delete:
            if prog_hash in self.saved_progs:
                self.saved_progs.remove(prog_hash)
            else:
                to_combine_set.remove(prog_hash)


    def load_solver(self):
        if self.settings.debug:
            self.settings.logger.debug(f'Load exact solver: {self.settings.solver}')

        if self.settings.solver not in ['rc2', 'uwr', 'wmaxcdcl']:
            print('INVALID SOLVER')
            exit()

        self.settings.maxsat_timeout = None
        self.settings.stats.maxsat_calls = 0

        if self.settings.solver == 'rc2':
            self.settings.exact_maxsat_solver = 'rc2'
            self.settings.old_format = False
        elif self.settings.solver == 'uwr':
            self.settings.exact_maxsat_solver='uwrmaxsat'
            self.settings.exact_maxsat_solver_params="-v0 -no-sat -no-bin -m -bm"
            self.settings.old_format = False
        else:
            self.settings.exact_maxsat_solver='wmaxcdcl'
            self.settings.exact_maxsat_solver_params=""
            self.settings.old_format = True

        if self.settings.noisy:
            self.settings.lex = False
        else:
            self.settings.lex = True
            self.settings.best_mdl = False
            self.settings.lex_via_weights = False

        if self.settings.debug:
            self.settings.logger.debug(f'Load anytime solver:{self.settings.anytime_solver}')

        if self.settings.anytime_solver in ['wmaxcdcl', 'nuwls']:
            self.settings.maxsat_timeout = self.settings.anytime_timeout
            if self.settings.anytime_solver == 'wmaxcdcl':
                self.settings.anytime_maxsat_solver = 'wmaxcdcl'
                self.settings.anytime_maxsat_solver_params = ""
                self.settings.anytime_maxsat_solver_signal = 10
                self.settings.old_format = True
            elif self.settings.anytime_solver == 'nuwls':
                self.settings.anytime_maxsat_solver = 'NuWLS-c'
                self.settings.anytime_maxsat_solver_params = ""
                self.settings.anytime_maxsat_solver_signal = 15
            else:
                print('INVALID ANYTIME SOLVER')
                exit()

        # TODO: temporary config, need to be modified
        # self.settings.last_combine_stage = False

        # return Combiner(self.settings, self.tester, self.coverage_pos, self.coverage_neg, self.prog_lookup)

    def add_inconsistent(self, prog_hash):
        self.inconsistent.add(prog_hash)

    def find_combination(self, last_combine_stage=False):
        # print('')
        # print(f'lex:{self.settings.lex}')
        # print(f'best_mdl:{self.settings.best_mdl}')
        # print(f'best_prog_score:{self.settings.best_prog_score}')
        # print(f'best_prog_size:{self.settings.best_prog_size}')

        timeout = self.settings.maxsat_timeout
        encoding = []

        rulehash_to_id = {}
        ruleid_to_rule = {}
        ruleid_to_size = {}
        rule_var = {}

        base_rules = []
        recursive_rules = []

        programs_covering_pos_example = {}
        programs_covering_neg_example = {}
        program_var = {}
        program_clauses = {}
        vpool = IDPool()

        pos_example_covered_var = {}
        neg_example_covered_var = {}

        pos_index = list(range(self.tester.num_pos))
        neg_index = list(range(self.tester.num_neg))

        for i in pos_index:
            pos_example_covered_var[i] = vpool.id("pos_example_covered({0})".format(i))
            programs_covering_pos_example[i] = []

        if self.settings.noisy:
            for i in neg_index:
                neg_example_covered_var[i] = vpool.id("neg_example_covered({0})".format(i))
                programs_covering_neg_example[i] = []

        rule_var = {}

        for program_count, prog_hash in enumerate(self.saved_progs):

            prog = self.prog_lookup[prog_hash]

            pos_covered = self.coverage_pos[prog_hash]
            neg_covered = self.coverage_neg[prog_hash]

            # UNCOMMENT TO SHOW PROGRAMS ADDED TO THE SOLVER
            # tp = len(pos_covered)
            # fp = len(neg_covered)
            # size = calc_prog_size(prog)
            # fn = self.tester.num_pos - tp
            # print(f'size: {size} fp:{fp} tp:{tp} mdl:{size + fp + fn} {format_prog(prog)}')
            # print(sorted(pos_covered))

            for ex, x in enumerate(pos_covered):
                if x == 1:
                    # AC: REALLY REALLY HACKY
                    # WE NEED TO +1 BECAUSE OF POS_INDEX BELOW
                    programs_covering_pos_example[ex].append(program_count)

            if self.settings.noisy:
                for ex, x in enumerate(neg_covered):
                    if x == 1:
                        programs_covering_neg_example[ex].append(program_count)

            rule_vars = []
            ids = []
            for rule in prog:
                rule_hash = hash(rule)
                if rule_hash not in rulehash_to_id:
                    k = len(rulehash_to_id) + 1
                    rulehash_to_id[rule_hash] = k
                    ruleid_to_rule[k] = rule
                    ruleid_to_size[k] = calc_rule_size(rule)
                    ids.append(k)
                else:
                    ids.append(rulehash_to_id[rule_hash])

            for rule in prog:
                rule_hash = hash(rule)
                rule_id = rulehash_to_id[rule_hash]
                rule_size = ruleid_to_size[rule_id]

                if rule_id not in rule_var:
                    rule_var[rule_id] = vpool.id("rule({0}))".format(rule_id))

                rule_vars.append(rule_var[rule_id])
                if self.settings.lex and self.settings.recursion_enabled:
                    if rule_is_recursive(rule):
                        recursive_rules.append(rule_id)
                    else:
                        base_rules.append(rule_id)

            if len(rule_vars) == 1:
                program_var[program_count] = rule_vars[0]
                program_clauses[program_count] = []
            else:
                program_var[program_count] = vpool.id("program({0})".format(program_count))
                pvar = program_var[program_count]

                clause = [pvar] + [-var for var in rule_vars]
                encoding.append(clause)
                program_clauses[program_count] = [clause]
                for var in rule_vars:
                    clause = [-pvar, var]
                    encoding.append(clause)
                    program_clauses[program_count].append(clause)

        if self.settings.lex and self.settings.recursion_enabled:
            encoding.append([rule_var[rule_id] for rule_id in base_rules])

        for ex in pos_index:
            encoding.append([-pos_example_covered_var[ex]] + [program_var[p] for p in programs_covering_pos_example[ex]])

        if self.settings.noisy:
            for ex in neg_index:
                for p in programs_covering_neg_example[ex]:
                    encoding.append([neg_example_covered_var[ex], -program_var[p]])

        soft_clauses = []
        weights = []

        if self.settings.best_prog_score:
            tp_, fn_, tn_, fp_ = self.settings.best_prog_score
            size_ = self.settings.best_prog_size

        if self.settings.lex:
            soft_lit_groups = []
            rule_soft_lits = []
            for rule_id in rule_var:
                if rule_var[rule_id] is not None:
                    rule_soft_lits.append(-rule_var[rule_id])
                    weights.append(ruleid_to_size[rule_id])

            if self.settings.best_prog_score:
                if fn_ == 0:
                    for i in pos_index:
                        encoding.append([pos_example_covered_var[i]])
                    if fp_ == 0:
                        if not self.settings.nonoise:
                            for i in neg_index:
                                encoding.append([-neg_example_covered_var[i]])
                        soft_lit_groups = [[lit for lit in rule_soft_lits]]
                    else:
                        soft_lit_groups = [[-neg_example_covered_var[i] for i in neg_index]]
                        soft_lit_groups.append([lit for lit in rule_soft_lits])
                else:
                    soft_lit_groups = [[pos_example_covered_var[i] for i in pos_index]]
                    if not self.settings.nonoise:
                        soft_lit_groups.append([-neg_example_covered_var[i] for i in neg_index])
                    soft_lit_groups.append([lit for lit in rule_soft_lits])
            else:
                soft_lit_groups = [[pos_example_covered_var[i] for i in pos_index]]
                if not self.settings.nonoise:
                    soft_lit_groups.append([-neg_example_covered_var[i] for i in neg_index])
                soft_lit_groups.append([lit for lit in rule_soft_lits])
        else:
            for rule_id in rule_var:
                if rule_var[rule_id] is not None:
                    soft_clauses.append([-rule_var[rule_id]])
                    weights.append(ruleid_to_size[rule_id])
            for i in pos_index:
                soft_clauses.append([pos_example_covered_var[i]])
                weights.append(POS_EXAMPLE_WEIGHT)
            if not self.settings.nonoise:
                for i in neg_index:
                    soft_clauses.append([-neg_example_covered_var[i]])
                    weights.append(NEG_EXAMPLE_WEIGHT)

        # PRUNE INCONSISTENT
        for prog in self.inconsistent:
            should_add = True
            ids = []
            for rule in prog:
                k = hash(rule)
                if k not in rulehash_to_id:
                    should_add = False
                    break
                ids.append(k)
            if not should_add:
                continue
            ids = [rulehash_to_id[k] for k in ids]
            clause = [-rule_var[k] for k in ids]
            encoding.append(clause)

        best_prog = []
        best_fp = False
        best_fn = False
        best_size = False

        if self.settings.best_mdl:
            mdl_ = self.settings.best_mdl

        while True:
            model_found = False
            model_inconsistent = False

            if not self.settings.lex:
                if timeout is None or last_combine_stage:
                    cost, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights, self.settings)
                else:
                    cost, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings, timeout)
            else:
                if timeout is None or last_combine_stage:
                    cost, model = maxsat.exact_lex_solve(encoding, soft_lit_groups, weights, self.settings)
                else:
                    cost, model = maxsat.anytime_lex_solve(encoding, soft_lit_groups, weights, self.settings, timeout)

            if model is None:
                print("WARNING: No solution found, exit combiner.")
                break

            fn = sum(1 for i in pos_index if model[pos_example_covered_var[i]-1] < 0)
            fp = 0
            if not self.settings.nonoise:
                fp = sum(1 for i in neg_index if model[neg_example_covered_var[i]-1] > 0)
            size = sum([ruleid_to_size[rule_id] for rule_id in ruleid_to_size if model[rule_var[rule_id]-1] > 0])

            if self.settings.lex:
                if self.settings.best_prog_score:
                    if fn_ < fn or (fn_ == fn and fp_ < fp) or (fn_ == fn and fp_ == fp and size_ <= size):
                        break
            else:
                if self.settings.best_prog_score:
                    if mdl_ <= POS_EXAMPLE_WEIGHT * fn + NEG_EXAMPLE_WEIGHT * fp + size:
                        break

            model_found = True
            model_incomplete = False

            rules = [rule_id for rule_id in ruleid_to_rule if model[rule_var[rule_id]-1] > 0]
            model_prog = [ruleid_to_rule[k] for k in rules]

            if not prog_is_recursive(model_prog) and not prog_has_invention(model_prog):
                best_prog = rules
                best_fp = fp
                best_fn = fn
                best_size = size
                break

            else:
                if not self.settings.lex:
                    print("ERROR: Combining rec or pi programs not supported with MDL objective. Exiting.")
                    assert(False)

                model_inconsistent = self.tester.test_prog_inconsistent(model_prog)
                if not model_inconsistent:
                    best_prog = rules
                    best_fp = fp
                    best_fn = fn
                    best_size = size
                    break

                smaller = self.tester.reduce_inconsistent(model_prog)
                ids = [rulehash_to_id[hash(rule)] for rule in smaller]
                clause = [-rule_var[k] for k in ids]
                encoding.append(clause)

        best_prog = [ruleid_to_rule[k] for k in best_prog]
        if self.settings.lex:
            # return best_prog, (best_fn, best_fp, best_size)
            return best_prog, best_size
        return best_prog, best_fn + best_fp + best_size

    def update_best_prog(self, new_progs, last_combine_stage=False):

        self.saved_progs.update(new_progs)
        if not self.saved_progs:
            return

        new_solution, cost = self.find_combination(last_combine_stage)

        if len(new_solution) == 0:
            return None

        if self.settings.noisy:
            if cost > self.settings.best_mdl:
                assert(False)
                return None
        elif self.settings.solution_found and cost > self.settings.best_prog_size:
            print(cost, self.settings.best_prog_size, self.settings.solution_found)
            assert(False)
            return None

        new_solution = reduce_prog(new_solution)
        pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        tp = pos_covered.count(1)
        fp = neg_covered.count(1)
        tn = self.tester.num_neg - fp
        fn = self.tester.num_pos - tp
        size = calc_prog_size(new_solution)

        return new_solution, size, (tp, fn, tn, fp)
