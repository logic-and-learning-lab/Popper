import time
from collections import defaultdict
from bitarray.util import subset, any_and, ones
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations
from . combine import Combiner

class CombineHelper:

    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state
        self.covered_by_pos = defaultdict(set)
        self.covered_by_neg = defaultdict(set)

        # program_hash -> coverage (bit_arrary)
        self.coverage_pos = {}
        self.coverage_neg = {}

        self.cached_prog_size = {}
        self.prog_lookup = {}
        self.scores = {}

        self.combiner = self.load_solver()
        self.to_combine = set()

        self.uncovered = ones(self.tester.num_pos)


    def combine(self, prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed,add_gen, tp, fp, fn, pruned_more_general, skipped, skip_early_neg, is_recursive, has_invention, size_change):


        add_to_combiner = self.decide_whether_to_combine(prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed, add_gen, tp, fp, fn, pruned_more_general, skipped, skip_early_neg, is_recursive, has_invention)

        if add_to_combiner:
            self.to_combine.add(hash(prog))

        call_combine = len(self.to_combine) > 0
        call_combine = call_combine and (self.settings.noisy or self.settings.solution_found)
        call_combine = call_combine and (len(self.to_combine) >= self.settings.batch_size or size_change)

        if add_to_combiner and (not self.settings.noisy) and (not self.settings.solution_found) and (not self.settings.recursion_enabled):
            if any_and(self.uncovered, pos_covered):
                if self.settings.solution:
                    self.settings.solution = self.settings.solution | prog
                else:
                    self.settings.solution = prog
                self.uncovered = self.uncovered & ~pos_covered
                tp2 = self.tester.num_pos - self.uncovered.count(1)
                fn2 = self.uncovered.count(1)
                tn2 = self.tester.num_neg
                fp2 = 0
                hypothesis_size = calc_prog_size(self.settings.solution)
                self.settings.best_prog_score = (tp2, fn2, tn2, fp2, hypothesis_size)
                self.settings.print_incomplete_solution2(self.settings.solution, tp2, fn2, tn2, fp2, hypothesis_size)

                if not self.uncovered.any():
                    self.settings.solution_found = True
                    self.settings.max_literals = hypothesis_size - 1
                    min_coverage = self.settings.min_coverage = 2

                    for i in range(self.settings.max_literals + 1, self.settings.max_size + 1):
                        print('MOO_generator.prune_size', i)
                        # generator.prune_size(i)

                call_combine = not self.uncovered.any()

        if call_combine:
            if self.settings.noisy:
                self.filter_combine_programs(self.to_combine)

            with self.settings.stats.duration('combine'):
                is_new_solution_found = self.combiner.update_best_prog(self.to_combine)

            # to_combine = set()
            self.to_combine.clear()

            return is_new_solution_found

            # new_hypothesis_found = is_new_solution_found is not None

            # if new_hypothesis_found:
            #     new_hypothesis, conf_matrix = is_new_solution_found


    def decide_whether_to_combine(self, prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed,add_gen, tp, fp, fn, pruned_more_general, skipped, skip_early_neg, is_recursive, has_invention):
        add_to_combiner = False
        if self.settings.noisy and (not skipped) and (not skip_early_neg) and (not is_recursive) and (not has_invention) and tp > prog_size + fp and fp + prog_size < self.settings.best_mdl and (not noisy_subsumed):
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
                assert k in (self.combiner.saved_progs | self.to_combine)
                if k in self.combiner.saved_progs:
                    self.combiner.saved_progs.remove(k)
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
                        elif prog2_hash in self.combiner.saved_progs:
                            self.combiner.saved_progs.remove(prog2_hash)
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
        xs = self.combiner.saved_progs | to_combine_set
        min_sz = min(self.cached_prog_size[prog] for prog in xs)
        must_beat = self.settings.best_mdl - min_sz

        to_delete = set()
        for prog_hash in xs:
            size, tp, fp = self.scores[prog_hash]
            if fp + size >= must_beat:
                to_delete.add(prog_hash)

        for prog_hash in to_delete:
            if prog_hash in self.combiner.saved_progs:
                self.combiner.saved_progs.remove(prog_hash)
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
        self.settings.last_combine_stage = False

        return Combiner(self.settings, self.tester, self.coverage_pos, self.coverage_neg, self.prog_lookup)