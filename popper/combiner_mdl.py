# Code and ideas from the papers:

# Andrew Cropper, Céline Hocquette:
# Learning Logic Programs by Combining Programs. ECAI 2023: 501-508

# Céline Hocquette, Andreas Niskanen, Matti Järvisalo, Andrew Cropper:
# Learning MDL Logic Programs from Noisy Data. AAAI 2024: 10553-10561

from collections import defaultdict
from . import maxsat
from ortools.sat.python import cp_model
from pysat.formula import IDPool
from . import stats
from . import logger
from bitarray.util import subset, any_and, ones, zeros, count_and, count_or
from . util import rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_prog, reduce_prog, calc_rule_size, print_incomplete_solution2
from . state import update_best_hypothesis

POS_EXAMPLE_WEIGHT = 1
NEG_EXAMPLE_WEIGHT = 1

class SetCoverProgressPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self, rule_vars, fn_vars, fp_vars, num_pos, num_neg,
                 ruleid_to_rule, ruleid_to_size, settings, state):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.rule_vars = rule_vars
        self.fn_vars = fn_vars
        self.fp_vars = fp_vars
        self.num_pos = num_pos
        self.num_neg = num_neg
        self.ruleid_to_rule = ruleid_to_rule
        self.ruleid_to_size = ruleid_to_size # Needed for size calculation
        self.settings = settings
        self.state = state

    def on_solution_callback(self):
        # 1. Calculate Complexity (Size) directly from variables
        # We sum the size of every rule where the Boolean variable is True
        current_hypothesis_size = sum(
            self.ruleid_to_size[k]
            for k, var in self.rule_vars.items()
            if self.Value(var)
        )

        # 2. Extract Error Counts
        fn_count = sum(self.Value(fn) for fn in self.fn_vars)
        fp_count = sum(self.Value(fp) for fp in self.fp_vars)
        tp_count = self.num_pos - fn_count
        tn_count = self.num_neg - fp_count

        # 4. Reconstruct Hypothesis
        hypothesis = [self.ruleid_to_rule[k] for k, var in self.rule_vars.items() if self.Value(var)]

        update_best_hypothesis(
            self.settings,
            self.state,
            hypothesis,
            current_hypothesis_size,
            (tp_count, fn_count, tn_count, fp_count),
        )



class OptPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self, rule_vars, fn_vars, fp_vars, num_pos, num_neg,
                 ruleid_to_rule, ruleid_to_size, state):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.rule_vars = rule_vars
        self.fn_vars = fn_vars
        self.fp_vars = fp_vars
        self.num_pos = num_pos
        self.num_neg = num_neg
        self.ruleid_to_rule = ruleid_to_rule
        self.ruleid_to_size = ruleid_to_size # Needed for size calculation
        self.state = state
        self.best_hash = hash(frozenset(state.best_hypothesis))

    def on_solution_callback(self):
   # 4. Reconstruct Hypothesis
        hypothesis = frozenset([
            self.ruleid_to_rule[k]
            for k, var in self.rule_vars.items()
            if self.Value(var)
        ])

        if hash(hypothesis) == self.best_hash:
            return

        # 1. Calculate Complexity (Size) directly from variables
        # We sum the size of every rule where the Boolean variable is True
        current_hypothesis_size = sum(
            self.ruleid_to_size[k]
            for k, var in self.rule_vars.items()
            if self.Value(var)
        )

        # 2. Extract Error Counts
        fn_count = sum(self.Value(fn) for fn in self.fn_vars)
        fp_count = sum(self.Value(fp) for fp in self.fp_vars)
        tp_count = self.num_pos - fn_count
        tn_count = self.num_neg - fp_count

        # 3. Objective Value (Total MDL)
        # current_cost should be equal to (current_hypothesis_size + fn_count + fp_count)
        current_cost = self.ObjectiveValue()

        # 5. Update State
        # self.state.best_hypothesis_score = (tp_count, fn_count, tn_count, fp_count)
        # self.state.best_hypothesis_size = current_hypothesis_size
        # self.state.best_hypothesis = hypothesis
        # self.state.best_hypothesis_mdl = current_cost
        # print("OPT")
        print_incomplete_solution2(hypothesis, current_hypothesis_size, (tp_count, fn_count, tn_count, fp_count))

class CombinerMDL:

    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state

        # a positive example is covered by a hypothesis_hash
        self.covered_by_pos = defaultdict(set)

        # a negative example is covered by a hypothesis_hash
        self.covered_by_neg = defaultdict(set)

        # maps hypothesis_hash:int -> pos_covered:bitarray
        self.coverage_pos = {}

        # maps hypothesis_hash:int -> neg_covered:bitarray
        self.coverage_neg = {}

        # maps hypothesis_hash:int -> hypothesis
        self.prog_lookup = {}

        # dunno
        self.scores = {}

        # stores hypothesis hashes that can be added to the combiner
        # @AC, why this and prog_lookup?
        # prog_lookup.keys() == self.saved_progs?
        self.saved_progs = set()

        # temp store of hypothesis hashes until we need call combine
        self.to_combine = set()

        self.inconsistent = set()

        # self.load_solver()

    def add_prog(self, prog, prog_size, test_result):
        if self.decide_whether_to_combine(prog, prog_size, test_result):
            state = self.state
            
            # MOVE SOMEWHERE ELSE LATER
            if state.min_size is None:
                state.min_size = prog_size
            self.to_combine.add(hash(prog))

    def _remove_prog(self, prog_hash):
        if prog_hash in self.saved_progs:
            self.saved_progs.remove(prog_hash)
        else:
            self.to_combine.remove(prog_hash)

        k_pos = self.coverage_pos.pop(prog_hash)
        k_neg = self.coverage_neg.pop(prog_hash)

        self.state.success_sets_noise.discard((k_pos, k_neg))
        if self.state.success_sets.pop(k_pos, None) is not None:
            self.state.success_sets_version += 1

        for ex in k_pos.search(1):
            self.covered_by_pos[ex].remove(prog_hash)
        for ex in k_neg.search(1):
            self.covered_by_neg[ex].remove(prog_hash)

        del self.scores[prog_hash]
        del self.prog_lookup[prog_hash]

    def combine(self, size_change, last_combine_stage=False):        
        
        call_combine = len(self.to_combine) > 0 and (len(self.to_combine) >= self.settings.batch_size or size_change)
        
        if self.settings.recursion_enabled:
            call_combine = len(self.to_combine) > 0

        if not call_combine:
            return False
        
        self.filter_combine_programs(self.to_combine)

        with stats.duration('combine'):
            combine_result = self.update_best_prog(last_combine_stage=last_combine_stage)
        self.to_combine.clear()
        
        return combine_result

    def build_incompatibility(self):
        """
        Computes pairwise rule incompatibility based on MDL gain.
        incompatible[h] = set of rule hashes that should not be selected together.
        """

        incompatible = defaultdict(set)

        progs = list(self.saved_progs)

        for i in range(len(progs)):
            h1 = progs[i]
            pos1 = self.coverage_pos[h1]
            neg1 = self.coverage_neg[h1]
            size1, tp1, fp1 = self.scores[h1]

            gain1 = tp1 - fp1 - size1

            for j in range(i + 1, len(progs)):
                h2 = progs[j]
                pos2 = self.coverage_pos[h2]
                neg2 = self.coverage_neg[h2]
                size2, tp2, fp2 = self.scores[h2]

                gain2 = tp2 - fp2 - size2

                # --- UNION COVERAGE ---
                tp_union = count_or(pos1, pos2)
                fp_union = count_or(neg1, neg2)
                size_union = size1 + size2

                gain_union = tp_union - fp_union - size_union

                # --- BASIC INCOMPATIBILITY CONDITION ---
                if gain_union <= max(gain1, gain2):
                    # print('poo', h1, h2)
                    incompatible[h1].add(h2)
                    incompatible[h2].add(h1)

        return incompatible

    def decide_whether_to_combine(self, prog, prog_size, test_result):
        pos_covered, neg_covered = test_result.pos_covered, test_result.neg_covered
        tp, fn, fp, tn =  test_result.tp, test_result.fn, test_result.fp, test_result.tn
        inconsistent = test_result.inconsistent

        best_mdl = self.state.best_hypothesis_mdl if self.state.best_hypothesis_mdl else float('inf')
        if (fp+ prog_size) >= best_mdl:
            print(fp + prog_size, best_mdl, format_prog(prog))
            assert(False)
        # IF A RULE COSTS MORE (SIZE + ERRORS) THAN THE POSITIVES IT COVERS, IT IS DEAD WEIGHT
        if tp <= (fp + prog_size):
            assert(False)

        local_delete = set()

        # check whether we have already seen a program that covers these positives and negatives
        ignore_this_prog = (pos_covered, neg_covered) in self.state.success_sets_noise

        # check whether new program is dominated by an older one
        # old_pos ⊇ new_pos, old_neg ⊆ new_neg, old_size <= new_size
        if not ignore_this_prog:
            # find programs that cover at least the same pos examples as new prog
            s_pos = set.intersection(*(self.covered_by_pos[ex] for ex in pos_covered.search(1)))

            for old_prog in s_pos:
                n1 = self.coverage_neg[old_prog]
                # check whether old program covers a subset of the negative examples
                if subset(n1, neg_covered):
                    # we can skip checking size because programs are considered in size order.
                    ignore_this_prog = True
                    break

        # find programs that cover at least the same neg examples as new prog
        # new_pos ⊇ old_pos, new_neg ⊆ old_neg, new_size <= old_size
        if not ignore_this_prog and (inconsistent or fp > 0):
            s_neg = set.intersection(*(self.covered_by_neg[ex] for ex in neg_covered.search(1)))

            for old_prog in s_neg:
                # check whether old program covers a subset of the pos examples covered by the new prog
                if subset(self.coverage_pos[old_prog], pos_covered):
                    size1, tp1, fp1 = self.scores[old_prog]

                    # if old prog is the same size then we can ignore the old prog
                    if size1 == prog_size:
                        local_delete.add(old_prog)
                        continue

                    # @AC, what is this? is it sound?
                    if (tp - fp - prog_size) <= (tp1 - fp1 - size1):
                        ignore_this_prog = True
                        break

                    # # print('ASDA1') OLD PROGRAM DOMINATES NEW PROGRAM ON SAME NEGATIVE COVERAGE
                    # # same neg => same fp, so compare tp and size
                    # if tp1 >= tp and size1 <= prog_size:
                    #     print('ASDA1')
                    #     ignore_this_prog = True
                    #     break

                    # # print('ASDA2') NEW PROGRAM DOMINATES OLD PROGRAM ON SAME NEGATIVE COVERAGE
                    # if tp >= tp1 and prog_size <= size1:
                    #     print('ASDA2')
                    #     local_delete.add(old_prog)

        # “If the new program is at least as good as an old one on positive coverage, and is no worse in size and FP, then delete the old one.”
        if not inconsistent:
            # find examples not covered by this new prog
            not_covered = self.tester.pos_examples_ ^ pos_covered

            # find all old programs that are not subsumed by the new program
            progs_not_subsumed = set.union(*(self.covered_by_pos[ex] for ex in not_covered.search(1)))

            all_progs = set.union(*(self.covered_by_pos[ex] for ex, ex_cov in enumerate(self.tester.pos_examples_) if ex_cov == 1))

            s_pos2 = all_progs.difference(progs_not_subsumed)
            for prog1 in s_pos2:
                size1, tp1, fp1 = self.scores[prog1]
                # if size1 >= prog_size:
                    # local_delete.add(prog1)

                # --- ADDITION 3: SAFE SUBSUMPTION ---
                # Only delete if new program is smaller/same AND no dirtier (FP).
                # This prevents a small "dirty" rule from killing a larger "perfect" rule.
                if size1 >= prog_size and fp1 >= fp:
                    local_delete.add(prog1)

        # if local_delete or ignore_this_prog:
        #     print('OLD', local_delete, ignore_this_prog)

        for k in local_delete:
            # assert(not ignore_this_prog)
            self._remove_prog(k)

        if not ignore_this_prog:
            self.state.success_sets_noise.add((pos_covered, neg_covered))
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
            self.scores[k] = (prog_size, tp, fp)
            self.prog_lookup[k] = prog

            if fp == 0:
                self.state.success_sets[pos_covered] = prog_size
                self.state.success_sets_version += 1

            return not ignore_this_prog

    def filter_combine_programs(self, to_combine_set):
        xs = self.saved_progs | to_combine_set
        min_sz = min(self.scores[prog][0] for prog in xs)
        must_beat = self.state.best_hypothesis_mdl - min_sz

        to_delete = set()
        for prog_hash in xs:
            size, tp, fp = self.scores[prog_hash]
            if fp + size >= must_beat:
                to_delete.add(prog_hash)
                continue

        for prog_hash in to_delete:
            self._remove_prog(prog_hash)

    def add_inconsistent(self, prog_hash):
        self.inconsistent.add(prog_hash)

    def find_combination_norec_maxsat(self, last_combine_stage=False):
        encoding = []
        ruleid_to_rule = {}
        ruleid_to_size = {}

        num_pos = self.tester.num_pos
        num_neg = self.tester.num_neg

        # Pre-allocated arrays completely bypass dictionary hashing overhead
        rules_covering_pos = [[] for _ in range(num_pos)]
        rules_covering_neg = [[] for _ in range(num_neg)]

        soft_clauses = []
        weights = []

        # VARIABLE OFFSET MATH:
        # Rules get IDs 1 to N. Pos examples start after N. Neg examples start after Pos.
        N = len(self.saved_progs)
        pos_var = lambda ex: N + 1 + ex
        neg_var = lambda ex: N + num_pos + 1 + ex


        # tmp_dump = {}

        # 1. PARSE RULES & POPULATE SIZES/COVERAGE
        for k, prog_hash in enumerate(self.saved_progs, start=1):
            prog = self.prog_lookup[prog_hash]
            rule = next(iter(prog))

            # tmp_dump[k] = prog

            size = calc_rule_size(rule)
            ruleid_to_rule[k] = rule
            ruleid_to_size[k] = size

            for ex in self.coverage_pos[prog_hash].search(1):
                rules_covering_pos[ex].append(k)

            for ex in self.coverage_neg[prog_hash].search(1):
                rules_covering_neg[ex].append(k)

            # Soft clause: Reward the solver for NOT picking the rule (minimises size)
            soft_clauses.append([-k])
            weights.append(size)

        # 2. ENCODE POSITIVE EXAMPLES (Minimise FN)
        for ex in range(num_pos):
            pvar = N + 1 + ex

            # Hard clause: If example is covered (pvar), at least one covering rule MUST be true
            encoding.append([-pvar] + rules_covering_pos[ex])

            # if len(rules_covering_pos[ex]) == 1:
            #     h = tmp_dump[rules_covering_pos[ex][0]]
            #     print(format_prog(h))
            #     # pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
            #     # tp_ = self.tester.num_pos - fn
            #     # tn_ = self.tester.num_neg - fp
            #     # size, tp, fp = self.scores[h]
            #     # print(f'asda price ex:{ex} h:{h} size:{size} tp:{tp} fp:{fp}')


            # Soft clause: Reward the solver for covering the positive example
            soft_clauses.append([pvar])
            weights.append(POS_EXAMPLE_WEIGHT)

        # 3. ENCODE NEGATIVE EXAMPLES (Minimise FP)
        for ex in range(num_neg):
            nvar = neg_var(ex)
            # Hard clause: If ANY covering rule is true, the negative example MUST be covered (nvar)
            for rule_k in rules_covering_neg[ex]:
                encoding.append([nvar, -rule_k])

            # Soft clause: Reward the solver for NOT covering the negative example
            soft_clauses.append([-nvar])
            weights.append(NEG_EXAMPLE_WEIGHT)

        # if trick:
        #     incompatible = self.build_incompatibility()

        #     # hash_to_ruleid = {}
        #     for k, prog_hash in enumerate(self.saved_progs, start=1):
        #         hash_to_ruleid[prog_hash] = k

        #     for h1, bad_set in incompatible.items():
        #         r1 = hash_to_ruleid[h1]
        #         for h2 in bad_set:
        #             r2 = hash_to_ruleid[h2]
        #             if r1 < r2:
        #                 encoding.append([-r1, -r2])  # hard clause

        if last_combine_stage or not self.settings.nuwls:
            _, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights)
        else:
            _, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings.anytime_timeout)

        if model is None:
            return [], False

        selected = [k for k in ruleid_to_rule if model[k-1] > 0]

        best_size = sum(ruleid_to_size[k] for k in selected)

        # Count FNs: Check the pos_var range in the model
        fn = sum(1 for ex in range(num_pos) if model[N + ex] < 0)

        # Count FPs: Check the neg_var range in the model
        fp = sum(1 for ex in range(num_neg) if model[N + num_pos + ex] > 0)

        # Calculate the actual MDL Score
        mdl = (POS_EXAMPLE_WEIGHT * fn) + (NEG_EXAMPLE_WEIGHT * fp) + best_size
        mdl_ = self.state.best_hypothesis_mdl if self.state.best_hypothesis_mdl else float('inf')

        # Ensure this new model is strictly better than the global best
        if mdl_ <= mdl:
            return [], False

        best_prog = [ruleid_to_rule[k] for k in selected]

        # Returns the raw fn + fp + size as requested in your snippet
        return best_prog, mdl

    def find_combination_norec_cp(self, last_combine_stage=False):
        model, rule_vars, fn_vars, fp_vars, ruleid_to_rule, ruleid_to_size, total_mdl_expr = self._build_mdl_model()

        mdl_limit = self.state.best_hypothesis_mdl if self.state.best_hypothesis_mdl else float('inf')
        if mdl_limit != float('inf'):
            model.Add(total_mdl_expr < int(mdl_limit))

        model.Minimize(total_mdl_expr)

        if self.state.best_hypothesis:
            best_set = {hash(r) for r in self.state.best_hypothesis}
            for k, var in rule_vars.items():
                rule = ruleid_to_rule[k]
                model.AddHint(var, 1 if hash(rule) in best_set else 0)

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1
        solver.parameters.linearization_level = 2

        if not last_combine_stage:
            solver.parameters.max_time_in_seconds = self.settings.anytime_timeout

        printer = SetCoverProgressPrinter(
            rule_vars=rule_vars,
            fn_vars=fn_vars,
            fp_vars=fp_vars,
            num_pos=self.tester.num_pos,
            num_neg=self.tester.num_neg,
            ruleid_to_rule=ruleid_to_rule,
            ruleid_to_size=ruleid_to_size,
            settings=self.settings,
            state=self.state,
        )

        status = solver.Solve(model, printer)

        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            return [], False

        mdl = solver.ObjectiveValue()
        if mdl_limit <= mdl:
            return [], False

        best_prog = [ruleid_to_rule[k] for k, var in rule_vars.items() if solver.Value(var)]
        return best_prog, mdl


    def _enumerate_all_optimal(self):
        model, rule_vars, fn_vars, fp_vars, ruleid_to_rule, ruleid_to_size, total_mdl_expr = self._build_mdl_model()

        optimal_mdl = self.state.best_hypothesis_mdl if self.state.best_hypothesis_mdl else float('inf')
        if optimal_mdl == float('inf'):
            assert(False)
        model.Add(total_mdl_expr == int(optimal_mdl))

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1
        solver.parameters.linearization_level = 2
        solver.parameters.enumerate_all_solutions = True

        printer = OptPrinter(
            rule_vars=rule_vars,
            fn_vars=fn_vars,
            fp_vars=fp_vars,
            num_pos=self.tester.num_pos,
            num_neg=self.tester.num_neg,
            ruleid_to_rule=ruleid_to_rule,
            ruleid_to_size=ruleid_to_size,
            state=self.state,
        )

        status = solver.Solve(model, printer)

        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            return [], False

        best_prog = [ruleid_to_rule[k] for k, var in rule_vars.items() if solver.Value(var)]
        return best_prog, solver.ObjectiveValue()


    def _build_mdl_model(self):
        """Builds the CP-SAT model and returns it along with all supporting data structures."""
        num_pos = self.tester.num_pos
        num_neg = self.tester.num_neg
        N = len(self.saved_progs)

        ruleid_to_rule = {}
        ruleid_to_size = {}
        rules_covering_pos = [[] for _ in range(num_pos)]
        rules_covering_neg = [[] for _ in range(num_neg)]

        for k, prog_hash in enumerate(self.saved_progs, start=1):
            prog = self.prog_lookup[prog_hash]
            rule = next(iter(prog))
            ruleid_to_rule[k] = rule
            ruleid_to_size[k] = calc_rule_size(rule)

            for ex in self.coverage_pos[prog_hash].search(1):
                rules_covering_pos[ex].append(k)
            for ex in self.coverage_neg[prog_hash].search(1):
                rules_covering_neg[ex].append(k)

        model = cp_model.CpModel()
        rule_vars = {k: model.NewBoolVar(f'r_{k}') for k in range(1, N + 1)}
        fn_vars = [model.NewBoolVar(f'fn_{i}') for i in range(num_pos)]
        fp_vars = [model.NewBoolVar(f'fp_{j}') for j in range(num_neg)]

        for i in range(num_pos):
            rules = [rule_vars[k] for k in rules_covering_pos[i]]
            model.AddBoolOr(rules + [fn_vars[i]])

        for j in range(num_neg):
            for r_var in [rule_vars[k] for k in rules_covering_neg[j]]:
                model.AddImplication(r_var, fp_vars[j])

        objective_terms = (
            [rule_vars[k] * ruleid_to_size[k] for k in range(1, N + 1)]
            + fn_vars
            + fp_vars
        )
        total_mdl_expr = cp_model.LinearExpr.Sum(objective_terms)

        return model, rule_vars, fn_vars, fp_vars, ruleid_to_rule, ruleid_to_size, total_mdl_expr

    # OLD!
    # NEEDS REFACTORING
    def find_combination(self, last_combine_stage=False):
        # print('')
        # print(f'lex:{self.settings.lex}')
        # print(f'best_mdl:{self.state.best_hypothesis_mdl}')
        # print(f'best_prog_score:{self.state.best_hypothesis_score}')
        # print(f'best_prog_size:{self.state.best_hypothesis_size}')

        # timeout = self.settings.maxsat_timeout
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

        for ex in pos_index:
            encoding.append([-pos_example_covered_var[ex]] + [program_var[p] for p in programs_covering_pos_example[ex]])

        for ex in neg_index:
            for p in programs_covering_neg_example[ex]:
                encoding.append([neg_example_covered_var[ex], -program_var[p]])

        soft_clauses = []
        weights = []

        # if self.state.best_hypothesis_score:
        #     tp_, fn_, tn_, fp_ = self.state.best_hypothesis_score
        #     size_ = self.state.best_hypothesis_size

        for rule_id in rule_var:
            if rule_var[rule_id] is not None:
                soft_clauses.append([-rule_var[rule_id]])
                weights.append(ruleid_to_size[rule_id])
        for i in pos_index:
            soft_clauses.append([pos_example_covered_var[i]])
            weights.append(POS_EXAMPLE_WEIGHT)

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

        if self.state.best_hypothesis_mdl:
            mdl_ = self.state.best_hypothesis_mdl

        while True:
            model_found = False

            if last_combine_stage:
                _, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights)
            else:
                _, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings.anytime_timeout)

            if model is None:
                print("WARNING: No solution found, exit combiner.")
                break

            fn = sum(1 for i in pos_index if model[pos_example_covered_var[i]-1] < 0)
            fp = sum(1 for i in neg_index if model[neg_example_covered_var[i]-1] > 0)
            size = sum([ruleid_to_size[rule_id] for rule_id in ruleid_to_size if model[rule_var[rule_id]-1] > 0])

            if self.state.best_hypothesis_score:
                if mdl_ <= POS_EXAMPLE_WEIGHT * fn + NEG_EXAMPLE_WEIGHT * fp + size:
                    break

            model_found = True

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

                if not self.tester.test_prog_inconsistent(model_prog):
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
        return best_prog, best_fn + best_fp + best_size

    def greedy_upper_bound(self):
        covered_pos = zeros(self.tester.num_pos)
        covered_neg = zeros(self.tester.num_neg)

        total_size = 0
        candidates = list(self.saved_progs)

        out = []
        while candidates:
            best = None
            best_delta = 0  # only accept strictly improving moves

            for h in candidates:
                new_tp = count_and(~covered_pos, self.coverage_pos[h])
                new_fp = count_and(~covered_neg, self.coverage_neg[h])
                size, tp, fp = self.scores[h]

                delta = size + new_fp - new_tp

                if best is None or delta < best_delta:
                    best = h
                    best_delta = delta

            if best is None or best_delta >= 0:
                break

            size, tp, fp = self.scores[best]
            total_size += size
            covered_pos |= self.coverage_pos[best]
            covered_neg |= self.coverage_neg[best]
            candidates.remove(best)
            out.append(best)

        fn = self.tester.num_pos - covered_pos.count(1)
        fp = covered_neg.count(1)

        mdl_limit = self.state.best_hypothesis_mdl if self.state.best_hypothesis_mdl else float('inf')
        prog_mdl = int(total_size + fp + fn)

        if prog_mdl >= mdl_limit:
            return False

        prog = []
        for prog_hash in out:
            for rule in self.prog_lookup[prog_hash]:
                prog.append(rule)

        # pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        tp = self.tester.num_pos - fn
        tn = self.tester.num_neg - fp

        update_best_hypothesis(
            self.settings,
            self.state,
            prog,
            total_size,
            (tp, fn, tn, fp),
        )
        logger.info(f'New bound from greedy search: {prog_mdl}')

        return True

    def update_best_prog(self, last_combine_stage=False):
        new_progs = self.to_combine

        self.saved_progs.update(new_progs)
        if not self.saved_progs:
            return False

        new_hyp_found = False

        if self.settings.recursion_enabled:
            new_solution, cost = self.find_combination(last_combine_stage)
        else:
            # do a quick greedy search to determine an upperbound
            new_hyp_found = self.greedy_upper_bound()

            if not self.settings.nuwls:
                if last_combine_stage:
                    logger.info(f'Calling CP solver for final noisy combine stage with {len(self.saved_progs)} rules')
                new_solution, cost = self.find_combination_norec_cp(last_combine_stage)
                if last_combine_stage and self.settings.all_opt:
                    logger.info(f'Calling CP solver to find all optimal hypotheses')
                    self._enumerate_all_optimal()
            else:
                if last_combine_stage:
                    logger.info(f'Calling MaxSAT solver for final noisy combine stage with {len(self.saved_progs)} rules')
                new_solution, cost = self.find_combination_norec_maxsat(last_combine_stage)

        if len(new_solution) == 0:
            return new_hyp_found

        if cost > self.state.best_hypothesis_mdl:
            assert(False)

        new_solution = reduce_prog(new_solution)
        pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        tp = pos_covered.count(1)
        fp = neg_covered.count(1)
        tn = self.tester.num_neg - fp
        fn = self.tester.num_pos - tp
        size = calc_prog_size(new_solution)

        update_best_hypothesis(self.settings, self.state, new_solution, size, (tp, fn, tn, fp))
        return True
