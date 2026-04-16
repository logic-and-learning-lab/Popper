# Code and ideas from the papers:
# Andrew Cropper, Céline Hocquette: # Learning Logic Programs by Combining Programs. ECAI 2023: 501-508
# Céline Hocquette, Andreas Niskanen, Matti Järvisalo, Andrew Cropper: # Learning MDL Logic Programs from Noisy Data. AAAI 2024: 10553-10561

from collections import defaultdict
from ortools.sat.python import cp_model
from . import maxsat
from pysat.formula import IDPool
from . import stats
from . import logger
from . state import update_best_hypothesis
from bitarray.util import subset, any_and
from . util import rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_prog, reduce_prog, calc_rule_size, print_incomplete_solution2

class SetCoverProgressPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self, rule_vars, num_pos, num_neg,
                 ruleid_to_rule, ruleid_to_size, state):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.rule_vars = rule_vars
        self.num_pos = num_pos
        self.num_neg = num_neg
        self.ruleid_to_rule = ruleid_to_rule
        self.state = state

    def on_solution_callback(self):
        # self.solution_count += 1
        # elapsed_time = time.time() - self.start_time

        hypothesis = [self.ruleid_to_rule[k] for k, var in self.rule_vars.items() if self.Value(var)]
        current_hypothesis_size = int(self.ObjectiveValue())

        # 2. Extract Error Counts
        fn_count = 0
        fp_count = 0
        tp_count = self.num_pos
        tn_count = self.num_neg

        # 5. Update State
        self.state.best_hypothesis_score = (tp_count, fn_count, tn_count, fp_count)
        self.state.best_hypothesis_size = current_hypothesis_size
        self.state.best_hypothesis = hypothesis
        # self.state.best_hypothesis_mdl = current_cost
        print_incomplete_solution2(hypothesis, current_hypothesis_size, (tp_count, fn_count, tn_count, fp_count))

        # print('MOOOOO')


class AllOptPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self, rule_vars, ruleid_to_rule, num_pos, num_neg, state):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.rule_vars = rule_vars
        self.ruleid_to_rule = ruleid_to_rule
        self.num_pos=num_pos
        self.num_neg=num_neg
        self.best_hash = hash(frozenset(state.best_hypothesis))

    def on_solution_callback(self):
        # self.solution_count += 1
        # elapsed_time = time.time() - self.start_time

        hypothesis = frozenset([self.ruleid_to_rule[k] for k, var in self.rule_vars.items() if self.Value(var)])
        # current_hypothesis_size = int(self.ObjectiveValue())
        current_hypothesis_size = calc_prog_size(hypothesis)

        if hash(hypothesis) == self.best_hash:
            return

        # 2. Extract Error Counts
        fn_count = 0
        fp_count = 0
        tp_count = self.num_pos
        tn_count = self.num_neg
        print('OPTTTTTTT')

        print_incomplete_solution2(hypothesis, current_hypothesis_size, (tp_count, fn_count, tn_count, fp_count))


class CombinerSize:

    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state

        # a positive example is covered by a hypothesis_hash
        self.covered_by_pos = defaultdict(set)

        # maps hypothesis_hash:int -> pos_covered:bitarray
        self.coverage_pos = {}

        # maps hypothesis_hash:int -> hypothesis_size:int
        self.cached_prog_size = {}

        # maps hypothesis_hash:int -> hypothesis
        self.prog_lookup = {}

        # maps pos_covered:bitarray -> hypothesis_hash
        self.success_sets_aux = {}

        # stores hypothesis hashes that can be added to the combiner
        # @AC, why this and prog_lookup?
        # prog_lookup.keys() == self.saved_progs?
        self.saved_progs = set()

        # temp store of hypothesis hashes until we need call combine
        self.to_combine = set()

        self.inconsistent = set()

        # self.load_solver()

    def add_prog(self, prog, prog_size, test_result):
        state = self.state
        pos_covered = test_result.pos_covered
        
        self.filter_combine_programs(prog, prog_size, pos_covered)

        # UPDATE STATE
        # MOVE SOMEWHERE ELSE LATER
        if state.min_size is None:
            state.min_size = prog_size

        k = hash(prog)
        state.success_sets[pos_covered] = prog_size
        self.success_sets_aux[pos_covered] = k
        self.coverage_pos[k] = pos_covered
        self.prog_lookup[k] = prog

        for p, s in self.state.success_sets.items():
            if p == pos_covered:
                continue
            self.state.paired_success_sets[s + prog_size].add(p | pos_covered)

        self.to_combine.add(hash(prog))

        if state.solution_found:
            return False
        
        if any_and(state.uncovered, pos_covered):
            if state.best_hypothesis:
                new_hypothesis = state.best_hypothesis | prog
            else:
                new_hypothesis = prog
            state.uncovered &= ~pos_covered
            fn = state.uncovered.count(1)
            tp = self.tester.num_pos - fn
            new_hypothesis_size = calc_prog_size(new_hypothesis)
            conf_matrix = (tp, fn, self.tester.num_neg, 0)            
            update_best_hypothesis(self.settings, self.state, new_hypothesis, new_hypothesis_size, conf_matrix)

            if fn == 0:
                self._combine(False)
                return True
            
        return False

    def _combine(self, last_combine_stage):
        with stats.duration('combine'):
            x = self.update_best_prog(last_combine_stage=last_combine_stage)
        self.to_combine.clear()
        return x
    
    def combine(self, size_change, last_combine_stage=False):
        call_combine = len(self.to_combine) > 0 and self.state.solution_found and (len(self.to_combine) >= self.settings.batch_size or size_change)
        if call_combine:
            return self._combine(last_combine_stage)
        return False

    # delete programs from the combiner (or the list of programs to be combined) which are worse than this new program
    def filter_combine_programs(self, prog, prog_size, pos_covered):
        if self.settings.recursion_enabled:
            return

        # find previously seen programs where this new one dominates
        to_delete = []
        for pos_covered2, prog_size2 in self.state.success_sets.items():
            if prog_size > prog_size2:
                continue
            if subset(pos_covered2, pos_covered):
                to_delete.append(self.success_sets_aux[pos_covered2])

        # then delete them
        for prog2_hash in to_delete:
            pos_covered2 = self.coverage_pos[prog2_hash]
            if prog2_hash in self.to_combine:
                self.to_combine.remove(prog2_hash)
            elif prog2_hash in self.saved_progs:
                self.saved_progs.remove(prog2_hash)
            else:
                assert False
            del self.state.success_sets[pos_covered2]
            del self.success_sets_aux[pos_covered2]
            del self.coverage_pos[prog2_hash]
            del self.prog_lookup[prog2_hash]

    def add_inconsistent(self, prog_hash):
        self.inconsistent.add(prog_hash)

    # maxsat code to find a good combination of rules
    # assumes no recursion to make it easier to understand
    def find_combination_norec_maxsat(self, last_combine_stage=False):
        encoding = []

        ruleid_to_rule = {}
        ruleid_to_size = {}

        # Maps positive example index -> list of SAT variables covering it
        rules_covering_pos_example = defaultdict(list)

        weights = []
        rule_soft_lits = []

        # k acts as both the rule ID *and* the exact PySAT variable (starting at 1)
        for k, prog_hash in enumerate(self.saved_progs, start=1):
            prog = self.prog_lookup[prog_hash]
            rule = next(iter(prog))
            size = calc_rule_size(rule)
            ruleid_to_rule[k] = rule
            ruleid_to_size[k] = size

            # Map coverage directly to the rule's SAT variable (k)
            for ex in self.coverage_pos[prog_hash].search(1):
                rules_covering_pos_example[ex].append(k)

            # soft constraints
            # why -k?
            rule_soft_lits.append(-k)
            weights.append(size)

        # HARD CONSTRAINT
        # solver must cover every positive example
        for ex in range(self.tester.num_pos):
            cov_clause = rules_covering_pos_example.get(ex)

            if cov_clause is None:
                continue

            # Optimisation: If only one rule covers this example, it's essential.
            if len(cov_clause) == 1:
                encoding.append([cov_clause[0]])
            else:
                encoding.append(cov_clause)

        # SOFT CONSTRAINT: Minimise the total size of the selected rules
        # The base solver expects a list of clauses, so we wrap each literal in a list
        soft_clauses = [[lit] for lit in rule_soft_lits]

        if last_combine_stage or not self.settings.nuwls:
            # call the exact maxsat solver
            _, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights)
        else:
            # call nuwls
            _, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings.anytime_timeout)

        if model is None:
            return [], False

        # the size of the model the sat solver just found
        # best_size = solver.ObjectiveValue()
        best_size = sum(ruleid_to_size[var] for var in model if var > 0)

        # check new model is strictly better than global best
        size_ = self.state.best_hypothesis_size if self.state.best_hypothesis_score else float('inf')
        if size_ <= best_size:
            return [], False

        # get the rules and return them
        best_prog = [ruleid_to_rule[var] for var in model if var > 0]

        return best_prog, best_size

    def find_combination_norec_cp(self, last_combine_stage=False):
        cp_mod, rule_vars, ruleid_to_rule, ruleid_to_size, total_size_expr = self._build_set_cover_model()

        current_best_size = self.state.best_hypothesis_size if self.state.best_hypothesis_score else float('inf')
        if current_best_size != float('inf'):
            cp_mod.Add(total_size_expr < int(current_best_size))

        cp_mod.Minimize(total_size_expr)

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1
        solver.parameters.linearization_level = 2

        if not last_combine_stage:
            solver.parameters.max_time_in_seconds = float(self.settings.anytime_timeout)

        printer = SetCoverProgressPrinter(
            rule_vars=rule_vars,
            num_pos=self.tester.num_pos,
            num_neg=self.tester.num_neg,
            ruleid_to_rule=ruleid_to_rule,
            ruleid_to_size=ruleid_to_size,
            state=self.state,
        )

        status = solver.Solve(cp_mod, printer)

        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            return [], False

        best_size = int(solver.ObjectiveValue())
        if current_best_size <= best_size:
            return [], False

        best_prog = [ruleid_to_rule[k] for k, var in rule_vars.items() if solver.Value(var)]
        return best_prog, best_size


    def _enumerate_all_optimal(self):
        optimal_size = self.state.best_hypothesis_size if self.state.best_hypothesis_score else float('inf')
        if optimal_size == float('inf'):
            assert(False)

        cp_mod, rule_vars, ruleid_to_rule, ruleid_to_size, total_size_expr = self._build_set_cover_model()

        cp_mod.Add(total_size_expr == optimal_size)

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1
        solver.parameters.linearization_level = 2
        solver.parameters.enumerate_all_solutions = True

        printer = AllOptPrinter(
            rule_vars=rule_vars,
            ruleid_to_rule=ruleid_to_rule,
            num_pos=self.tester.num_pos,
            num_neg=self.tester.num_neg,
            state=self.state,
        )

        status = solver.Solve(cp_mod, printer)

        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            return [], False

        best_prog = [ruleid_to_rule[k] for k, var in rule_vars.items() if solver.Value(var)]
        return best_prog, solver.ObjectiveValue()


    def _build_set_cover_model(self):
        """Builds the CP-SAT model and returns it along with all the supporting data structures."""
        cp_mod = cp_model.CpModel()
        ruleid_to_rule = {}
        ruleid_to_size = {}
        rules_covering_pos_example = defaultdict(list)
        rule_vars = {}

        for k, prog_hash in enumerate(self.saved_progs, start=1):
            prog = self.prog_lookup[prog_hash]
            rule = next(iter(prog))
            size = calc_rule_size(rule)
            ruleid_to_rule[k] = rule
            ruleid_to_size[k] = size
            rule_vars[k] = cp_mod.NewBoolVar(f'rule_{k}')
            for ex in self.coverage_pos[prog_hash].search(1):
                rules_covering_pos_example[ex].append(k)

        for ex in range(self.tester.num_pos):
            cov_clause = rules_covering_pos_example.get(ex)
            if cov_clause is None:
                continue
            if len(cov_clause) == 1:
                cp_mod.Add(rule_vars[cov_clause[0]] == 1)
            else:
                cp_mod.AddBoolOr([rule_vars[k] for k in cov_clause])

        total_size_expr = sum(rule_vars[k] * ruleid_to_size[k] for k in rule_vars)

        return cp_mod, rule_vars, ruleid_to_rule, ruleid_to_size, total_size_expr

    # GARBAGE AND NEEDS REFACTORING
    def find_combination(self, last_combine_stage=False):
        encoding = []
        rulehash_to_id = {}
        ruleid_to_rule = {}
        ruleid_to_size = {}
        rule_var = {}
        base_rules = []
        programs_covering_pos_example = defaultdict(list)
        program_var = {}
        vpool = IDPool()

        for program_count, prog_hash in enumerate(self.saved_progs):
            prog = self.prog_lookup[prog_hash]
            pos_covered = self.coverage_pos[prog_hash]

            for ex in pos_covered.search(1):
                programs_covering_pos_example[ex].append(program_count)

            rule_vars = []

            for rule in prog:
                rule_hash = hash(rule)

                # Assign ID if new
                if rule_hash not in rulehash_to_id:
                    k = len(rulehash_to_id) + 1
                    rulehash_to_id[rule_hash] = k
                    ruleid_to_rule[k] = rule
                    ruleid_to_size[k] = calc_rule_size(rule)

                rule_id = rulehash_to_id[rule_hash]

                # Assign SAT var if new
                if rule_id not in rule_var:
                    rule_var[rule_id] = vpool.id(('rule', rule_id))

                rule_vars.append(rule_var[rule_id])

                # Track base rules for recursion constraints
                if self.settings.recursion_enabled:
                    if not rule_is_recursive(rule):
                        if rule_id not in base_rules:
                            base_rules.append(rule_id)

            # Program clauses
            if len(rule_vars) == 1:
                program_var[program_count] = rule_vars[0]
            else:
                program_var[program_count] = vpool.id(('program', program_count))
                pvar = program_var[program_count]

                clause = [pvar] + [-var for var in rule_vars]
                encoding.append(clause)
                for var in rule_vars:
                    clause = [-pvar, var]
                    encoding.append(clause)

        if self.settings.recursion_enabled:
            encoding.append([rule_var[rule_id] for rule_id in base_rules])

        # HARD CONSTRAINT: Force the solver to cover every positive example
        for ex in range(self.tester.num_pos):
            encoding.append([program_var[p] for p in programs_covering_pos_example[ex]])

        weights = []
        rule_soft_lits = []

        for rule_id, r_var in rule_var.items():
            rule_soft_lits.append(-r_var)
            weights.append(ruleid_to_size[rule_id])

        # SOFT CONSTRAINT: Minimise the total size of the selected rules
        # The base solver expects a list of clauses, so we wrap each literal in a list
        soft_clauses = [[lit] for lit in rule_soft_lits]

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
        best_size = False

        # Check existing best size
        size_ = self.state.best_hypothesis_size if self.state.best_hypothesis_score else float('inf')

        while True:

            if last_combine_stage:
                _, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, weights)
            else:
                _, model = maxsat.anytime_maxsat_solve(encoding, soft_clauses, weights, self.settings.anytime_timeout)

            if model is None:
                break

            size = sum([ruleid_to_size[rule_id] for rule_id in ruleid_to_size if model[rule_var[rule_id]-1] > 0])

            # Stop if the new program isn't strictly smaller than what we already have
            if size_ <= size:
                break

            rules = [rule_id for rule_id in ruleid_to_rule if model[rule_var[rule_id]-1] > 0]
            model_prog = [ruleid_to_rule[k] for k in rules]

            # REDUNDANCY FIXED: Flattened the acceptance logic
            if (not prog_is_recursive(model_prog) and not prog_has_invention(model_prog)) or \
               (not self.tester.test_prog_inconsistent(model_prog)):
                best_prog = rules
                best_size = size
                break

            smaller = self.tester.reduce_inconsistent(model_prog)
            ids = [rulehash_to_id[hash(rule)] for rule in smaller]
            clause = [-rule_var[k] for k in ids]
            encoding.append(clause)

        best_prog = [ruleid_to_rule[k] for k in best_prog]

        return best_prog, best_size

    def update_best_prog(self, last_combine_stage=False):
        new_progs = self.to_combine

        self.saved_progs.update(new_progs)
        if not self.saved_progs:
            return False

        if self.settings.recursion_enabled:
            new_solution, size = self.find_combination(last_combine_stage)
        else:
            if not self.settings.nuwls:
                if last_combine_stage:
                    logger.info(f'Calling CP solver for final combine stage with {len(self.saved_progs)} rules')
                new_solution, cost = self.find_combination_norec_cp(last_combine_stage)
                if last_combine_stage and self.settings.all_opt:
                    logger.info(f'Calling CP solver to find all optimal hypotheses')
                    self._enumerate_all_optimal()
            else:
                if last_combine_stage:
                    logger.info(f'Calling MaxSAT solver for final noiseless combine stage with {len(self.saved_progs)} rules')
                new_solution, cost = self.find_combination_norec_maxsat(last_combine_stage)
            size = calc_prog_size(new_solution)

        if len(new_solution) == 0:
            return False

        if self.state.solution_found and size > self.state.best_hypothesis_size:
            assert(False)

        if self.settings.recursion_enabled:
            new_solution = reduce_prog(new_solution)
            pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
            tp = pos_covered.count(1)
            fp = neg_covered.count(1)
            tn = self.tester.num_neg - fp
            fn = self.tester.num_pos - tp
        else:
            tp = self.tester.num_pos
            fp = 0
            tn = self.tester.num_neg
            fn = 0

        update_best_hypothesis(self.settings, self.state, new_solution, size, (tp, fn, tn, fp))

        return True
