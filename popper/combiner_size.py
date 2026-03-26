# Code and ideas from the papers:

# Andrew Cropper, Céline Hocquette:
# Learning Logic Programs by Combining Programs. ECAI 2023: 501-508

# Céline Hocquette, Andreas Niskanen, Matti Järvisalo, Andrew Cropper:
# Learning MDL Logic Programs from Noisy Data. AAAI 2024: 10553-10561

# maxsat code originally written by Andreas Niskanen (andreas.niskanen@helsinki.fi)
from collections import defaultdict
from . import maxsat
from pysat.formula import IDPool
import itertools
from . import stats
from . import logger
from bitarray.util import subset, any_and, ones
from . util import rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_prog, reduce_prog, calc_rule_size

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

        self.load_solver()

    def combine(self, prog, prog_size, test_result, size_change, add_to_combiner, last_combine_stage=False):
        state = self.state
        pos_covered = test_result.pos_covered

        if add_to_combiner:
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

        call_combine = len(self.to_combine) > 0 and state.solution_found and (len(self.to_combine) >= self.settings.batch_size or size_change)

        combine_result1 = None
        if add_to_combiner and not state.solution_found:
            if any_and(state.uncovered, pos_covered):
                if state.best_hypothesis:
                    tmp = state.best_hypothesis | prog
                else:
                    tmp = prog
                state.uncovered &= ~pos_covered
                fn = state.uncovered.count(1)
                tp = self.tester.num_pos - fn
                hypothesis_size = calc_prog_size(tmp)
                combine_result1 = tmp, hypothesis_size, (tp, fn, self.tester.num_neg, 0)
                call_combine = fn == 0

        if call_combine:
            with stats.duration('combine'):
                combine_result2 = self.update_best_prog(last_combine_stage=last_combine_stage)

            self.to_combine.clear()

            if combine_result2:
                return combine_result2
        return combine_result1


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

    def load_solver(self):
        if self.settings.debug:
            logger.debug(f'Load exact solver: {self.settings.solver}')

        if self.settings.solver not in ['rc2', 'uwr']:
            print('INVALID SOLVER')
            exit()

        self.settings.maxsat_timeout = None

        if self.settings.solver == 'rc2':
            self.settings.exact_maxsat_solver = 'rc2'
            self.settings.old_format = False
        elif self.settings.solver == 'uwr':
            self.settings.exact_maxsat_solver='uwrmaxsat'
            self.settings.exact_maxsat_solver_params="-v0 -no-sat -no-bin -m -bm"
            self.settings.old_format = False

        self.settings.lex = True
        self.settings.lex_via_weights = False

        if self.settings.debug:
            logger.debug(f'Load anytime solver:{self.settings.anytime_solver}')

        if self.settings.anytime_solver in ['nuwls']:
            self.settings.maxsat_timeout = self.settings.anytime_timeout
            if self.settings.anytime_solver == 'nuwls':
                self.settings.anytime_maxsat_solver = 'NuWLS-c'
                self.settings.anytime_maxsat_solver_params = ""
                self.settings.anytime_maxsat_solver_signal = 15
            else:
                print('INVALID ANYTIME SOLVER')
                exit()

    def add_inconsistent(self, prog_hash):
        self.inconsistent.add(prog_hash)

    def find_combination_norec(self, last_combine_stage=False):
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

            # Populate soft constraints instantly
            rule_soft_lits.append(-k)
            weights.append(size)

        # HARD CONSTRAINT: Force the solver to cover every positive example
        for ex in range(self.tester.num_pos):
            cov_clause = rules_covering_pos_example.get(ex)
            if not cov_clause:
                return [], False  # Trivially UNSAT: An example is completely uncovered
            encoding.append(cov_clause)

        # SOFT CONSTRAINT: Minimise the total size of the selected rules
        soft_lit_groups = [rule_soft_lits]

        timeout = self.settings.maxsat_timeout

        if timeout is None or last_combine_stage:
            _, model = maxsat.exact_lex_solve(encoding, soft_lit_groups, weights, self.settings)
        else:
            _, model = maxsat.anytime_lex_solve(encoding, soft_lit_groups, weights, self.settings, timeout)

        if model is None:
            return [], False

        # Calculate the size of the model the sat solver just found
        best_size = sum(ruleid_to_size[var] for var in model if var > 0)

        # Ensure this new model is strictly better than global best
        size_ = self.state.best_hypothesis_size if self.state.best_hypothesis_score else float('inf')
        if size_ <= best_size:
            return [], False

        # It's strictly better! Extract the rules and return them
        best_prog = [ruleid_to_rule[var] for var in model if var > 0]

        return best_prog, best_size

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

        # SOFT CONSTRAINT: Minimise the size of the selected rules
        soft_lit_groups = [[lit for lit in rule_soft_lits]]

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

        timeout = self.settings.maxsat_timeout
        while True:
            # Replaced unused `cost` variable with `_`
            if timeout is None or last_combine_stage:
                _, model = maxsat.exact_lex_solve(encoding, soft_lit_groups, weights, self.settings)
            else:
                _, model = maxsat.anytime_lex_solve(encoding, soft_lit_groups, weights, self.settings, timeout)

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
            return

        if self.settings.recursion_enabled:
            new_solution, size = self.find_combination(last_combine_stage)
        else:
            new_solution, size = self.find_combination_norec(last_combine_stage)

        if len(new_solution) == 0:
            return

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

        return new_solution, size, (tp, fn, tn, fp)
