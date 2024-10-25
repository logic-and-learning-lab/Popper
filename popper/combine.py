# code originally written by Andreas Niskanen (andreas.niskanen@helsinki.fi)
from . util import calc_prog_size, reduce_prog, prog_is_recursive, prog_has_invention, calc_rule_size, rule_is_recursive, format_prog
from collections import defaultdict
from . import maxsat
from pysat.formula import IDPool
import time
import bitarray

POS_EXAMPLE_WEIGHT = 1
NEG_EXAMPLE_WEIGHT = 1

class Combiner:
    def __init__(self, settings, tester, coverage_pos, coverage_neg, prog_lookup):
        self.settings = settings
        self.tester = tester
        self.best_cost = None
        self.saved_progs = set()
        self.inconsistent = set()
        self.coverage_pos = coverage_pos
        self.coverage_neg = coverage_neg
        self.prog_lookup = prog_lookup

    def add_inconsistent(self, prog_hash):
        self.inconsistent.add(prog_hash)

    def find_combination(self, timeout):
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
            tp_, fn_, tn_, fp_, size_ = self.settings.best_prog_score

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
            return best_prog, (best_fn, best_fp, best_size)
        return best_prog, best_fn + best_fp + best_size

    def update_best_prog(self, new_progs, timeout=None):
        if timeout is None:
            timeout = self.settings.maxsat_timeout

        self.saved_progs.update(new_progs)

        new_solution, cost = self.find_combination(timeout)

        if len(new_solution) == 0:
            return None

        if self.best_cost is None:
            self.best_cost = cost
        elif cost >= self.best_cost:
            return None

        self.best_cost = cost

        new_solution = reduce_prog(new_solution)
        pos_covered, neg_covered = self.tester.test_prog_all(new_solution)
        tp = pos_covered.count(1)
        fp = neg_covered.count(1)
        tn = self.tester.num_neg - fp
        fn = self.tester.num_pos - tp
        size = calc_prog_size(new_solution)

        return new_solution, (tp, fn, tn, fp, size)
