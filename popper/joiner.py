# Code and idea from the paper:
# Céline Hocquette, Andreas Niskanen, Rolf Morel, Matti Järvisalo, Andrew Cropper:
# Learning Big Logical Rules by Joining Small Rules. IJCAI 2024: 3430-3438

import time
import collections
from collections import defaultdict
from bitarray import bitarray, frozenbitarray
from bitarray.util import ones
from bitarray.util import subset
from pysat.solvers import Solver
import clingo
from typing import NamedTuple
from functools import reduce
from . tester import TestResult
from . import stats
from . import logger
from ortools.sat.python import cp_model

class Program(NamedTuple):
    rules: tuple
    negated: bool = False

from . util import format_rule, calc_prog_size, format_prog, prog_is_recursive, prog_has_invention, Literal

from . import maxsat
from pysat.formula import IDPool
from pysat.solvers import Solver
from pysat.card import *
from pysat.pb import *
from pysat.formula import WCNF
#from pysat.examples.rc2 import RC2

import time
from pysat.formula import WCNF, IDPool
from pysat.examples.rc2 import RC2, RC2Stratified
from pysat.card import CardEnc, EncType
from bitarray import bitarray


import re


# --- THE ASP LOGIC ---
ASP_SUB_OPTIMAL = """
% ==========================================

#show select/1.
%#show covers_joined/1.
% #show size/2.
% 1. GENERATION (Only guess the independent variables)
% ==========================================
2 { select(P) : program(P) }.

program(P):- size(P,_).
neg_ex(E):- misses_neg(_,E).

% ==========================================
% 2. DETERMINISTIC DERIVATION (No guessing here!)
% ==========================================
% The joined rule misses E if ANY selected program misses E.
misses_joined(E) :- select(P), misses_pos(P, E).

% The joined rule covers E if it is a positive example and does NOT miss it.
covers_joined(E) :- uncovered(E), not misses_joined(E).

% ==========================================
% 3. CONSTRAINTS
% ==========================================
% Must miss all negative examples
:- neg_ex(E), { select(P) : misses_neg(P, E) } 0.

% Datalog constraint
:- datalog_req, head_arg(A), { select(P) : has_arg(P, A) } 0.

% Must cover at least one currently UNCOVERED example
:- { covers_joined(E) : uncovered(E) } 0.

% Prevent supersets of previously selected combinations
% :- prev_combo(Id), { not select(P) : combo_prog(Id, P) } 0.

% ==========================================
% 4. OPTIMIZATION
% ==========================================
% 4. OPTIMIZATION
% Priority 2: Maximise coverage
#maximize { 1@2, E : covers_joined(E) }.

% Priority 1: Minimise (Sum of sizes - num_selected + 1)
#minimize { S-1@1, P : select(P), size(P, S) }.

"""


def get_rule_hash(rule):
    head, body = rule
    head = (head.predicate, head.arguments)
    body = frozenset((literal.predicate, literal.arguments) for literal in body)
    return abs(hash((head, body)))

def get_prog_hash(prog):
    return -abs(sum([get_rule_hash(rule) for rule in prog.rules])) if prog.negated else abs(sum([get_rule_hash(rule) for rule in prog.rules]))

class Joiner:
    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state

        self.head_pred = self.settings.head_literal.predicate
        self.head_args = self.settings.head_literal.arguments

        self.proghash_to_id = {}
        self.progid_to_prog = {}
        self.progid_to_size = {}

        self.existing_consistent = collections.defaultdict(list)

        self.incomplete = set()
        self.example_covered_var = {}
        self.program_selected_var = {}
        self.programs_not_covering_example = {}
        self.programs_covering_example = {}
        self.pos_exs_covered = dict()
        self.neg_exs_covered = dict()
        # self.uncovered = set([x for x in self.pos_index])

        self.vpool = IDPool()

        self.pos_index = list(range(self.tester.num_pos))
        self.neg_index = list(range(self.tester.num_neg))

        self.pos_examples_ = ones(self.tester.num_pos)
        self.neg_examples_ = ones(self.tester.num_neg)

        self.success_sets_joiner = {}
        self.to_join = defaultdict(list)

        for x in range(self.tester.num_pos):
            self.example_covered_var[x] = self.vpool.id("example_covered({0})".format(x))
            self.programs_not_covering_example[x] = set()
            self.programs_covering_example[x] = set()
        self.programs_covering_one_positive = set()

        for x in range(self.tester.num_neg):
            self.programs_not_covering_example[-x-1] = set()
            self.programs_covering_example[-x-1] = set()

        self.settings.non_datalog = False
        if not self.settings.non_datalog:
            self.programs_with_arg = {}
            for x in self.head_args:
                self.programs_with_arg[x] = []

        self.optimal_depth_search = 1
        self.optimal = False

        # keep track of the minimum size of programs in the joiner
        # the first program added to the joiner is the one with minimal size
        self.min_size = None

    def add_prog(self, prog, prog_size, test_result, add_to_combiner):
        pos_covered = test_result.pos_covered
        inconsistent = test_result.inconsistent
        tp = test_result.tp
        num_pos = self.tester.num_pos

        if add_to_combiner:
            self.add_consistent_program(pos_covered, prog_size)
        elif inconsistent and tp > 0:
            neg_covered = self.tester.test_prog_neg(prog)
            subsumed_joiner = any(
                subset(pos_covered, xs1) and subset(xs2, neg_covered)
                for (xs1, xs2) in self.success_sets_joiner
            )
            covers_everything = (
                self.settings.non_datalog
                and pos_covered.count(1) == num_pos
                and neg_covered.count(1) == self.tester.num_neg
            )
            if not subsumed_joiner and not covers_everything:
                p = Program(prog, False)
                self.to_join[prog_size].append([p, pos_covered, neg_covered])
                self.success_sets_joiner[pos_covered, neg_covered] = p

    def join(self, prog, prog_size):
        head_pred = next(iter(prog))[0].predicate
        num_pos = self.tester.num_pos


        assert(prog_size)

        with stats.duration('join'):
            # add all inconsistent programs if we call the suboptimal joiner, otherwise only add
            # programs with size up to prog_size-joiner.min_size
            max_s = prog_size - self.min_size if (self.state.solution_found and self.min_size) else prog_size
            for k in range(max_s + 1):
                for prog_, pos_covered, neg_covered in self.to_join[k]:
                    self.add_program_fragment(prog_, pos_covered, neg_covered)
                self.to_join[k] = []
            # we only run the joiner up to the current program size

            result = self.make_consistent_fragments(max_size=prog_size)

            for program_, coverage_ in result:
                program_ = inline_logic_rules_ast(program_, head_pred)
                # logger.out(f'JOIN PROG: {format_prog(program_)}')
                tp = coverage_.count(1)
                fn = num_pos - tp
                yield program_, calc_prog_size(program_), TestResult(
                    tp=tp,
                    fn=fn,
                    tn=None,
                    fp=None,
                    pos_covered=coverage_,
                    neg_covered=None,
                    inconsistent=False,
                    conf_matrix=(tp, fn, None, None),
                )

    def add_program_fragment(self, prog, pos_covered, neg_covered):
        prog_hash = get_prog_hash(prog)
        prog_rules = prog.rules
        prog_size = calc_prog_size(prog_rules)
        if prog_hash not in self.proghash_to_id:
            prog_id = len(self.proghash_to_id) + 1
            self.proghash_to_id[prog_hash] = prog_id
            self.progid_to_prog[prog_id] = prog
        prog_id = self.proghash_to_id[prog_hash]
        # if prog.negated:
        #     adjusted_size = prog_size + 2
        # # if the fragment selected has recursion or predicate invention we combine it as a new invented predicate
        # # therefore we need to take into account the new body literal added
        # elif prog_is_recursive(prog_rules) or prog_has_invention(prog_rules):
        #     adjusted_size = prog_size + 1
        # # otherwise we simply concatenate the body literals, therefore the size is calc_prog_size(prog)-1
        # # (we ignore the head literal)
        # else:
        #     adjusted_size = prog_size - 1
        self.progid_to_size[prog_id] = prog_size
        self.min_size = prog_size if not self.min_size else min(self.min_size, prog_size)
        assert self.progid_to_size[prog_id] >= self.min_size
        self.pos_exs_covered[prog_id] = pos_covered
        self.neg_exs_covered[prog_id] = neg_covered

        self.program_selected_var[prog_id] = self.vpool.id("program_selected({0})".format(prog_id))
        pos_not_covered = self.pos_examples_ & ~pos_covered
        neg_not_covered = self.neg_examples_ & ~neg_covered

        for ex, x in enumerate(pos_not_covered):
            if x == 1:
                self.programs_not_covering_example[ex].add(prog_id)
            else:
                self.programs_covering_example[ex].add(prog_id)

        for ex, x in enumerate(neg_not_covered):
            if x == 1:
                self.programs_not_covering_example[-ex-1].add(prog_id)
            else:
                self.programs_covering_example[-ex-1].add(prog_id)

        num_pos_covered = pos_covered.count()
        if num_pos_covered > 1 and pos_not_covered:
            self.incomplete.add(prog_id)

        if not self.settings.non_datalog:
            # programs with more than one rule are added with an invented predicate
            if len(prog_rules) == 1:
                head, body = next(iter(prog_rules))
                body_vars = {a for b in body for a in b.arguments}.intersection(head.arguments)
                for x in body_vars:
                    self.programs_with_arg[x].append(prog_id)
            else:
                head, body = next(iter(prog_rules))
                for x in head.arguments:
                    self.programs_with_arg[x].append(prog_id)               

        if num_pos_covered == 1:
            self.programs_covering_one_positive.add(prog_id)

    def add_consistent_program(self, pos_covered, size):
        self.existing_consistent[size].append(~pos_covered)

    def solve_encoding_suboptimal_asp(self):
        state = self.state
        fragments = []
        uncovered = state.uncovered.copy()

        while uncovered.any():
            # Filter for programs that cover at least one currently uncovered example
            valid_progs = {
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            }

            if len(valid_progs) < 2:
                break

            facts = []

            # 1. Program sizes
            for p in valid_progs:
                facts.append(f"size({p}, {self.progid_to_size[p]}).")

            # 2. Positive examples and coverage gaps
            for i in uncovered.search(1):
                facts.append(f"uncovered({i}).")
                for p in self.programs_not_covering_example[i]:
                    if p in valid_progs:
                        facts.append(f"misses_pos({p}, {i}).")

            # 3. Negative examples
            has_neg = False
            for x in self.neg_index:
                neg_key = -x - 1
                for p in self.programs_not_covering_example[neg_key]:
                    if p in valid_progs:
                        facts.append(f"misses_neg({p}, {x}).")
                        has_neg = True

            if not has_neg:
                break

            # 4. Datalog constraints
            if not self.settings.non_datalog:
                facts.append("datalog_req.")
                for arg in self.head_args:
                    safe_arg = str(arg).replace('V', 'v')
                    facts.append(f"head_arg({safe_arg}).")
                    for p in self.programs_with_arg[arg]:
                        if p in valid_progs:
                            facts.append(f"has_arg({p}, {safe_arg}).")

            # 5. ASP Solving
            encoding = "\n".join(facts) + '\n' + ASP_SUB_OPTIMAL
            ctl = clingo.Control([])
            ctl.add("base", [], encoding)
            ctl.ground([("base", [])])

            optimal_selected = []

            solve_start = time.time()

            def on_model(m):
                nonlocal optimal_selected

                elapsed = time.time() - solve_start

                # m.cost returns a list of integers representing optimization levels
                # Based on your ASP:
                # m.cost[0] will be negative total coverage (since it's a maximize)
                # m.cost[1] will be the total size penalty
                # print(f"ASP New Model Found! Cost: {m.cost} | Time: {elapsed:.3f}s")

                optimal_selected = [a.arguments[0].number for a in m.symbols(atoms=True) if a.name == "select"]

            ctl.solve(on_model=on_model)

            if not optimal_selected:
                break

            # Compute coverage as intersection across selected programs
            pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
            for s in optimal_selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            uncovered &= ~pos_covered
            unfolded_prog = self.build_unfolded_program(optimal_selected)
            fragments.append([unfolded_prog, frozenbitarray(pos_covered)])

            if (~pos_covered).count() > 0:
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded_prog))

            # print('asp', pos_covered.count(1), calc_prog_size(unfolded_prog))

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    def solve_encoding_suboptimal_cpsat(self):
        state = self.state
        fragments = []
        uncovered = state.uncovered.copy()

        while uncovered.any():
            # Restrict to fragments that cover at least one currently uncovered example.
            valid_progs = {
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            }

            if len(valid_progs) < 2:
                break

            uncovered_indices = list(uncovered.search(1))

            # For each negative example, collect which valid fragments miss it.
            # If no valid fragment misses a negative example, the join cannot be
            # made safe (every selected fragment covers it, so the join covers it
            # too), so we stop.
            neg_missers = {}
            for x in self.neg_index:
                missers = [p for p in self.programs_not_covering_example[-x - 1] if p in valid_progs]
                if missers:
                    neg_missers[x] = missers

            if not neg_missers:
                break

            # ------------------------------------------------------------------
            # Build CP-SAT model
            # ------------------------------------------------------------------
            model = cp_model.CpModel()

            # One boolean decision variable per valid fragment.
            select = {p: model.new_bool_var(f's{p}') for p in valid_progs}

            # Hard constraint: at least two fragments must be selected.
            model.add(sum(select[p] for p in valid_progs) >= 2)

            # Hard constraint: every negative example must be missed by at least
            # one selected fragment (ensuring the join misses it too).
            for x, missers in neg_missers.items():
                model.add(sum(select[p] for p in missers) >= 1)

            # Hard constraint (Datalog safety): every head argument must appear
            # in the body of at least one selected fragment.
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [p for p in self.programs_with_arg[arg] if p in valid_progs]
                    if carriers:
                        model.add(sum(select[p] for p in carriers) >= 1)

            # Derived variable: covers_joined[e] is true iff no selected fragment
            # misses positive example e (join semantics: all bodies must hold).
            covers_joined = {}
            for e in uncovered_indices:
                cv = model.new_bool_var(f'c{e}')
                covers_joined[e] = cv
                missers = [p for p in self.programs_not_covering_example[e] if p in valid_progs]
                if missers:
                    # cv=1  =>  every misser is not selected
                    model.add_bool_and([select[p].Not() for p in missers]).only_enforce_if(cv)
                    # cv=0  =>  at least one misser is selected
                    model.add_bool_or([select[p] for p in missers]).only_enforce_if(cv.Not())
                else:
                    # No valid fragment misses e, so the join always covers it.
                    model.add(cv == 1)

            # Hard constraint: the join must cover at least one uncovered example.
            coverage_expr = sum(covers_joined[e] for e in uncovered_indices)
            model.add(coverage_expr >= 1)

            # Size cost: sum of (adjusted_size - 1) over selected fragments.
            # The -1 reflects that body literals are concatenated (head counted once).
            size_expr = sum(select[p] * (self.progid_to_size[p] - 1) for p in valid_progs)

            solver = cp_model.CpSolver()

            solution_printer = VarArraySolutionPrinter()
            solution_printer.phase_name = 'max_cov'

            # ------------------------------------------------------------------
            # Phase 1: maximise coverage (primary objective).
            # ------------------------------------------------------------------
            model.maximize(coverage_expr)
            status = solver.solve(model, solution_printer)

            if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                break

            best_coverage = int(solver.objective_value)

            # ------------------------------------------------------------------
            # Phase 2: fix coverage at the optimum, minimise size (secondary).
            # ------------------------------------------------------------------
            model.add(coverage_expr >= best_coverage)
            model.minimize(size_expr)

            solution_printer.phase_name = 'min_size'
            status = solver.solve(model, solution_printer)

            if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                break

            optimal_selected = [p for p in valid_progs if solver.value(select[p])]

            if not optimal_selected:
                break

            # Compute joint coverage: intersection across all selected fragments
            # (AND semantics -- all body literals must hold simultaneously).
            pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
            for s in optimal_selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            uncovered &= ~pos_covered
            unfolded_prog = self.build_unfolded_program(optimal_selected)
            fragments.append([unfolded_prog, pos_covered])

            if (~pos_covered).count() > 0:
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded_prog))

            # print('cpv1', pos_covered.count(1), calc_prog_size(unfolded_prog))

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments


    def solve_encoding_suboptimal_cpsat_v2(self):
        state = self.state
        fragments = []
        uncovered = state.uncovered.copy()

        while uncovered.any():
            # Restrict to fragments that cover at least one currently uncovered example.
            valid_progs = {
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            }

            if len(valid_progs) < 2:
                break

            uncovered_indices = list(uncovered.search(1))

            # For each negative example, collect which valid fragments miss it.
            # If no valid fragment misses a negative example, the join cannot be
            # made safe (every selected fragment covers it, so the join covers it
            # too), so we stop.
            neg_missers = {}
            for x in self.neg_index:
                missers = [p for p in self.programs_not_covering_example[-x - 1] if p in valid_progs]
                if missers:
                    neg_missers[x] = missers

            if not neg_missers:
                break

            # ------------------------------------------------------------------
            # Build CP-SAT model
            # ------------------------------------------------------------------
            model = cp_model.CpModel()

            # One boolean decision variable per valid fragment.
            select = {p: model.new_bool_var(f's{p}') for p in valid_progs}

            # Hard constraint: at least two fragments must be selected.
            model.add(sum(select[p] for p in valid_progs) >= 2)

            # Hard constraint: every negative example must be missed by at least
            # one selected fragment (ensuring the join misses it too).
            for x, missers in neg_missers.items():
                model.add(sum(select[p] for p in missers) >= 1)

            # Hard constraint (Datalog safety): every head argument must appear
            # in the body of at least one selected fragment.
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [p for p in self.programs_with_arg[arg] if p in valid_progs]
                    if carriers:
                        model.add(sum(select[p] for p in carriers) >= 1)

            # Derived variable: covers_joined[e] is true iff no selected fragment
            # misses positive example e (join semantics: all bodies must hold).
            covers_joined = {}
            for e in uncovered_indices:
                cv = model.new_bool_var(f'c{e}')
                covers_joined[e] = cv
                missers = [p for p in self.programs_not_covering_example[e] if p in valid_progs]
                if missers:
                    # cv=1  =>  every misser is not selected
                    model.add_bool_and([select[p].Not() for p in missers]).only_enforce_if(cv)
                    # cv=0  =>  at least one misser is selected
                    model.add_bool_or([select[p] for p in missers]).only_enforce_if(cv.Not())
                else:
                    # No valid fragment misses e, so the join always covers it.
                    model.add(cv == 1)

            # Hard constraint: the join must cover at least one uncovered example.
            coverage_expr = sum(covers_joined[e] for e in uncovered_indices)
            model.add(coverage_expr >= 1)

            # Size cost: sum of (adjusted_size - 1) over selected fragments.
            # The -1 reflects that body literals are concatenated (head counted once).
            size_expr = sum(select[p] * (self.progid_to_size[p] - 1) for p in valid_progs)

            solver = cp_model.CpSolver()
            solver.parameters.linearization_level = 2
            solution_printer = VarArraySolutionPrinter()
            solution_printer.phase_name = 'max_cov'

            # ------------------------------------------------------------------
            # Phase 1: maximise coverage (primary objective).
            # ------------------------------------------------------------------
            model.maximize(coverage_expr)
            status = solver.solve(model, solution_printer)

            if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                break

            best_coverage = int(solver.objective_value)

            # ------------------------------------------------------------------
            # Phase 2: fix coverage at the optimum, minimise size (secondary).
            # ------------------------------------------------------------------
            model.add(coverage_expr >= best_coverage)
            model.minimize(size_expr)

            solution_printer.phase_name = 'min_size'
            status = solver.solve(model, solution_printer)

            if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                break

            optimal_selected = [p for p in valid_progs if solver.value(select[p])]

            if not optimal_selected:
                break

            # Compute joint coverage: intersection across all selected fragments
            # (AND semantics -- all body literals must hold simultaneously).
            pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
            for s in optimal_selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            uncovered &= ~pos_covered
            unfolded_prog = self.build_unfolded_program(optimal_selected)
            fragments.append([unfolded_prog, pos_covered])

            if (~pos_covered).count() > 0:
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded_prog))

            # print('cpv2', pos_covered.count(1), calc_prog_size(unfolded_prog))

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    def solve_encoding_suboptimal_cpsat_v3(self):
        """
        Single-solve variant of the CP-SAT joiner. Combines the two-phase
        lexicographic objective into one weighted maximisation:

            maximise  coverage * W  -  size_cost

        where W = sum(adjusted_size - 1 for all valid fragments) + 1.

        Because W exceeds the worst-case size_cost by construction, one extra
        covered example always outweighs any size saving, so the ordering is
        exactly lexicographic -- coverage first, size second -- with a single
        solver call instead of two.
        """
        fragments = []
        uncovered = self.state.uncovered.copy()

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1

        while uncovered.any():
            valid_progs = {
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            }

            if len(valid_progs) < 2:
                break

            uncovered_indices = list(uncovered.search(1))

            neg_missers = {}
            for x in self.neg_index:
                missers = [p for p in self.programs_not_covering_example[-x - 1] if p in valid_progs]
                if missers:
                    neg_missers[x] = missers

            if not neg_missers:
                break

            # ------------------------------------------------------------------
            # Build CP-SAT model
            # ------------------------------------------------------------------
            model = cp_model.CpModel()

            select = {p: model.new_bool_var(f's{p}') for p in valid_progs}

            # At least two fragments must be selected.
            model.add(sum(select[p] for p in valid_progs) >= 2)

            # Every negative example must be missed by at least one selected fragment.
            for x, missers in neg_missers.items():
                model.add(sum(select[p] for p in missers) >= 1)

            # Datalog safety: every head argument must appear in at least one
            # selected fragment's body.
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [p for p in self.programs_with_arg[arg] if p in valid_progs]
                    if carriers:
                        model.add(sum(select[p] for p in carriers) >= 1)

            # covers_joined[e] is true iff no selected fragment misses example e.
            covers_joined = {}
            for e in uncovered_indices:
                cv = model.new_bool_var(f'c{e}')
                covers_joined[e] = cv
                missers = [p for p in self.programs_not_covering_example[e] if p in valid_progs]
                if missers:
                    model.add_bool_and([select[p].Not() for p in missers]).only_enforce_if(cv)
                    model.add_bool_or([select[p] for p in missers]).only_enforce_if(cv.Not())
                else:
                    model.add(cv == 1)

            coverage_expr = sum(covers_joined[e] for e in uncovered_indices)
            model.add(coverage_expr >= 1)

            size_expr = sum(select[p] * (self.progid_to_size[p] - 1) for p in valid_progs)

            # W must exceed the maximum possible value of size_expr so that one
            # extra covered example always dominates the full size penalty.
            worst_case_size = sum(self.progid_to_size[p] - 1 for p in valid_progs)
            coverage_weight = worst_case_size + 1

            model.maximize(coverage_expr * coverage_weight - size_expr)

            # ------------------------------------------------------------------
            # Single solve call.
            # ------------------------------------------------------------------
            status = solver.solve(model)

            if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                break

            optimal_selected = [p for p in valid_progs if solver.value(select[p])]

            if not optimal_selected:
                break

            pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
            for s in optimal_selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            uncovered &= ~pos_covered
            unfolded_prog = self.build_unfolded_program(optimal_selected)
            fragments.append([unfolded_prog, pos_covered])

            if (~pos_covered).any():
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded_prog))

            # print('cpv3', pos_covered.count(1), calc_prog_size(unfolded_prog))

        logger.debug(f"number of fragments found with joiner (v2): {len(fragments)}")
        return fragments

    def solve_encoding_suboptimal_maxsat(self):
        uncovered = self.state.uncovered.copy()
        fragments = []

        # Pool for mapping string/tuple keys to SAT variables (integers)
        vpool = IDPool()

        while uncovered.any():
            uncovered_indices = list(uncovered.search(1))

            # 1. Filter valid fragments
            valid_progs = [
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            ]

            if len(valid_progs) < 2:
                break

            # Initialize Weighted CNF
            wcnf = WCNF()

            # Helper to get variable IDs
            # x(p) -> Program p is selected
            def x(p): return vpool.id(f"sel_{p}")
            # c(e) -> Positive example e is covered
            def c(e): return vpool.id(f"cov_{e}")

            # 2. HARD CONSTRAINTS (Must be satisfied)
            # ---------------------------------------------------------

            # Constraint: At least two fragments must be selected
            # We use a cardinality encoding for this
            clauses = CardEnc.atleast(
                lits=[x(p) for p in valid_progs],
                bound=2,
                top_id=vpool.top,
                encoding=EncType.seqcounter
            )
            wcnf.extend(clauses)
            vpool.top = wcnf.nv # Update top variable ID

            # Constraint: Negative Example Hitting Set (Joined rule must be safe)
            for x_neg in self.neg_index:
                neg_key = -x_neg - 1
                missers = [x(p) for p in valid_progs if p in self.programs_not_covering_example[neg_key]]
                if missers:
                    wcnf.append(missers) # OR-clause: at least one must miss the negative
                else:
                    # If no valid fragment misses this negative, no join is safe.
                    return fragments

            # Constraint: Datalog Safety
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [x(p) for p in self.programs_with_arg[arg] if p in valid_progs]
                    if carriers:
                        wcnf.append(carriers)

            # Constraint: Conjunction Logic (Implication)
            # If fragment p is selected AND p misses example e, then joined rule misses e.
            # Logic: x(p) -> ~c(e)  =>  (~x(p) v ~c(e))
            for e in uncovered_indices:
                for p in valid_progs:
                    if p in self.programs_not_covering_example[e]:
                        wcnf.append([-x(p), -c(e)])

            # Constraint: Progress (Must cover at least one new example)
            wcnf.append([c(e) for e in uncovered_indices])

            # 3. SOFT CLAUSES (Optimization)
            # ---------------------------------------------------------

            # Calculate weights for lexicographical optimization
            # W_coverage must be > sum of all possible size penalties
            max_size_penalty = sum(max(1, self.progid_to_size[p] - 1) for p in valid_progs)
            weight_coverage = max_size_penalty + 1

            # Priority 1: Maximize Coverage (Soft clause for each uncovered example)
            for e in uncovered_indices:
                wcnf.append([c(e)], weight=weight_coverage)

            # Priority 2: Minimize Size (Soft clause for NOT selecting a fragment)
            # Penalty is (size - 1).
            for p in valid_progs:
                penalty = max(1, self.progid_to_size[p] - 1)
                wcnf.append([-x(p)], weight=penalty)

            # 4. SOLVE
            # ---------------------------------------------------------
            solve_start = time.time()
            with RC2(wcnf) as rc2:
                model = rc2.compute()

                if model is None:
                    break

                elapsed = time.time() - solve_start

                # Extract selected IDs (Positive literals in the model)
                selected_ids = []
                for p in valid_progs:
                    if x(p) in model:
                        selected_ids.append(p)

            if not selected_ids:
                break

            # 5. POST-PROCESSING
            # ---------------------------------------------------------
            # Compute bitarray intersection
            pos_covered = bitarray(self.pos_exs_covered[selected_ids[0]].copy())
            for p_id in selected_ids[1:]:
                pos_covered &= self.pos_exs_covered[p_id]

            pos_covered = frozenbitarray(pos_covered)

            unfolded = self.build_unfolded_program(selected_ids)
            fragments.append([unfolded, pos_covered])

            # Update search state
            uncovered &= ~pos_covered
            if (~pos_covered).any():
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded))

            # print(f"MaxSAT Model Found! Cost: {rc2.cost} | Time: {elapsed:.3f}s")
            # print('maxsat', pos_covered.count(1), calc_prog_size(unfolded))

        return fragments

    def solve_encoding_suboptimal_maxsat_v2(self):
        uncovered = self.state.uncovered.copy()
        fragments = []

        # Pool for mapping string/tuple keys to SAT variables (integers)
        vpool = IDPool()

        while uncovered.any():
            uncovered_indices = list(uncovered.search(1))

            # 1. Filter valid fragments
            valid_progs = [
                p for p in self.program_selected_var.keys()
                if (self.pos_exs_covered[p] & uncovered).any()
            ]

            if len(valid_progs) < 2:
                break

            # Initialize Weighted CNF
            wcnf = WCNF()

            # Helper to get variable IDs
            # x(p) -> Program p is selected
            def x(p): return vpool.id(f"sel_{p}")
            # c(e) -> Positive example e is covered
            def c(e): return vpool.id(f"cov_{e}")

            # 2. HARD CONSTRAINTS (Must be satisfied)
            # ---------------------------------------------------------

            # Constraint: At least two fragments must be selected
            # We use a cardinality encoding for this
            clauses = CardEnc.atleast(
                lits=[x(p) for p in valid_progs],
                bound=2,
                top_id=vpool.top,
                encoding=EncType.seqcounter
            )
            wcnf.extend(clauses)
            vpool.top = wcnf.nv # Update top variable ID

            # Constraint: Negative Example Hitting Set (Joined rule must be safe)
            for x_neg in self.neg_index:
                neg_key = -x_neg - 1
                missers = [x(p) for p in valid_progs if p in self.programs_not_covering_example[neg_key]]
                if missers:
                    wcnf.append(missers) # OR-clause: at least one must miss the negative
                else:
                    # If no valid fragment misses this negative, no join is safe.
                    return fragments

            # Constraint: Datalog Safety
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [x(p) for p in self.programs_with_arg[arg] if p in valid_progs]
                    if carriers:
                        wcnf.append(carriers)

            # Constraint: Conjunction Logic (Implication)
            # If fragment p is selected AND p misses example e, then joined rule misses e.
            # Logic: x(p) -> ~c(e)  =>  (~x(p) v ~c(e))
            for e in uncovered_indices:
                for p in valid_progs:
                    if p in self.programs_not_covering_example[e]:
                        wcnf.append([-x(p), -c(e)])

            # Constraint: Progress (Must cover at least one new example)
            wcnf.append([c(e) for e in uncovered_indices])

            # 3. SOFT CLAUSES (Optimization)
            # ---------------------------------------------------------

            # Calculate weights for lexicographical optimization
            # W_coverage must be > sum of all possible size penalties
            max_size_penalty = sum(max(1, self.progid_to_size[p] - 1) for p in valid_progs)
            weight_coverage = max_size_penalty + 1

            # Priority 1: Maximize Coverage (Soft clause for each uncovered example)
            for e in uncovered_indices:
                wcnf.append([c(e)], weight=weight_coverage)

            # Priority 2: Minimize Size (Soft clause for NOT selecting a fragment)
            # Penalty is (size - 1).
            for p in valid_progs:
                penalty = max(1, self.progid_to_size[p] - 1)
                wcnf.append([-x(p)], weight=penalty)

            # 4. SOLVE
            # ---------------------------------------------------------
            solve_start = time.time()
            with RC2Stratified(wcnf, solver='g3', adapt=True, exhaust=True, blo='div', incr=False, minz=True, trim=0) as rc2:
            # with RC2(wcnf) as rc2:
                model = rc2.compute()

                if model is None:
                    break

                elapsed = time.time() - solve_start

                # Extract selected IDs (Positive literals in the model)
                selected_ids = []
                for p in valid_progs:
                    if x(p) in model:
                        selected_ids.append(p)

            if not selected_ids:
                break

            # 5. POST-PROCESSING
            # ---------------------------------------------------------
            # Compute bitarray intersection
            pos_covered = bitarray(self.pos_exs_covered[selected_ids[0]].copy())
            for p_id in selected_ids[1:]:
                pos_covered &= self.pos_exs_covered[p_id]

            pos_covered = frozenbitarray(pos_covered)

            unfolded = self.build_unfolded_program(selected_ids)
            fragments.append([unfolded, pos_covered])

            # Update search state
            uncovered &= ~pos_covered
            if (~pos_covered).any():
                self.add_consistent_program(pos_covered, calc_prog_size(unfolded))

            # print(f"MaxSAT Model Found! Cost: {rc2.cost} | Time: {elapsed:.3f}s")
            print('\t maxsat_v2', pos_covered.count(1), calc_prog_size(unfolded))

        return fragments

    # @profile
    def solve_encoding_suboptimal_sat_greedy(self):

        # print('solve_encoding_suboptimal_sat_greedy')

        uncovered = self.state.uncovered.copy()
        fragments = []

        # Build a persistent SAT solver with the time-invariant hard constraints.
        # These never change across outer iterations so we add them once.
        with Solver(name='cadical153') as sat:

            # Negative example hitting set.
            for x_neg in self.neg_index:
                neg_key = -x_neg - 1
                missers = [
                    self.program_selected_var[p]
                    for p in self.programs_not_covering_example[neg_key]
                    if p in self.program_selected_var
                ]
                if missers:
                    sat.add_clause(missers)
                else:
                    return fragments

            # Datalog safety.
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [
                        self.program_selected_var[p]
                        for p in self.programs_with_arg[arg]
                        if p in self.program_selected_var
                    ]
                    if carriers:
                        sat.add_clause(carriers)

            # At least two fragments selected.
            card = CardEnc.atleast(
                lits=list(self.program_selected_var.values()),
                bound=2,
                top_id=self.vpool.top,
                encoding=EncType.seqcounter,
            )
            for clause in card.clauses:
                sat.add_clause(clause)
            self.vpool.top = max(self.vpool.top, card.nv)

            while uncovered.any():
                valid_progs = {
                    p for p in self.program_selected_var
                    if (self.pos_exs_covered[p] & uncovered).any()
                }

                if len(valid_progs) < 2:
                    break

                # Try each uncovered example in turn.
                # For each one, ask SAT: is there a valid selection that covers it?
                # If yes, take that solution and update uncovered.
                # If no, skip this example and try the next.
                solved = False
                for e in uncovered.search(1):
                    # Assumption: example e must be covered, i.e. no selected fragment
                    # from valid_progs misses it.  We force this via a unit assumption
                    # on the coverage implication: assume every misser is NOT selected.
                    missers_of_e = [
                        p for p in self.programs_not_covering_example[e]
                        if p in valid_progs
                    ]

                    # Assume all missers of e are deselected.
                    assumptions = [-self.program_selected_var[p] for p in missers_of_e]

                    # Also restrict to valid_progs only by assuming all others deselected.
                    assumptions += [
                        -self.program_selected_var[p]
                        for p in self.program_selected_var
                        if p not in valid_progs
                    ]

                    if not sat.solve(assumptions=assumptions):
                        # Cannot cover example e under current constraints -- skip it.
                        continue


                    # t1 = time.time()
                    model = sat.get_model()
                    # d1 = time.time()-t1
                    selected_ids = [
                        p for p in valid_progs
                        if model[self.program_selected_var[p] - 1] > 0
                    ]

                    # print('inner loop solving time', d1)

                    if not selected_ids:
                        continue

                    pos_covered = bitarray(self.pos_exs_covered[selected_ids[0]].copy())
                    for p_id in selected_ids[1:]:
                        pos_covered &= self.pos_exs_covered[p_id]

                    if not pos_covered.any():
                        assert(False)
                        # continue

                    len_a = len(selected_ids)
                    # t1 = time.time()
                    selected_ids, pos_covered = self.remove_redundant_fragments(selected_ids, pos_covered)
                    # d = time.time()-t1
                    len_b = len(selected_ids)
                    # print(len_a, len_b)

                    pos_covered = frozenbitarray(pos_covered)
                    uncovered &= ~pos_covered
                    unfolded = self.build_unfolded_program(selected_ids)
                    fragments.append([unfolded, pos_covered])

                    if (~pos_covered).any():
                        self.add_consistent_program(pos_covered, calc_prog_size(unfolded))

                    # print('\t greedy1', pos_covered.count(1), calc_prog_size(unfolded))
                    solved = True
                    break

                if not solved:
                    # No uncovered example could be covered -- done.
                    break

        # logger.debug(f"number of fragments found with joiner (sat greedy): {len(fragments)}")
        return fragments


    def solve_encoding_suboptimal_sat_greedy_v2(self):
        """
        Greedy SAT-based joiner. Covers one uncovered example per outer
        iteration using a persistent CaDiCaL solver. Key design choices:

        - Hard constraints are added once and never rebuilt.
        - Progress is enforced by a hard OR clause over valid_progs added
          each outer iteration -- no invalid_progs assumption list needed.
        - Fragments that no longer cover any uncovered example are permanently
          deselected via unit clauses, shrinking the active problem over time.
        - valid_prog_missers is precomputed once per outer iteration.
        - Uncoverable examples are skipped via a cheap set lookup before
          any solver call.
        """
        # from pysat.solvers import Solver
        # from pysat.card import CardEnc, EncType
        # from pysat.formula import IDPool

        if not self.state.uncovered.any():
            return []

        uncovered = self.state.uncovered.copy()
        fragments = []

        with Solver(name='cadical153') as sat:

            # ------------------------------------------------------------------
            # One-time hard constraints.
            # ------------------------------------------------------------------

            # Negative example hitting set.
            for x_neg in self.neg_index:
                neg_key = -x_neg - 1
                missers = [
                    self.program_selected_var[p]
                    for p in self.programs_not_covering_example[neg_key]
                    if p in self.program_selected_var
                ]
                if missers:
                    sat.add_clause(missers)
                else:
                    return fragments

            # Datalog safety.
            if not self.settings.non_datalog:
                for arg in self.head_args:
                    carriers = [
                        self.program_selected_var[p]
                        for p in self.programs_with_arg[arg]
                        if p in self.program_selected_var
                    ]
                    if carriers:
                        sat.add_clause(carriers)

            # At least two fragments selected.
            card = CardEnc.atleast(
                lits=list(self.program_selected_var.values()),
                bound=2,
                top_id=self.vpool.top,
                encoding=EncType.seqcounter,
            )
            for clause in card.clauses:
                sat.add_clause(clause)
            self.vpool.top = max(self.vpool.top, card.nv)

            # Bias solver to prefer not selecting fragments -- encourages
            # minimal selections on the first guess.
            # for var in self.program_selected_var.values():
                # sat.set_polarity(var, False)

            # ------------------------------------------------------------------
            # Outer loop.
            # ------------------------------------------------------------------
            valid_progs = {
                p for p in self.program_selected_var
                if (self.pos_exs_covered[p] & uncovered).any()
            }

            while uncovered.any():
                if len(valid_progs) < 2:
                    break

                # Progress clause: at least one fragment from valid_progs must
                # be selected.  Since every fragment in valid_progs covers at
                # least one currently uncovered example, this guarantees the
                # join makes progress.  Added as a hard clause -- no assumption
                # needed, and valid_progs only shrinks so this is monotone.
                sat.add_clause([self.program_selected_var[p] for p in valid_progs])

                # Precompute missers filtered to valid_progs once per outer
                # iteration rather than once per example.
                valid_prog_missers = {
                    e: [p for p in self.programs_not_covering_example[e] if p in valid_progs]
                    for e in uncovered.search(1)
                }

                solved = False
                for e, missers_of_e in valid_prog_missers.items():
                    # Cheap pre-filter: if no valid fragment covers e, skip
                    # immediately without calling the solver.
                    if not self.programs_covering_example[e].intersection(valid_progs):
                        continue

                    # Assume every misser of e is deselected, forcing the join
                    # to cover e.  This is the only assumption needed -- the
                    # progress clause handles the restriction to valid_progs.
                    assumptions = [-self.program_selected_var[p] for p in missers_of_e]

                    if not sat.solve(assumptions=assumptions):
                        continue

                    model_set = {v for v in sat.get_model() if v > 0}
                    selected_ids = [
                        p for p in valid_progs
                        if self.program_selected_var[p] in model_set
                    ]

                    if not selected_ids:
                        continue

                    pos_covered = bitarray(self.pos_exs_covered[selected_ids[0]].copy())
                    for p_id in selected_ids[1:]:
                        pos_covered &= self.pos_exs_covered[p_id]

                    if not pos_covered.any():
                        assert False, "solver returned selection covering nothing"

                    selected_ids, pos_covered = self.remove_redundant_fragments(
                        selected_ids, pos_covered
                    )

                    # from bitarray.util import frozenbitarray
                    pos_covered = frozenbitarray(pos_covered)
                    uncovered &= ~pos_covered

                    unfolded = self.build_unfolded_program(selected_ids)
                    fragments.append([unfolded, pos_covered])

                    if (~pos_covered).any():
                        self.add_consistent_program(pos_covered, calc_prog_size(unfolded))

                    # ----------------------------------------------------------
                    # Shrink the encoding: permanently deselect fragments that
                    # no longer cover any uncovered example, and update
                    # valid_progs incrementally rather than recomputing it.
                    # ----------------------------------------------------------
                    newly_useless = {
                        p for p in valid_progs
                        if not (self.pos_exs_covered[p] & uncovered).any()
                    }
                    for p in newly_useless:
                        sat.add_clause([-self.program_selected_var[p]])
                    valid_progs -= newly_useless

                    # print('\t greedy2', pos_covered.count(1), calc_prog_size(unfolded))
                    solved = True
                    break

                if not solved:
                    break

        logger.debug(f"number of fragments found with joiner (sat greedy): {len(fragments)}")
        return fragments

    def remove_redundant_fragments(self, selected_ids, pos_covered):
        """
        Greedily remove fragments from selected_ids that are not needed.
        A fragment is redundant if the remaining selection:
          - still covers at least one positive example (intersection is non-empty)
          - still misses all negative examples (intersection of neg_covered is empty)
          - still satisfies Datalog safety
        """
        changed = True
        while changed:
            changed = False
            for p in list(selected_ids):
                if len(selected_ids) <= 2:
                    # Must keep at least two fragments (hard constraint).
                    break
                subselected = [q for q in selected_ids if q != p]

                # Check Datalog safety first -- cheapest check.
                if self.break_datalog(subselected):
                    continue

                # Recompute coverage for the reduced selection.
                sub_pos = bitarray(self.pos_exs_covered[subselected[0]].copy())
                for q in subselected[1:]:
                    sub_pos &= self.pos_exs_covered[q]

                if not sub_pos.any():
                    # Removing p loses all coverage.
                    continue

                # Check neg safety: intersection of neg_covered must be empty.
                # neg_exs_covered[q] is the set of negatives covered by q.
                # The join covers a negative iff ALL selected fragments cover it.
                sub_neg = bitarray(self.neg_exs_covered[subselected[0]].copy())
                for q in subselected[1:]:
                    sub_neg &= self.neg_exs_covered[q]

                if sub_neg.any():
                    # Removing p causes the join to cover a negative example.
                    continue

                # p is genuinely redundant -- drop it.
                selected_ids = subselected
                pos_covered = frozenbitarray(sub_pos)
                changed = True
                break

        return selected_ids, pos_covered

    # def solve_encoding_greedy_sat(self):
    #     # Work on a copy of the uncovered examples
    #     uncovered = self.state.uncovered.copy()

    #     # Track which examples we have already attempted to cover so we don't loop forever
    #     processed = bitarray(len(uncovered))
    #     processed.setall(0)

    #     fragments = []

    #     # Continue while there are uncovered examples we haven't tried yet
    #     while (uncovered & ~processed).any():
    #         # FIX 1: Use next() to get the first available bit index from the iterator
    #         try:
    #             e_target = next((uncovered & ~processed).search(1))
    #         except StopIteration:
    #             break

    #         processed[e_target] = 1 # Mark this target example as attempted

    #         # Filter: In an AND-join, EVERY fragment must cover the target example.
    #         # We only consider fragments that already cover e_target.
    #         candidates = [
    #             p for p in self.program_selected_var.keys()
    #             if self.pos_exs_covered[p][e_target] == 1
    #         ]

    #         # Requirement: At least two fragments must cover this example to form a join
    #         if len(candidates) < 2:
    #             continue

    #         # 1. Initialize Solver and Variable Pool
    #         # Fresh solver and pool for each target example to keep the problem small
    #         solver = Solver(name='cadical153')
    #         vpool = IDPool()
    #         def x(p): return vpool.id(f"sel_{p}")

    #         # 2. Hard Constraint: At least two fragments must be selected
    #         # FIX 2: Use cnf.clauses and cnf.nv for PySAT compatibility
    #         # FIX: Use seqcounter (0) or totalizer (2) for bounds > 1
    #         cnf = CardEnc.atleast(
    #             lits=[x(p) for p in candidates],
    #             bound=2,
    #             top_id=vpool.top,
    #             encoding=EncType.seqcounter
    #         )
    #         for clause in cnf.clauses:
    #             solver.add_clause(clause)

    #         # Update the pool ceiling to account for auxiliary variables used by CardEnc
    #         vpool.top = cnf.nv

    #         # 3. Hard Constraint: Consistency (Hitting Set for Negatives)
    #         # The join must miss every negative example.
    #         # For each negative, at least one selected fragment must miss it.
    #         possible_to_be_consistent = True
    #         for x_neg in self.neg_index:
    #             neg_key = -x_neg - 1
    #             # Candidates that miss this negative
    #             missers = [x(p) for p in candidates if p in self.programs_not_covering_example[neg_key]]
    #             if missers:
    #                 solver.add_clause(missers)
    #             else:
    #                 # If no candidate misses this negative, a consistent join for e_target is impossible
    #                 possible_to_be_consistent = False
    #                 break

    #         if not possible_to_be_consistent:
    #             solver.delete()
    #             continue

    #         # 4. Hard Constraint: Datalog Safety (Optional)
    #         if not self.settings.non_datalog:
    #             for arg in self.head_args:
    #                 carriers = [x(p) for p in self.programs_with_arg[arg] if p in candidates]
    #                 if carriers:
    #                     solver.add_clause(carriers)
    #                 else:
    #                     possible_to_be_consistent = False
    #                     break

    #         if not possible_to_be_consistent:
    #             solver.delete()
    #             continue

    #         # 5. Solve the SAT problem
    #         if solver.solve():
    #             model = solver.get_model()
    #             model_set = set(model)

    #             # Identify which programs the solver picked
    #             selected_ids = [p for p in candidates if x(p) in model_set]

    #             # Compute the actual intersection coverage of the new joined rule
    #             # intersection across all selected fragments
    #             res_covered = bitarray(self.pos_exs_covered[selected_ids[0]].copy())
    #             for p_id in selected_ids[1:]:
    #                 res_covered &= self.pos_exs_covered[p_id]

    #             res_covered = frozenbitarray(res_covered)
    #             unfolded = self.build_unfolded_program(selected_ids)
    #             fragments.append([unfolded, res_covered])

    #             # Update search state: anything covered by this rule is removed from 'uncovered'
    #             uncovered &= ~res_covered

    #             # Maintain consistency constraints for future search iterations
    #             if (~res_covered).any():
    #                 self.add_consistent_program(res_covered, calc_prog_size(unfolded))

    #             print('\t greedy2', res_covered.count(1), calc_prog_size(unfolded))

    #         # Cleanup solver memory
    #         solver.delete()

        return fragments
    def make_consistent_fragments(self, min_size=None, max_size=None):
        # if we do not yet have a solution, only try to find at least one fragment which cover each positive example
        if self.state.solution_found:
            return []

        # print('make_consistent_fragments', min_size, max_size)
        if not min_size:
            min_size = self.optimal_depth_search
        else:
            min_size = max(min_size, self.optimal_depth_search)

        # print('solve_encoding_subopt')


        # @AC I AM STILL TRYING TO DECIDE HOW BEST TO DO THIS JOIN STAGE
        with stats.duration('join'):
            return self.solve_encoding_suboptimal_sat_greedy_v2()

        # t1 = time.time()
        # x = self.solve_encoding_suboptimal_sat_greedy()
        # print(f'supergreedy {time.time()-t1}')

        # t1 = time.time()
        # self.solve_encoding_suboptimal_sat_greedy_v2()
        # print(f'supergreedy v2 {time.time()-t1}')

        # return x



        # t1 = time.time()
            # return self.solve_encoding_suboptimal_sat_greedy()
        # print(f'supergreedy claude {time.time()-t1}')


        # t1 = time.time()
        # x = self.solve_encoding_suboptimal_maxsat_v2()
        # print(f'MaxSAT_v2 {time.time()-t1}')


        # t1 = time.time()
        # self.solve_encoding_suboptimal_maxsat()
        # print(f'MaxSAT {time.time()-t1}')



        # with stats.duration('solve_encoding_suboptimal_cpsat'):
        # t1 = time.time()
        # self.solve_encoding_suboptimal_cpsat()
        # print(f'CP_V1 {time.time()-t1}')

        # t1 = time.time()
        # self.solve_encoding_suboptimal_cpsat_v2()
        # print(f'CP_V2 {time.time()-t1}')

        # t1 = time.time()
        # self.solve_encoding_suboptimal_cpsat_v3()
        # print(f'CP_V3 {time.time()-t1}')

        # # with stats.duration('solve_encoding_suboptimal_asp'):
        # t1 = time.time()
        # x = self.solve_encoding_suboptimal_asp()
        # print(f'ASP {time.time()-t1}')

        # return x

    def break_datalog(self, subselected):
        if self.settings.non_datalog:
            return False
        for x in self.programs_with_arg:
            if not set(subselected).intersection(set(self.programs_with_arg[x])):
                return True
        return False

    def build_unfolded_program(self, fragments):
        program = []
        body_literals = []
        if not fragments:
            return []
        head_args = self.head_args
        fragment_vars = len(self.head_args)
        for frag_id in fragments:
            frag = self.progid_to_prog[frag_id]
            if frag.negated or len(frag) > 1:
                for h, _ in list(frag.rules):
                    if h.predicate == self.head_pred:
                        # head = self.settings.cached_literals[h.predicate, h.arguments]
                        head = Literal(h.predicate, h.arguments)
                rules, literal = self.build_invented(frag, negated=frag.negated)
                body_literals.append(literal)
                program.extend(rules)
            else:
                (h, body) = list(frag.rules)[0]
                if h.predicate == self.head_pred:
                    head = self.settings.cached_literals[h.predicate, head_args]
                fragment_var_map = dict()
                for literal in body:
                    literal_arguments = []
                    for a in literal.arguments:
                        if a in head_args:
                            literal_arguments.append(a)
                        elif a in fragment_var_map:
                            literal_arguments.append(fragment_var_map[a])
                        else:
                            fragment_var_map[a] = fragment_vars
                            fragment_vars += 1
                            literal_arguments.append(fragment_var_map[a])
                    literal_arguments = tuple(arg for arg in literal_arguments)
                    body_literals.append(self.settings.search_cached_literals(literal.predicate, literal_arguments))

        body = frozenset(body_literals)
        program.append(tuple((head, body)))
        return frozenset(program)

    def build_invented(self, fragment_prog, negated=False):
        invented_id = abs(get_prog_hash(fragment_prog))
        new_pred = self.head_pred + f"_{invented_id}"
        head_args = self.head_args
        if self.settings.has_directions:
            self.settings.literal_inputs[(new_pred, head_args)] = frozenset(arg for i, arg in enumerate(head_args) if self.settings.directions[self.head_pred][i] == '+')
            self.settings.literal_outputs[(new_pred, head_args)] = frozenset(arg for i, arg in enumerate(head_args) if self.settings.directions[self.head_pred][i] == '-')
        rules = []
        for r in fragment_prog.rules:
            (h, b) = r
            if h.predicate == self.head_pred:
                new_head = Literal(new_pred, head_args)
            else:
                new_head = h
            new_body = []
            for l in b:
                if l.predicate == self.head_pred:
                    new_l = Literal(new_pred, l.arguments)
                    if self.settings.has_directions:
                        for i, arg in enumerate(head_args):
                            self.settings.directions[new_pred][i] = self.settings.directions[self.head_pred][i]
                else:
                    new_l = l
                new_body.append(new_l)
            rules.append(tuple((new_head, frozenset(new_body))))
        # return rules, Literal(new_pred, head_args, negated=negated)
        # AC CHANGED
        return rules, Literal(new_pred, head_args)

def inline_logic_rules_ast(program_, target_predicate):
    """
    Inlines logic rules operating directly on a program object.
    Returns the AST (frozenset of rules) containing the inlined target rule.
    """
    rules = {}
    max_var = -1

    # 1. Parse the program objects into a dictionary and find the max variable ID
    for head, body in program_:
        rules[head.predicate] = {
            'head_literal': head,
            'args': head.arguments,
            'body': body
        }

        # Find the highest integer variable to avoid collisions globally
        for arg in head.arguments:
            if isinstance(arg, int) and arg > max_var:
                max_var = arg
        for lit in body:
            for arg in lit.arguments:
                if isinstance(arg, int) and arg > max_var:
                    max_var = arg

    if target_predicate not in rules:
        raise ValueError(f"Predicate '{target_predicate}' not found in program.")

    # 2. Perform the inlining
    root = rules[target_predicate]
    inlined_body_literals = []
    current_vars = set(root['args'])

    # Start generating new variables strictly above the highest known variable ID
    var_counter = max(max_var, 0)

    # Extract the Literal constructor dynamically to ensure type matching
    Literal = type(root['head_literal'])

    for lit in root['body']:
        if lit.predicate in rules and lit.predicate != target_predicate:
            sub_rule = rules[lit.predicate]

            # Map formal parameters (sub-rule head args) to actual parameters (call site args)
            mapping = dict(zip(sub_rule['args'], lit.arguments))

            for sub_lit in sub_rule['body']:
                new_sub_args = []
                for sa in sub_lit.arguments:
                    if sa in mapping:
                        # Map to the variable passed in the call
                        new_sub_args.append(mapping[sa])
                    elif isinstance(sa, int):
                        # Local variable in the sub-rule: Check for collision
                        if sa in current_vars:
                            var_counter += 1
                            mapping[sa] = var_counter
                        else:
                            mapping[sa] = sa
                            current_vars.add(sa)
                        new_sub_args.append(mapping[sa])
                    else:
                        # Constants or other types
                        new_sub_args.append(sa)

                # Create a new Literal object for the inlined body
                new_literal = Literal(predicate=sub_lit.predicate, arguments=tuple(new_sub_args))
                inlined_body_literals.append(new_literal)
        else:
            # Not an invented rule we can inline, keep the literal object as is
            inlined_body_literals.append(lit)
            current_vars.update([a for a in lit.arguments if isinstance(a, int)])

    # 3. Construct and return the new AST
    inlined_rule = (root['head_literal'], frozenset(inlined_body_literals))

    return frozenset({inlined_rule})


class VarArraySolutionPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self):
        cp_model.CpSolverSolutionCallback.__init__(self)
        # self._variables_dict = variables_dict
        self._start_time = time.time()
        self.phase_name = ''
        self._solution_count = 0

    def on_solution_callback(self):
        self._solution_count += 1
        elapsed_time = time.time() - self._start_time

        # Access the current objective value (the "cost")
        # obj = self.ObjectiveValue()

        # Get the IDs of the selected fragments
        # selected = [p for p, var in self._variables_dict.items() if self.Value(var)]

        # print(f"CPSAT {self.phase_name} model #{self._solution_count} | Cost: {obj:.2f} | Time: {elapsed_time:.3f}s")
        # print(f"Selected Fragments: {selected}")