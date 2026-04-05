# Code and idea from the paper:
# Céline Hocquette, Andreas Niskanen, Rolf Morel, Matti Järvisalo, Andrew Cropper:
# Learning Big Logical Rules by Joining Small Rules. IJCAI 2024: 3430-3438

from pysat.solvers import Glucose3, Cadical153
from pysat.card import CardEnc, EncType
import clingo
import time
import collections
from collections import defaultdict
from bitarray import bitarray, frozenbitarray
from bitarray.util import ones
from bitarray.util import subset
from pysat.solvers import Solver
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


ASP_GREEDY = """
% ==========================================

#show select/1.
2 { select(P) : program(P) }.

is_safe(E) :- select(P), hits_neg(_,E), not hits_neg(P, E).
:- select(P), hits_neg(P, E), not is_safe(E).
"""


# program(P):- misses_neg(P,_).
# neg_ex(E):- misses_neg(_,E).
# neg_ex(E):- hits_neg(_,E).

# % Must miss all negative examples
# % :- neg_ex(E), { select(P) : misses_neg(P, E) } 0.
# :- neg_ex(E), #count{P : select(P), not misses_neg(P, E)} == 0.

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

        # assert(prog_size)

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

                # pos_, neg_ = self.tester.test_prog_all(program_)
                # assert(pos_.count(1) == coverage_.count(1))
                # assert(neg_.count(1) == 0)

                logger.debug(f'JOIN PROG: {format_prog(program_)}')
                # print(f'JOIN PROG: {format_prog(program_)}')
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

    def make_consistent_fragments(self, min_size=None, max_size=None):
        # if we do not yet have a solution, only try to find at least one fragment which cover each positive example
        if self.state.solution_found:
            return []

        # print('make_consistent_fragments', min_size, max_size)
        if not min_size:
            min_size = self.optimal_depth_search
        else:
            min_size = max(min_size, self.optimal_depth_search)

        head_pred  = self.settings.head_literal.predicate

        def test_it(x):
            program_, coverage_ = x
            program_ = inline_logic_rules_ast(program_, head_pred)

            pos_, neg_ = self.tester.test_prog_all(program_)
            if pos_.count(1) != coverage_.count(1):
                print(format_prog(program_))
                assert(False)
            if neg_.count(1) != 0:
                print(format_prog(program_), neg_.count(1))
                assert(False)

            # print(format_prog(program_), 'GOOD!')
        # @AC I AM STILL TRYING TO DECIDE HOW BEST TO DO THIS JOIN STAGE


        # t1 = time.time()
        # xs = self.solve_sat(version=1)
        # print('sat1', time.time()-t1)
        # list(map(test_it, xs))

        # t1 = time.time()
        xs = self.solve_sat2()
        # print('sat2', time.time()-t1)
        # list(map(test_it, xs))

        # t1 = time.time()
        # xs = self.solve_sat(version=2)
        # print('sat2', time.time()-t1)
        # list(map(test_it, xs))

        # t1 = time.time()
        # xs = self.solve_asp()
        # print('asp', time.time()-t1)
        return xs

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

    # @profile
    def solve_sat(self, version=1):
        state = self.state
        uncovered = state.uncovered.copy()
        fragments = []

        while uncovered.any():
            solved = False
            for e in uncovered.search(1):
                # 1. Target R: Find all rules that cover positive example e
                R = [p for p in self.program_selected_var if self.pos_exs_covered[p][e]]
                if len(R) < 2:
                    continue

                # 2. Variable mapping
                vpool = IDPool()
                prog_to_var = {p: vpool.id(f"p_{p}") for p in R}

                if version == 1:
                    solver = Glucose3()
                elif version == 2:
                    solver = Cadical153()

                # 3. Cardinality constraint: at least 2 selected
                lits = [prog_to_var[p] for p in R]
                cnf = CardEnc.atleast(lits, bound=2, top_id=vpool.top, encoding=EncType.seqcounter)
                solver.append_formula(cnf.clauses)
                vpool.top = cnf.nv

                # 4. Safety constraints (linear encoding)
                # For each negative example E, at least one selected program must miss E.
                # Single clause: OR{ select(Q) : Q misses E }
                # No need to condition on hitters -- if all selected programs hit E, clause fires.
                possible = True
                for x in self.neg_index:
                    neg_key = -x - 1
                    missers = [prog_to_var[p] for p in R if p not in self.programs_covering_example[neg_key]]
                    hitters = [p for p in R if p in self.programs_covering_example[neg_key]]

                    if not hitters:
                        continue  # no program in R hits E, naturally safe

                    if not missers:
                        # every program in R hits E, impossible to make safe
                        possible = False
                        break

                    # at least one misser must be selected
                    solver.add_clause(missers)

                if not possible or not solver.solve():
                    solver.delete()
                    continue

                # 5. Extract model
                model_set = set(solver.get_model())
                solver.delete()
                optimal_selected = [p for p in R if prog_to_var[p] in model_set]

                if not optimal_selected:
                    continue

                # 6. Compute coverage
                pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
                for s in optimal_selected[1:]:
                    pos_covered &= self.pos_exs_covered[s]

                if not pos_covered.any():
                    continue

                uncovered &= ~pos_covered
                unfolded_prog = self.build_unfolded_program(optimal_selected)
                fragments.append([unfolded_prog, frozenbitarray(pos_covered)])
                solved = True
                break

            if not solved:
                break

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    def solve_sat2(self):
        state = self.state
        uncovered = state.uncovered.copy()
        fragments = []

        while uncovered.any():
            solved = False
            for e in uncovered.search(1):
                R = [p for p in self.program_selected_var if self.pos_exs_covered[p][e]]
                if len(R) < 2:
                    continue

                prog_to_var = {p: i + 1 for i, p in enumerate(R)}
                top_id = len(R)

                # Compute union of negative examples hit by any program in R
                # to avoid iterating over all neg examples
                hit_neg_union = bitarray(len(self.neg_index))
                hit_neg_union.setall(0)
                for p in R:
                    hit_neg_union |= self.neg_exs_covered[p]

                # Pre-feasibility check before building solver
                possible = True
                misser_clauses = []
                for x in hit_neg_union.search(1):
                    missers = [prog_to_var[p] for p in R if not self.neg_exs_covered[p][x]]
                    if not missers:
                        possible = False
                        break
                    misser_clauses.append(missers)

                if not possible:
                    continue

                solver = Glucose3()
                cnf = CardEnc.atleast(
                    lits=list(prog_to_var.values()),
                    bound=2,
                    top_id=top_id,
                    encoding=EncType.seqcounter
                )
                solver.append_formula(cnf.clauses)
                for clause in misser_clauses:
                    solver.add_clause(clause)

                if not solver.solve():
                    solver.delete()
                    continue

                model_set = set(solver.get_model())
                solver.delete()
                optimal_selected = [p for p in R if prog_to_var[p] in model_set]

                if not optimal_selected:
                    continue

                pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
                for s in optimal_selected[1:]:
                    pos_covered &= self.pos_exs_covered[s]

                if not pos_covered.any():
                    continue

                uncovered &= ~pos_covered
                unfolded_prog = self.build_unfolded_program(optimal_selected)
                fragments.append([unfolded_prog, frozenbitarray(pos_covered)])
                solved = True
                break

            if not solved:
                break

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    def solve_asp(self):
        state = self.state
        uncovered = state.uncovered.copy()
        fragments = []

        while uncovered.any():
            solved = False

            for e in uncovered.search(1):

                # 1. Target R: Find all rules (programs) that cover positive example e
                R = {
                    p for p in self.program_selected_var
                    if self.pos_exs_covered[p][e]
                }

                if len(R) < 2:
                    continue

                facts = []

                for p in R:
                    facts.append(f'program({p}).')

                # 3. Negative examples
                has_neg = False
                for x in self.neg_index:
                    neg_key = -x - 1
                    for p in self.programs_covering_example[neg_key]:
                        if p in R:
                            facts.append(f"hits_neg({p}, {x}).")
                            has_neg = True

                if not has_neg:
                    break

                # 5. ASP Solving
                encoding = "\n".join(facts) + '\n' + ASP_GREEDY

                # with open('asp_sat.pl', 'w') as f:
                    # f.write(encoding)

                # print('**********')
                # print(encoding)
                # print('**********')
                ctl = clingo.Control([])
                ctl.add("base", [], encoding)
                ctl.ground([("base", [])])



                optimal_selected = []

                solve_start = time.time()

                def on_model(m):
                    nonlocal optimal_selected

                    # elapsed = time.time() - solve_start

                    # m.cost returns a list of integers representing optimization levels
                    # Based on your ASP:
                    # m.cost[0] will be negative total coverage (since it's a maximize)
                    # m.cost[1] will be the total size penalty
                    # print(f"ASP New Model Found! Cost: {m.cost} | Time: {elapsed:.3f}s")

                    optimal_selected = [a.arguments[0].number for a in m.symbols(atoms=True) if a.name == "select"]
                    # optimal_selected = [a.arguments[0].number for a in m.symbols(atoms=True)]

                ctl.solve(on_model=on_model)

                if not optimal_selected:
                    continue

                # Compute coverage as intersection across selected programs
                pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
                for s in optimal_selected[1:]:
                    pos_covered &= self.pos_exs_covered[s]

                uncovered &= ~pos_covered
                unfolded_prog = self.build_unfolded_program(optimal_selected)
                fragments.append([unfolded_prog, frozenbitarray(pos_covered)])

                # if (~pos_covered).count() > 0:
                    # self.add_consistent_program(pos_covered, calc_prog_size(unfolded_prog))

                # exit()
                # print(optimal_selected)
                # print(format_prog(unfolded_prog))

                # program_ = inline_logic_rules_ast(unfolded_prog, self.head_pred)
                # pos_, neg_ = self.tester.test_prog_all(program_)
                # if pos_.count(1) != pos_covered.count(1):
                #     print(format_prog(program_))
                #     assert(False)
                # if neg_.count(1) != 0:
                #     print(format_prog(program_), neg_.count(1))
                #     assert(False)
                # print('asp', pos_covered.count(1), calc_prog_size(unfolded_prog))

            if not solved:
                break

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments


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

