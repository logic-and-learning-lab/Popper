# Code and idea from the paper:
# Céline Hocquette, Andreas Niskanen, Rolf Morel, Matti Järvisalo, Andrew Cropper:
# Learning Big Logical Rules by Joining Small Rules. IJCAI 2024: 3430-3438

from pysat.solvers import Glucose3, Cadical153
from pysat.card import CardEnc, EncType
import clingo
import os
import time
import collections
from collections import defaultdict
from bitarray import bitarray, frozenbitarray
from bitarray.util import ones, zeros, subset
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

        if inconsistent and tp > 0:
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

        logger.info('Starting join stage')

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

        logger.info('Finished join stage')

    def add_program_fragment(self, prog, pos_covered, neg_covered):
        prog_hash = get_prog_hash(prog)
        prog_rules = prog.rules
        prog_size = calc_prog_size(prog_rules)
        if prog_hash not in self.proghash_to_id:
            prog_id = len(self.proghash_to_id) + 1
            self.proghash_to_id[prog_hash] = prog_id
            self.progid_to_prog[prog_id] = prog
        prog_id = self.proghash_to_id[prog_hash]

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
        # print(size, pos_covered, ~pos_covered)
        # self.existing_consistent[size].append(~pos_covered) WHY ~ ????
        self.existing_consistent[size].append(pos_covered)

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
            self.solve_complete()
            # list(find_good_fragments_from_joiner(self))
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

        xs = self.solve_sat3()

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

    def solve_sat3(self):
        state = self.state
        uncovered = state.uncovered.copy()
        fragments = []
        all_progs = list(self.program_selected_var)
        if len(all_progs) < 2:
            return fragments

        prog_to_var = {p: i + 1 for i, p in enumerate(all_progs)}
        top_id = len(all_progs)

        solver = Glucose3()

        hit_neg_union = bitarray(len(self.neg_index))
        hit_neg_union.setall(0)
        for p in all_progs:
            hit_neg_union |= self.neg_exs_covered[p]

        feasible = True
        for x in hit_neg_union.search(1):
            missers = [prog_to_var[p] for p in all_progs if not self.neg_exs_covered[p][x]]
            if not missers:
                feasible = False
                break
            solver.add_clause(missers)

        if not feasible:
            solver.delete()
            return fragments

        cnf = CardEnc.atleast(
            lits=list(prog_to_var.values()),
            bound=2,
            top_id=top_id,
            encoding=EncType.seqcounter
        )
        solver.append_formula(cnf.clauses)

        newly_covered = set()

        for e in uncovered.search(1):
            if e in newly_covered:
                continue

            assumptions = [-prog_to_var[p] for p in all_progs if not self.pos_exs_covered[p][e]]
            if not solver.solve(assumptions=assumptions):
                newly_covered.add(e)
                continue

            model_set = set(solver.get_model())
            optimal_selected = [p for p in all_progs if prog_to_var[p] in model_set and self.pos_exs_covered[p][e]]

            if len(optimal_selected) < 2:
                newly_covered.add(e)
                continue

            pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
            for s in optimal_selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            selected_ids, pos_covered_ = self.remove_redundant_fragments(optimal_selected, pos_covered)
            if len(selected_ids) < len(optimal_selected):
                print(len(selected_ids), len(optimal_selected))

            assert subset(frozenbitarray(pos_covered), pos_covered_)

            if not pos_covered.any():
                newly_covered.add(e)
                continue

            for covered_e in pos_covered.search(1):
                newly_covered.add(covered_e)

            unfolded_prog = self.build_unfolded_program(optimal_selected)
            fragments.append([unfolded_prog, frozenbitarray(pos_covered)])

        solver.delete()
        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    # def solve_asp2(self):
    #     state = self.state
    #     uncovered = state.uncovered.copy()
    #     fragments = []

    #     all_progs = list(self.program_selected_var)
    #     if len(all_progs) < 2:
    #         return fragments

    #     # Build static facts once
    #     facts = []
    #     for p in all_progs:
    #         facts.append(f'program({p}).')

    #     # Negative example facts over all programs
    #     has_neg = False
    #     hit_neg_union = bitarray(len(self.neg_index))
    #     hit_neg_union.setall(0)
    #     for p in all_progs:
    #         hit_neg_union |= self.neg_exs_covered[p]

    #     feasible = True
    #     for x in hit_neg_union.search(1):
    #         missers = [p for p in all_progs if not self.neg_exs_covered[p][x]]
    #         if not missers:
    #             feasible = False
    #             break
    #         for p in all_progs:
    #             if self.neg_exs_covered[p][x]:
    #                 facts.append(f'hits_neg({p},{x}).')
    #                 has_neg = True

    #     if not feasible or not has_neg:
    #         return fragments

    #     # Positive example facts: covers(P, E) for scoping per iteration
    #     for p in all_progs:
    #         for e in self.pos_exs_covered[p].search(1):
    #             facts.append(f'covers_pos({p},{e}).')

    #     ASP_GREEDY = """
    #     #show select/1.
    #     2 { select(P) : program(P) }.
    #     is_safe(E) :- select(P), hits_neg(_,E), not hits_neg(P, E).
    #     :- select(P), hits_neg(P, E), not is_safe(E).
    #     """
    #     encoding = "\n".join(facts) + '\n' + ASP_GREEDY

    #     ctl = clingo.Control(['--warn=none'])
    #     ctl.add("base", [], encoding)
    #     ctl.ground([("base", [])])

    #     while uncovered.any():
    #         solved = False
    #         for e in uncovered.search(1):
    #             # Scope to programs covering e via assumptions
    #             assumptions = []
    #             for p in all_progs:
    #                 atom = clingo.Function('program', [clingo.Number(p)])
    #                 if self.pos_exs_covered[p][e]:
    #                     assumptions.append((atom, True))
    #                 else:
    #                     assumptions.append((atom, False))

    #             optimal_selected = []
    #             def on_model(m):
    #                 nonlocal optimal_selected
    #                 optimal_selected = [
    #                     a.arguments[0].number
    #                     for a in m.symbols(atoms=True)
    #                     if a.name == "select"
    #                 ]

    #             result = ctl.solve(assumptions=assumptions, on_model=on_model)

    #             if not optimal_selected:
    #                 continue

    #             pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
    #             for s in optimal_selected[1:]:
    #                 pos_covered &= self.pos_exs_covered[s]

    #             if not pos_covered.any():
    #                 continue

    #             uncovered &= ~pos_covered
    #             unfolded_prog = self.build_unfolded_program(optimal_selected)
    #             fragments.append([unfolded_prog, frozenbitarray(pos_covered)])
    #             solved = True
    #             break

    #         if not solved:
    #             break

    #     logger.debug(f"number of fragments found with joiner: {len(fragments)}")
    #     return fragments

    def solve_complete(self):
        state = self.state
        fragments = []

        all_progs = list(self.program_selected_var)
        if len(all_progs) < 2:
            return fragments

        sizes = []

        # Build static facts once
        facts = []
        for p in all_progs:
            facts.append(f'program({p}).')
            size = self.progid_to_size[p]
            # prog = self.progid_to_prog[p] # can drop
            # size_ = calc_prog_size(prog.rules) # can drop
            # assert(size == size_) #can drop
            sizes.append(size)
            facts.append(f'size({p}, {size}).')

        # Negative example facts over all programs
        has_neg = False
        hit_neg_union = zeros(len(self.neg_index))
        for p in all_progs:
            hit_neg_union |= self.neg_exs_covered[p]

        feasible = True
        for x in hit_neg_union.search(1):
            missers = [p for p in all_progs if not self.neg_exs_covered[p][x]]
            if not missers:
                feasible = False
                break
            for p in all_progs:
                if self.neg_exs_covered[p][x]:
                    facts.append(f'hits_neg({p},{x}).')
                    has_neg = True

        if not feasible or not has_neg:
            return fragments

        # Positive example facts: covers(P, E) for scoping per iteration
        for p in all_progs:
            for e in self.pos_exs_covered[p].search(1):
                facts.append(f'hits_pos({p},{e}).')

        ASP_COMPLETE = """
        #show select/1.
        2 { select(P) : program(P) }.

        covered_pos(E) :- hits_pos(_, E), hits_pos(P, E) : select(P).
        covered_neg(E) :- hits_neg(_, E), hits_neg(P, E) : select(P).

        :- not covered_pos(_).    % Must cover at least one positive
        :- covered_neg(_).        % Must NOT cover any negative

        selection_differs(H):- consistent_covers(H,_), covered_pos(E), not consistent_covers(H,E).
        :- consistent_covers(H,_), not selection_differs(H).
        """

        encoding = "\n".join(facts) + '\n' + ASP_COMPLETE


        coverage_facts = []
        seen_selected = []
        max_sat_time = 0.0
        max_unsat_time = 0.0
        debug_dir = 'asp_debug'
        os.makedirs(debug_dir, exist_ok=True)

        def dump_slowest_encoding(kind, elapsed, loop_encoding):
            filename = f"{kind.lower()}_{time.strftime('%Y%m%d_%H%M%S')}_{time.time_ns()}.pl"
            path = os.path.join(debug_dir, filename)
            with open(path, 'w') as f:
                f.write(loop_encoding)
            print(f'{kind} max {elapsed:.6f}s {path}')


        sizes = sorted(sizes)
        # min size of a fragment is the two smallest rules - 1 (since we drop a head literal)
        min_size = sizes[0] + sizes[1] - 1
        print(f'MIN_SIZE: {min_size}')

        seen = set()

        for i in range(min_size, self.state.max_literals+1):
            seen_size_atoms = []

            for j in range(1, i+1):
                for old_prog, old_pos_covered in enumerate(self.existing_consistent[j]):
                    k = f'old_{j}_{old_prog}'
                    # print(k, j)
                    for e in old_pos_covered.search(1):
                        seen_size_atoms.append(f'consistent_covers({k},{e}).')

            if not seen_size_atoms:
                continue

            print(f'size: {i} - seen_size_atoms {len(seen_size_atoms)}')

            seen_size_atoms_enc = '\n'.join(seen_size_atoms)
            while True:
                # print('limiting max size', i)
                # "size(N):- #sum{K : selected(P), size(P,K)} == N, N < 10."

                coverage_encoding = '\n'.join(coverage_facts)
                size_con = ":- #sum{K,P : select(P), size(P,K)} != " + str(i) + "."
                seen_selected_enc = '\n'.join(seen_selected)
                loop_encoding = '\n'.join([encoding, seen_size_atoms_enc, coverage_encoding, seen_selected_enc, size_con])
                # loop_encoding = '\n'.join([encoding, coverage_encoding, seen_selected_enc, size_con])

                with open('loop_encoding.pl', 'w') as f:
                    f.write(loop_encoding)

                start_time = time.perf_counter()
                ctl = clingo.Control(['--warn=none'])
                ctl.add("base", [], loop_encoding)
                ctl.ground([("base", [])])

                optimal_selected = []
                def on_model(m):
                    nonlocal optimal_selected
                    optimal_selected = [a.arguments[0].number for a in m.symbols(atoms=True)if a.name == "select"]
                result = ctl.solve(on_model=on_model)
                elapsed = time.perf_counter() - start_time




                # for other in seen:
                #     print(optimal_selected, other)
                #     if other.issubset(seen) or seen.issubset(other):
                #         print('shit')
                #         exit()

                # print('1')
                if not optimal_selected:
                    if elapsed > max_unsat_time:
                        dump_slowest_encoding('UNSAT', elapsed, loop_encoding)
                        max_unsat_time = elapsed
                    break

                if elapsed > max_sat_time:
                    dump_slowest_encoding('SAT', elapsed, loop_encoding)
                    max_sat_time = elapsed

                pos_covered = bitarray(self.pos_exs_covered[optimal_selected[0]].copy())
                for s in optimal_selected[1:]:
                    pos_covered &= self.pos_exs_covered[s]


                optimal_selected_, pos_covered_ = self.remove_redundant_fragments(optimal_selected, pos_covered)
                if len(optimal_selected) != len(optimal_selected_):
                    print('SIZE_DIFF', len(optimal_selected), len(optimal_selected_))

                unfolded_prog = self.build_unfolded_program(optimal_selected)
                unfolded_prog = inline_logic_rules_ast(unfolded_prog, self.head_pred)

                print('found model size', i, optimal_selected,  calc_prog_size(unfolded_prog), tuple(pos_covered.search(1)))

                # print('2')
                if not pos_covered.any():
                    assert(False)
                    exit()
                    break

                k = f'k{i}'

                seen_selected.append(','.join(f'select({i})' for i in optimal_selected) + ".")

                for e in pos_covered.search(1):
                    coverage_facts.append(f'consistent_covers({k},{e}).')

                # print('3')
                seen.add(frozenset(optimal_selected))

                # uncovered &= ~pos_covered
                # unfolded_prog = self.build_unfolded_program(optimal_selected)
                # fragments.append([unfolded_prog, frozenbitarray(pos_covered)])
                # solved = True
                # break

        print('DONE')

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



"""
Enumerate non-subsumed good fragments using Formal Concept Analysis.

A good fragment is a non-empty set S of positive examples such that:
  1. some program selection T (|T| >= 2) has pos-intersection equal to S,
  2. that selection has empty neg-intersection (consistency),
  3. no other good fragment covers a strict superset of S.

The enumeration uses Close-by-One (CbO) over the formal context
    (programs, positive examples, "covers"),
with consistency pruning: if a supporter's neg-intersection is non-empty,
every super-intent inherits a non-empty neg-intersection, so the whole
subtree is cut. A concept is emitted only when no valid extension exists,
i.e. it is a maximal good fragment (the local roof of the consistent
region in the intent lattice).
"""

from bitarray import bitarray, frozenbitarray


def find_good_fragments(program_ids, pos_covers, neg_covers, num_pos,
                        blocked_coverages=None):
    """
    Enumerate maximal good fragments.

    Parameters
    ----------
    program_ids : iterable
        Hashable program identifiers.
    pos_covers : Mapping[id, bitarray]
        Positive-example coverage per program (length num_pos).
    neg_covers : Mapping[id, bitarray]
        Negative-example coverage per program.
    num_pos : int
        Number of positive examples.
    blocked_coverages : Iterable[bitarray], optional
        Positive-coverage sets of already-known good fragments. Any
        coverage S satisfying S ⊆ K for some K in blocked_coverages is
        suppressed (both at emission and via subtree pruning when every
        program in the current extent stays within K).

    Yields
    ------
    (extent, intent) : tuple[tuple, frozenbitarray]
        `extent` is a tuple of program ids whose pos-intersection equals
        `intent` and whose neg-intersection is empty. Only non-subsumed
        fragments are yielded.
    """
    # Filter blocked to maximal elements only (drop any K ⊆ K').
    blocked = []
    if blocked_coverages:
        raw = [bitarray(b) for b in blocked_coverages]
        for i, b in enumerate(raw):
            dominated = False
            for j, c in enumerate(raw):
                if i == j:
                    continue
                # b ⊆ c strictly, or (b == c and j < i) to keep one copy.
                if (b | c) == c and (b != c or j < i):
                    dominated = True
                    break
            if not dominated:
                blocked.append(b)

    def subsumed(S):
        for K in blocked:
            if (S | K) == K:
                return True
        return False

    # Dedup: programs with identical (pos, neg) coverage are interchangeable.
    seen = {}
    unique = []
    for pid in program_ids:
        key = (frozenbitarray(pos_covers[pid]), frozenbitarray(neg_covers[pid]))
        if key not in seen:
            seen[key] = pid
            unique.append(pid)

    if len(unique) < 2:
        return

    def pos_intersect(T):
        it = iter(T)
        acc = bitarray(pos_covers[next(it)])
        for p in it:
            acc &= pos_covers[p]
        return acc

    def neg_intersect(T):
        it = iter(T)
        acc = bitarray(neg_covers[next(it)])
        for p in it:
            acc &= neg_covers[p]
        return acc

    def pos_union(T):
        acc = bitarray(num_pos)
        acc.setall(False)
        for p in T:
            acc |= pos_covers[p]
        return acc

    def generate(T, S, next_e):
        # Invariant: len(T) >= 2 and neg_intersect(T) has no 1s.
        # Subsumption prune: if S ⊆ K for some blocked K and no program in
        # T covers anything outside K, every descendant intent stays ⊆ K,
        # so nothing non-subsumed lives in this subtree.
        if blocked:
            trapping = [K for K in blocked if (S | K) == K]
            if trapping:
                U = pos_union(T)
                if any((U | K) == K for K in trapping):
                    return

        canonical = []
        any_extension = False

        for e in range(num_pos):
            if S[e]:
                continue
            T_e = [p for p in T if pos_covers[p][e]]
            if len(T_e) < 2:
                continue
            if neg_intersect(T_e).any():
                continue
            # At this point T_e is a valid extension (consistent, |T_e| >= 2).
            any_extension = True
            if e < next_e:
                # Non-canonical direction in CbO's lectic order: the
                # corresponding concept is reached via a different parent.
                continue
            S_e = pos_intersect(T_e)
            # Canonicity: closure added no attribute strictly below e.
            is_canonical = True
            for a in range(e):
                if S_e[a] and not S[a]:
                    is_canonical = False
                    break
            if is_canonical:
                canonical.append((e, T_e, S_e))

        # Maximal iff no extension (canonical or otherwise) is consistent.
        if not any_extension and S.any() and not subsumed(S):
            yield tuple(T), frozenbitarray(S)

        for e, T_e, S_e in canonical:
            yield from generate(T_e, S_e, e + 1)

    T0 = list(unique)
    if neg_intersect(T0).any():
        # Bottom of the intent lattice is already inconsistent: every
        # smaller extent has neg-intersection at least as large, so no
        # consistent concept exists anywhere in the lattice.
        return

    S0 = pos_intersect(T0)
    yield from generate(T0, S0, 0)


def _collect_blocked(joiner):
    """Collect all known good-fragment pos-coverages from the joiner."""
    blocked = []
    existing = getattr(joiner, 'existing_consistent', None)
    if existing:
        for covs in existing.values():
            blocked.extend(covs)
    return blocked


def _shrink_selection(U, intent, pos_covers, neg_covers, progid_to_size):
    """
    Greedily drop programs from U while preserving
    (i)  pos-intersection == intent,
    (ii) neg-intersection empty,
    (iii) |T| >= 2.

    Drops the heaviest program first; each drop is kept only if the
    invariants still hold. Runs in O(|U|^2) bitarray ops — cheap
    compared to unfolding and well below MaxSAT cost.
    """
    T = list(U)
    T.sort(key=lambda p: progid_to_size[p], reverse=True)

    def pos_inter(items):
        it = iter(items)
        acc = bitarray(pos_covers[next(it)])
        for p in it:
            acc &= pos_covers[p]
        return acc

    def neg_inter(items):
        it = iter(items)
        acc = bitarray(neg_covers[next(it)])
        for p in it:
            acc &= neg_covers[p]
        return acc

    changed = True
    while changed and len(T) > 2:
        changed = False
        for p in list(T):
            if len(T) <= 2:
                break
            trial = [q for q in T if q != p]
            if neg_inter(trial).any():
                continue
            if pos_inter(trial) != intent:
                continue
            T = trial
            changed = True
    return T


def find_good_fragments_from_joiner(joiner, max_size=None):
    """
    Convenience wrapper: enumerate maximal good fragments from a Joiner's
    accumulated program state, and print the size and coverage of the
    unfolded program for each.

    The closure extent Sup(S) is the *largest* consistent selection; it
    gives the most bloated unfolded program. We shrink it greedily (size
    preserved: any consistent T ⊆ Sup(S) with |T|>=2 has Pos(T)=S because
    S is non-subsumed) before unfolding.

    Parameters
    ----------
    max_size : int, optional
        Drop any fragment whose unfolded size is > max_size. Also skips
        unfolding when a cheap lower bound on the unfolded size already
        exceeds max_size (the two smallest individual program sizes in
        the shrunk selection sum to > max_size + 1, accounting for the
        one-literal overlap from inlining one head).

    Reads `joiner.progid_to_prog`, `joiner.pos_exs_covered`,
    `joiner.neg_exs_covered`, `joiner.progid_to_size`,
    and `joiner.tester.num_pos`.
    """
    from .util import calc_prog_size
    from .joiner import inline_logic_rules_ast

    program_ids = list(joiner.progid_to_prog.keys())
    for extent, intent in find_good_fragments(
        program_ids,
        joiner.pos_exs_covered,
        joiner.neg_exs_covered,
        joiner.tester.num_pos,
        blocked_coverages=_collect_blocked(joiner),
    ):
        selection = _shrink_selection(
            list(extent),
            intent,
            joiner.pos_exs_covered,
            joiner.neg_exs_covered,
            joiner.progid_to_size,
        )
        if max_size is not None:
            sizes = sorted(joiner.progid_to_size[p] for p in selection)
            # Any unfolded program has |T| top-level rules, each inheriting
            # body literals from its defining program; two-program lower
            # bound is the sum of the two smallest sizes minus 1 (one
            # shared invented-head literal).
            if sizes[0] + sizes[1] - 1 > max_size:
                continue
        unfolded = joiner.build_unfolded_program(selection)
        unfolded = inline_logic_rules_ast(unfolded, joiner.head_pred)
        size = calc_prog_size(unfolded)
        if max_size is not None and size > max_size:
            continue
        coverage = tuple(intent.search(1))
        print(f'fragment size={size} coverage={coverage} |T|={len(selection)}')
        yield tuple(selection), intent, unfolded, size


def find_optimal_fragments_from_joiner(joiner, max_size=None):
    """
    For each non-subsumed good coverage set S, enumerate all minimum-size
    fragments realising S.

    A selection T is *good* iff ``Pos(T) != {}`` and ``Neg(T) = {}``.
    A fragment p (the unfolded program built from T) is *optimal for S*
    iff its selection is good, ``Pos(T) = S`` and ``size(p) <= size(q)``
    for every good fragment q with the same coverage S.

    Pipeline:
      1. Close-by-One enumerates the non-subsumed good coverage sets S,
         each with its closure extent ``U = Sup(S)``.
      2. Because S is non-subsumed, any consistent ``T ⊆ U`` with
         ``|T| >= 2`` automatically has ``Pos(T) = S`` (a strict superset
         would contradict S's maximality). So within each U we just need
         minimum-weight consistent selections.
      3. A weighted MaxSAT (RC2) instance encodes per-U:
           - hard: for each neg n covered by some p in U, at least one
             p in U missing n must be selected
           - hard: at least 2 programs selected
           - soft: cost of selecting p is ``size(p) - 1``
             (approximating the unfolded-program size contribution;
             exact size is recomputed after unfolding)
         All models with the optimal cost are enumerated.

    Yields
    ------
    (extent, intent, unfolded, size)
        One tuple per optimal fragment.
    """
    from pysat.examples.rc2 import RC2
    from pysat.formula import WCNF
    from pysat.card import CardEnc, EncType
    from .util import calc_prog_size
    from .joiner import inline_logic_rules_ast

    num_neg = joiner.tester.num_neg
    num_pos = joiner.tester.num_pos
    program_ids = list(joiner.progid_to_prog.keys())

    for extent, intent in find_good_fragments(
        program_ids,
        joiner.pos_exs_covered,
        joiner.neg_exs_covered,
        num_pos,
        blocked_coverages=_collect_blocked(joiner),
    ):
        U = list(extent)
        var = {p: i + 1 for i, p in enumerate(U)}
        top_id = len(U)

        wcnf = WCNF()

        # Consistency: for each negative covered by some p in U, at least
        # one program in U that misses n must be selected.
        for n in range(num_neg):
            missers = [var[p] for p in U if not joiner.neg_exs_covered[p][n]]
            if missers and len(missers) < len(U):
                wcnf.append(missers)

        # |T| >= 2
        card = CardEnc.atleast(
            lits=list(var.values()),
            bound=2,
            top_id=top_id,
            encoding=EncType.seqcounter,
        )
        for cl in card.clauses:
            wcnf.append(cl)

        # Soft: penalty (size_p - 1) per selected program.
        for p, v in var.items():
            w = max(joiner.progid_to_size[p] - 1, 1)
            wcnf.append([-v], weight=w)

        coverage = tuple(intent.search(1))

        # Cost-side prune: min-cost selection is at least (2 smallest
        # progid_to_size[p] - 1 each). If that already exceeds the size
        # budget, skip the solver call.
        if max_size is not None:
            sizes = sorted(joiner.progid_to_size[p] for p in U)
            if len(sizes) < 2 or (sizes[0] - 1) + (sizes[1] - 1) > max_size - 1:
                continue

        with RC2(wcnf) as solver:
            opt_cost = None
            seen = set()
            for model in solver.enumerate():
                cost = solver.cost
                # Cost approximates unfolded size; cap at max_size - 1
                # (the soft weights are size(p) - 1 per selected program).
                if max_size is not None and cost > max_size - 1:
                    break
                if opt_cost is None:
                    opt_cost = cost
                elif cost > opt_cost:
                    break
                model_set = set(model)
                selected = tuple(sorted(p for p, v in var.items() if v in model_set))
                if selected in seen:
                    continue
                seen.add(selected)
                unfolded = joiner.build_unfolded_program(list(selected))
                unfolded = inline_logic_rules_ast(unfolded, joiner.head_pred)
                size = calc_prog_size(unfolded)
                if max_size is not None and size > max_size:
                    continue
                print(f'optimal fragment size={size} coverage={coverage}')
                yield selected, intent, unfolded, size
