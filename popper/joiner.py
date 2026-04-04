# Code and idea from the paper:
# Céline Hocquette, Andreas Niskanen, Rolf Morel, Matti Järvisalo, Andrew Cropper:
# Learning Big Logical Rules by Joining Small Rules. IJCAI 2024: 3430-3438


import collections
from collections import defaultdict
from bitarray import bitarray
from bitarray.util import ones
from bitarray.util import subset
import clingo
from typing import NamedTuple
from functools import reduce
from . tester import TestResult
from . import stats
from . import logger

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

def inline_logic_rules(rules_text, target_predicate):
    # 1. Parse rules into a dictionary
    rules = {}
    pattern = re.compile(r"(\w+)\((.*?)\)\s*:-\s*(.*)\.")
    all_found_vars = set()

    for line in rules_text.strip().split('\n'):
        match = pattern.search(line)
        if not match:
            continue
        head_name, head_args, body_str = match.groups()

        # Parse body literals: name(arg1, arg2)
        body_literals = re.findall(r"(\w+)\((.*?)\)", body_str)
        parsed_body = []
        for p_name, p_args in body_literals:
            args_list = [a.strip() for a in p_args.split(',')]
            parsed_body.append((p_name, args_list))
            all_found_vars.update(args_list)

        args_list = [a.strip() for a in head_args.split(',')]
        rules[head_name] = {'args': args_list, 'body': parsed_body}
        all_found_vars.update(args_list)

    if target_predicate not in rules:
        return f"Error: Predicate '{target_predicate}' not found."

    # 2. Determine the starting index for new variables
    v_ids = [int(v[1:]) for v in all_found_vars if v.startswith('V') and v[1:].isdigit()]
    var_counter = max(v_ids) if v_ids else 0

    # 3. Perform the inlining
    root = rules[target_predicate]
    inlined_literals = []
    current_vars = set(root['args'])

    for pred, args in root['body']:
        if pred in rules and pred != target_predicate:
            sub_rule = rules[pred]

            # Map formal parameters (sub-rule head) to actual parameters (call site)
            mapping = dict(zip(sub_rule['args'], args))

            for sub_pred, sub_args in sub_rule['body']:
                new_sub_args = []
                for sa in sub_args:
                    if sa in mapping:
                        # Map to the variable passed in the call
                        new_sub_args.append(mapping[sa])
                    elif sa.startswith('V'):
                        # Local variable in sub-rule: Check for collision
                        if sa not in mapping:
                            if sa in current_vars:
                                var_counter += 1
                                mapping[sa] = f"V{var_counter}"
                            else:
                                mapping[sa] = sa
                                current_vars.add(sa)
                        new_sub_args.append(mapping[sa])
                    else:
                        # Constants or other types
                        new_sub_args.append(sa)

                inlined_literals.append(f"{sub_pred}({','.join(new_sub_args)})")
        else:
            # Not a rule we can inline, keep as is
            inlined_literals.append(f"{pred}({','.join(args)})")
            current_vars.update([a for a in args if a.startswith('V')])

    # 4. Format and return
    head_str = f"{target_predicate}({','.join(root['args'])})"
    body_str = ",".join(inlined_literals)
    return f"{head_str}:- {body_str}."

PB_ENCODING = 5
CARD_ENCODING = 1

# Returns True iff lit is satisfied in model.
def lit_is_true(model, lit):
    assert(abs(model[abs(lit)-1]) == abs(lit))
    return model[abs(lit)-1] == lit

# joiner builds consistent rules by combining inconsistent rules
# it takes as inputs fragments for which tp>0 and fn>0 and return combinations for which tp>0 and fn=0

# datalog constraints: every variable that appears in the head of the specialisation must also appears in the body of 
# the specialisation
# we save for each fragment the head variables appearing in the body
# for each head variable, at least one fragment which uses this head variable as a body variable must be used
# this contraint is enforced only if the non-datalog flag is off

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

        # the constraints are saved in self.constraints, these are shared by all versions of the joiner 
        # (optimal and suboptimal)
        self.constraints = []
        self.optimal_depth_search = 1
        self.optimal = False

        self.join_minimize = False

        # keep track of the minimum size of programs in the joiner
        # the first program added to the joiner is the one with minimal size
        self.min_size = None

        self.deleted_cover_one = False

        # keep track of the to delete / deleted programs which are beyond the current size bound for the optimal joiner
        self.prog_to_large = collections.defaultdict(list)
        self.prog_deleted = collections.defaultdict(list)


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

        with stats.duration('join'):
            # add all inconsistent programs if we call the suboptimal joiner, otherwise only add
            # programs with size up to prog_size-joiner.min_size
            max_s = prog_size - self.min_size if (self.state.solution_found and self.min_size) else prog_size
            for k in range(max_s + 1):
                for prog_, pos_covered, neg_covered in self.to_join[k]:
                    self.add_program_fragment(prog_, pos_covered, neg_covered)
                self.to_join[k] = []
            # we only run the joiner up to the current program size
            for program_, coverage_ in self.make_consistent_fragments(max_size=prog_size):
                program_ = inline_logic_rules_ast(program_, head_pred)
                logger.out(f'JOIN PROG: {format_prog(program_)}')
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
        if prog.negated:
            adjusted_size = prog_size + 2
        # if the fragment selected has recursion or predicate invention we combine it as a new invented predicate
        # therefore we need to take into account the new body literal added
        elif prog_is_recursive(prog_rules) or prog_has_invention(prog_rules):
            adjusted_size = prog_size + 1
        # otherwise we simply concatenate the body literals, therefore the size is calc_prog_size(prog)-1
        # (we ignore the head literal)
        else:
            adjusted_size = prog_size - 1
        self.progid_to_size[prog_id] = adjusted_size
        self.min_size = adjusted_size if not self.min_size else min(self.min_size, adjusted_size)
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

        # save the programs which are larger than the current depth of optimal search
        # we will delete them for the optimal call
        if adjusted_size > self.optimal_depth_search - self.min_size:
            self.prog_to_large[adjusted_size].append([prog_id, prog, pos_covered, neg_covered])

    # def add_program_fragment(self, prog, pos_covered, neg_covered):
    #     # TODO: check subsumed
    #     if pos_covered.any():
    #         print("pos")
    #         prog_hash = abs(get_prog_hash(prog))
    #         if prog_hash not in self.proghash_to_id:
    #             prog_id = len(self.proghash_to_id) + 1
    #         else:
    #             prog_id = self.proghash_to_id[prog_hash]
    #         assert prog_id >= 0 and prog_hash >= 0
    #         self.add_program_fragment_(prog, prog_hash, prog_id, pos_covered, neg_covered)
    #     if self.settings.negjoin and (~pos_covered).any():
    #         print("neg")
    #         prog_hash = -abs(get_prog_hash(prog))
    #         if prog_hash not in self.proghash_to_id:
    #             prog_id = -len(self.proghash_to_id) - 1
    #         else:
    #             prog_id = self.proghash_to_id[prog_hash]
    #         assert prog_id < 0 and prog_hash < 0
    #         self.add_program_fragment_(prog, prog_hash, prog_id, ~pos_covered, ~neg_covered)

    # def add_program_fragment_(self, prog, prog_hash, k, pos_covered, neg_covered):
    #     print(f"add_program_fragment_ {prog_hash} {k}")
    #     if prog_hash not in self.proghash_to_id:
    #         self.proghash_to_id[prog_hash] = k
    #         self.progid_to_prog[k] = prog
    #         # if the fragment selected is negated we combine it as a new invented predicate
    #         # therefore we need to take into account the new body literal added
    #         if k < 0:
    #             self.progid_to_size[k] = calc_prog_size(prog)+2
    #             if not self.min_size:
    #                 self.min_size = calc_prog_size(prog)+1
    #         # if the fragment selected has recursion or predicate invention we combine it as a new invented predicate
    #         # therefore we need to take into account the new body literal added
    #         elif prog_is_recursive(prog) or prog_has_invention(prog):
    #             self.progid_to_size[k] = calc_prog_size(prog)+1
    #             if not self.min_size:
    #                 self.min_size = calc_prog_size(prog)+1
    #         # otherwise we simply concatenate the body literals, therefore the size is calc_prog_size(prog)-1 
    #         # (we ignore the head literal)
    #         else:
    #             self.progid_to_size[k] = calc_prog_size(prog)-1
    #             if not self.min_size:
    #                 self.min_size = calc_prog_size(prog)-1
    #         assert self.progid_to_size[k] >= self.min_size
    #     prog_id = self.proghash_to_id[prog_hash]
    #     self.pos_exs_covered[prog_id] = pos_covered
    #     self.neg_exs_covered[prog_id] = neg_covered

        # print(format_prog(prog))
        # print(prog_id)
        # print([i for i, ex in enumerate(pos_covered) if ex == 1])
        # print([i for i, ex in enumerate(neg_covered) if ex == 1])


    # delete programs from the joiner
    def delete_progs(self, progs):
        # progs_var = set([self.program_selected_var[p] for p in progs])
        for p in progs:
            del self.program_selected_var[p]
        for ex in self.pos_index:
            self.programs_not_covering_example[ex] = set([p for p in self.programs_not_covering_example[ex] if p not in progs])
            self.programs_covering_example[ex] = set([p for p in self.programs_covering_example[ex] if p not in progs])
        for ex in self.neg_index:
            self.programs_not_covering_example[-ex-1] = set([p for p in self.programs_not_covering_example[-ex-1] if p not in progs])
            self.programs_covering_example[-ex-1] = set([p for p in self.programs_covering_example[-ex-1] if p not in progs])
        if not self.settings.non_datalog:
            for x in self.programs_with_arg:
                self.programs_with_arg[x] = [p for p in self.programs_with_arg[x] if p not in progs]

        # for c in self.constraints_superset:
        #     assert all([abs(x) in set(list(progs_var)+list(self.program_selected_var.values())) for x in c ])
        # self.constraints_superset = [c for c in self.constraints_superset if all([abs(x) not in progs_var for x in c])]


    # update the programs stored in the joiner based on the current size of the search
    def update_progs_in_joiner_size(self, size, only_complete=False):
        max_s = size - self.min_size
        to_delete = set([p for s in range(max_s, max(self.prog_to_large)+1) for p, _, _, _ in self.prog_to_large[s]])
        self.incomplete = self.incomplete.difference(to_delete)
        if only_complete:
            to_delete = to_delete.union(self.incomplete)
            for s in self.prog_to_large:
                self.prog_to_large[s] = [prog for prog in self.prog_to_large[s] if prog[0] not in self.incomplete]
            for s in self.prog_deleted:
                self.prog_deleted[s] = [prog for prog in self.prog_deleted[s] if self.proghash_to_id[get_prog_hash(prog[0])] not in self.incomplete]
            self.incomplete = set()
        self.delete_progs(to_delete)
        
        for s in range(max_s, max(self.prog_to_large)+1):
            # keep track of delete programs as we might need to add them again later
            for p_id, prog, pos_covered, neg_covered in self.prog_to_large[s]:
                self.prog_deleted[self.progid_to_size[p_id]].append([prog, pos_covered, neg_covered])
            self.prog_to_large[s] = []
        for s in range(max_s):
            for prog, pos_covered, neg_covered in self.prog_deleted[s]:
                self.add_program_fragment(prog, pos_covered, neg_covered)
            self.prog_deleted[s] = []
        if not any(self.program_selected_var):
            return False
        else:
            return True

    def add_consistent_program(self, pos_covered, size):
        self.existing_consistent[size].append(~pos_covered)


    # # TODO: can backtrack and remove programs from the joiner?
    def build_base_encoding(self, pbenc=False, optimal=False, examples=None):
        """
        Builds the base SAT encoding for joining rules.
        Returns a tuple containing the generated clauses and the set of valid programs.
        """
        logger.info('build_base_encoding')

        if optimal:
            assert examples is None, "Examples must be None when searching for an optimal solution."

        if examples is None:
            examples = ones(self.tester.num_pos)

        # 1. Determine the universe of programs to consider
        if self.settings.non_datalog:
            # Flatten all programs that cover the given examples into a single set
            all_progs = {
                prog
                for i, _ in enumerate(examples)
                for prog in self.programs_covering_example[i]
            }
        else:
            all_progs = set(self.program_selected_var.keys())

        clauses = []

        # 2. Constraint: Positive Example Coverage
        # If the joined rule is to cover example `i`, NO selected fragment can miss it.
        # Logically: E_i => ~P_p (encoded as ~E_i \/ ~P_p)
        for i in examples.search(1):
            for p in self.programs_not_covering_example[i]:
                if p in all_progs:
                    clauses.append([-self.example_covered_var[i], -self.program_selected_var[p]])

        # 3. Constraint: Negative Example Consistency
        # For each negative example, the joined rule must NOT cover it.
        # Therefore, at least one selected fragment must MISS the negative example.
        for x in self.neg_index:
            neg_ex_key = -x - 1  # Note: Negative examples are stored using negative dictionary keys

            valid_progs_to_miss_neg = [
                self.program_selected_var[p]
                for p in self.programs_not_covering_example[neg_ex_key]
                if p in all_progs
            ]
            clauses.append(valid_progs_to_miss_neg)

        # 4. Constraint: Datalog Safety (Optional)
        # Every variable in the head must appear in at least one selected fragment's body.
        if not self.settings.non_datalog:
            for arg in self.head_args:
                valid_progs_with_arg = [
                    self.program_selected_var[p]
                    for p in self.programs_with_arg[arg]
                    if p in all_progs
                ]
                clauses.append(valid_progs_with_arg)

        self.top = self.vpool.top

        # 5. Constraint: Pseudo-Boolean Encoding (Optional)
        if pbenc:
            with stats.duration('pbenc'):
                max_s = self.optimal_depth_search + 1 if self.optimal else max(self.existing_consistent) + 1

                for s in range(max_s):
                    for pos_covered_complement in self.existing_consistent[s]:
                        clause = [
                            self.example_covered_var[i]
                            for i, val in enumerate(pos_covered_complement)
                            if val == 1
                        ]
                        clauses.append(clause)

        return clauses, all_progs


    def minimize_size(self, pos_covered, timeout=None):
        hard_clauses = self.build_base_encoding(False)
        hard_clauses.extend(self.constraints)
        for ex in pos_covered:
            clause = [self.example_covered_var[ex]]
            hard_clauses.append(clause)
        soft_clauses = []
        weights = []
        for p in self.program_selected_var:
            clause = [-self.program_selected_var[p]]
            soft_clauses.append(clause)
            weights.append(self.progid_to_size[p])
        if timeout is None:
            cost, model = maxsat.exact_maxsat_solve(hard_clauses, soft_clauses, weights, self.settings)
        else:
            cost, model = maxsat.anytime_maxsat_solve(hard_clauses, soft_clauses, weights, self.settings, timeout)

        selected = [p for p in self.program_selected_var if lit_is_true(model, self.program_selected_var[p])]
        return selected

    def build_encoding_suboptimal(self, uncovered, pbenc=False):

        encoding, prog_vars = self.build_base_encoding(optimal=False, examples=uncovered)

        # subsumption constraints
        encoding.extend(self.constraints)
        if pbenc:
            # any combination cannot have a longer size than the maximum number of literals allowed in a hypothesis
            program_lits = [self.program_selected_var[p] for p in self.program_selected_var]
            program_weights = [self.progid_to_size[p] for p in self.program_selected_var]
            if len(program_lits) > 0:
                cnf = PBEnc.atmost(lits=program_lits, weights=program_weights, bound=self.state.max_literals, top_id=self.top, encoding=PB_ENCODING)
                for clause in cnf.clauses:
                    encoding.append(clause)
                    self.top = max(self.top, max([abs(lit) for lit in clause]))

        return encoding, prog_vars

    def build_encoding_optimal(self, max_size=None, pbenc=False):

        with stats.duration('build_build_base_encoding'):
            encoding, _ = self.build_base_encoding(optimal=True, pbenc=pbenc)
        # subsumption constraints

        encoding.extend(self.constraints)

        # since a solution has been found, at least two positive examples must be covered by a new rule
        cnf = CardEnc.atleast(lits=[self.example_covered_var[ex] for ex in self.example_covered_var], bound=2, top_id=self.top, encoding=CARD_ENCODING)
        for clause in cnf.clauses:
            encoding.append(clause)
            self.top = max(self.top, max([abs(lit) for lit in clause]))
        encoding.extend(cnf.clauses)

        # any combination cannot have a longer size than the size of the current best hypothesis
        if self.state.max_literals:
            size_bound = min([self.state.max_literals-1, max_size]) if max_size else self.state.max_literals-1
            program_lits = [self.program_selected_var[p] for p in self.program_selected_var]
            program_weights = [self.progid_to_size[p] for p in self.program_selected_var]
            cnf = PBEnc.atmost(lits=program_lits, weights=program_weights, bound=size_bound-1, top_id=self.top, encoding=PB_ENCODING)
            for clause in cnf.clauses:
                encoding.append(clause)
                self.top = max(self.top, max([abs(lit) for lit in clause]))

            # any incomplete combination cannot have a longer size than the maximum number of literals allowed in a hypothesis minus the min size of
            # rules in the combiner
            # we still search until combiner.min_size as we might need to reduce this value
            size_bound_partial_complete = max([self.state.min_size, self.state.max_literals-self.state.min_size-1])
            if max_size:
                size_bound_partial_complete = min([size_bound_partial_complete, max_size])

            if pbenc and size_bound_partial_complete < size_bound:
                self.top += 1
                incomplete_var = self.top
                # incomplete_var <-> \/_{e in E^+} not covered(e)
                for x in self.pos_index:
                    encoding.append([incomplete_var, self.example_covered_var[x]])
                encoding.append([-incomplete_var] + [-self.example_covered_var[x] for x in self.pos_index])
                program_lits = [self.program_selected_var[p] for p in self.program_selected_var]
                program_weights = [self.progid_to_size[p] for p in self.program_selected_var]
                cnf = PBEnc.atmost(lits=program_lits, weights=program_weights, bound=size_bound_partial_complete-1, top_id=self.top, encoding=PB_ENCODING)
                for clause in cnf.clauses:
                    encoding.append([-incomplete_var] + clause)
                    self.top = max(self.top, max([abs(lit) for lit in clause]))

        return encoding

    # suboptimal encoding: tries to find at least one combination covering each positive example
    def solve_encoding_suboptimal(self, encoding):

        fragments = []
        uncovered = ones(self.tester.num_pos)

        sat_solver = Solver(name="cadical153")
        for clause in encoding:
            sat_solver.add_clause(clause)

        while uncovered.any():
            # print(f"solving {uncovered.count()} uncovered examples")
            clause = [self.example_covered_var[i] for i, x in enumerate(uncovered) if x == 1]
            sat_solver.add_clause(clause)

            model = None
            selected, pos_covered = [], set()
            with stats.duration('join solve suboptimal'):
                assumptions = []
                # subset-maximize coverage in an iterative loop
                # TODO: do this in a restricted way?
                while True:
                    model_found = sat_solver.solve(assumptions)
                    if not model_found:
                        break
                    model = sat_solver.get_model()
                    # assert len(self.pos_index) + len(self.program_selected_var) == len(model)
                    selected = [p for p in self.program_selected_var if lit_is_true(model, self.program_selected_var[p])]
                    pos_covered = set([ex for ex in self.example_covered_var if lit_is_true(model, self.example_covered_var[ex])])
                    # TODO: check break to find max ones
                    assumptions = [self.example_covered_var[ex] for ex in pos_covered]
                    clause = [self.example_covered_var[ex] for ex in self.example_covered_var if not lit_is_true(model, self.example_covered_var[ex])]
                    sat_solver.add_clause(clause)

            if model is None:
                break

            pos_covered = ones(self.tester.num_pos)
            # for all programs selected
            for p in self.program_selected_var:
                if lit_is_true(model, self.program_selected_var[p]):
                    pos_covered = pos_covered & self.pos_exs_covered[p]

            if self.join_minimize:
                assert(False)
                with stats.duration('minimize_size'):
                    selected = self.minimize_size(pos_covered, self.join_minimize_timeout)
            else:
                with stats.duration('remove_redundancy'):
                    selected2 = self.remove_redundant_selected_fragment(selected, pos_covered)
                    assert(selected == selected2)

            #self.constraints.append(clause)

            # selected_fragments = [self.progid_to_prog[f] for f in selected]
            unfolded_prog = self.build_unfolded_program(selected)
            # folded_prog = self.build_folded_program(selected_fragments)
            # combination = Combination(unfolded_prog, folded_prog)
            # print(f"found fragment {format_prog(unfolded_prog)} {pos_covered}")
            fragments.append([unfolded_prog, pos_covered])

            # # Build constraints to prune other models
            # prevent the solver from finding supersets of the rules selected
            no_superset_clause = [-self.program_selected_var[p] for p in selected]
            sat_solver.add_clause(no_superset_clause)
            self.constraints.append(no_superset_clause)

            # prevent the solver from finding combinations which cover a subset of the positive examples covered
            # and have greater size
            if (~pos_covered).count() > 0:
                combination_size = calc_prog_size(unfolded_prog)
            #    constraint_coverage = self.consistent_constraint(pos_covered, combination_size)
            #    self.constraints += constraint_coverage
            #    encoding += constraint_coverage
                self.add_consistent_program(pos_covered, combination_size)

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    @profile
    def solve_encoding_suboptimal_asp(self):
        state = self.state
        # logger.info('solve_encoding_suboptimal_maxsat_asp')

        fragments = []
        uncovered = state.uncovered.copy()

        # print('uncovered',uncovered)

        # Keep track of combinations found in previous loop iterations
        # to prevent the solver from finding supersets of them again.
        # NO NEED I THINK
        # previous_selections = []

        while uncovered.any():
            with stats.duration('join solve suboptimal (ASP)'):


                # Only consider programs that cover at least ONE currently uncovered example
                all_progs = set(self.program_selected_var.keys())

                valid_progs = set([p for p in all_progs if (self.pos_exs_covered[p] & uncovered).any()])

                if len(valid_progs) < 2:
                    return []

                # print('rule filtering\t', len(all_progs), len(valid_progs))

                facts = []

                for p in valid_progs:
                    facts.append(f"size({p}, {self.progid_to_size[p]}).")


                # has_some_neg =
                # Inject Examples and "Miss" facts
                for i in uncovered.search(1):
                    facts.append(f"uncovered({i}).")
                    # if all(p in self.programs_not_covering_example[i] for p in valid_progs):
                    #     print('NOT GOOD')
                    #     assert(False)

                    for p in self.programs_not_covering_example[i]:
                        if p in valid_progs:
                            facts.append(f"misses_pos({p}, {i}).")

                # print('pos example filtering\t', len(uncovered), uncovered.count(1))

                # MORE FILTERING HERE
                # NEVER ADD PROGRAMS THAT ARE SUBSUMED RELATIVE TO THE UNCOVERED POS EXAMPLES

                # count_neg = 0
                # count_neg_filter = 0
                has_neg=False
                for x in self.neg_index:
                    neg_key = -x - 1
                    # count_neg+=1
                    # neg_coverd_by_something_interesting=False
                    for p in self.programs_not_covering_example[neg_key]:
                        # if p in all_progs:
                        if p in valid_progs:
                            facts.append(f"misses_neg({p}, {x}).")
                            has_neg = True
                            # neg_coverd_by_something_interesting = True
                        # elif p in all_progs:
                            # print('skipped something')
                    # if neg_coverd_by_something_interesting:
                        # count_neg_filter +=1
                        # facts.append(f"neg_ex({x}).")

                # print('neg example filtering\t', count_neg, count_neg_filter)

                if not has_neg:
                    # print("POO")
                    return  []
                        # exit()

                # Inject Datalog facts
                if not self.settings.non_datalog:
                    facts.append("datalog_req.")
                    for arg in self.head_args:
                        safe_arg = str(arg).replace('V', 'v')
                        facts.append(f"head_arg({safe_arg}).")
                        for p in self.programs_with_arg[arg]:
                            if p in valid_progs:
                                facts.append(f"has_arg({p}, {safe_arg}).")

                # Inject historical constraints (no supersets)
                # AC: I THINK THIS IS NO LONGER POSSIBLE DUE TO THE SET COVERING IDEA
                # for combo_id, prev_sel in enumerate(previous_selections):
                #     facts.append(f"prev_combo({combo_id}).")
                #     for p in prev_sel:
                #         facts.append(f"combo_prog({combo_id}, {p}).")

                facts = "\n".join(facts)
                encoding = facts + '\n' + ASP_SUB_OPTIMAL

                # print('writing debug')
                # with open('debug.pl', 'w') as f:
                    # f.write(encoding)


                ctl = clingo.Control([])
                ctl.add("base", [], encoding)
                ctl.ground([("base", [])])

                optimal_selected = []
                def on_model(m):
                    nonlocal optimal_selected
                    # Clingo calls this repeatedly as it finds better models.
                    # The last one it finds is the mathematical optimum.
                    optimal_selected = [a.arguments[0].number for a in m.symbols(atoms=True) if a.name == "select"]

                ctl.solve(on_model=on_model)

            # If no model is found, we cannot cover any more examples. Break.
            if not optimal_selected:
                break

            # exit()

            selected = optimal_selected

            # --- POST-PROCESSING (Matching original logic) ---
            # Recompute actual coverage strictly using reduce intersection
            # pos_covered = reduce(lambda a, b: a & b, [self.pos_exs_covered[s] for s in selected])

            pos_covered = bitarray(self.pos_exs_covered[selected[0]].copy())
            # print('POS_COVERED.COUNT', pos_covered.count(1))
            for s in selected[1:]:
                pos_covered &= self.pos_exs_covered[s]

            uncovered = uncovered & ~pos_covered
            assert pos_covered.count() > 0

            # if self.join_minimize:
            #     assert(False)
            #     # WHAT IS THIS?
            #     with stats.duration('minimize_size'):
            #         selected = self.minimize_size(pos_covered, self.join_minimize_timeout)
            # else:
            #     # WHAT IS THIS?
            #     with stats.duration('remove_redundancy'):
            #         selected2 = self.remove_redundant_selected_fragment(selected, pos_covered)
            #         assert(selected == selected2)

            # Re-verify pos_covered after minimization/redundancy removal
            pos_covered = reduce(lambda a, b: a & b, [self.pos_exs_covered[s] for s in selected])

            unfolded_prog = self.build_unfolded_program(selected)
            fragments.append([unfolded_prog, pos_covered])

            # Save this selection to block it in the next loop iteration
            # NO NEED
            # previous_selections.append(selected)

            if (~pos_covered).count() > 0:
                combination_size = calc_prog_size(unfolded_prog)
                self.add_consistent_program(pos_covered, combination_size)

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")
        return fragments

    # suboptimal encoding: tries to find at least one combination covering each positive example
    # minimize size of each combination found
    def solve_encoding_suboptimal_maxsat(self):
        state = self.state
        logger.info('solve_encoding_suboptimal_maxsat')

        fragments = []
        # uncovered = ones(self.tester.num_pos)
        uncovered = state.uncovered

        # print(uncovered, uncovered2)


        while uncovered.any():
            encoding, prog_vars = self.build_encoding_suboptimal(uncovered)
            # print(f"uncovered {[i for i, x in enumerate(uncovered) if x == 1]}")
            # print(f"solving {uncovered.count()} uncovered examples with {len(self.program_selected_var.keys())} programs")
            clause = [self.example_covered_var[i] for i, x in enumerate(uncovered) if x == 1]
            encoding.append(clause)
            soft_clauses = [[self.example_covered_var[x]] for x in self.pos_index]

            model = None
            selected, pos_covered = [], set()
            with stats.duration('join solve suboptimal (MaxSAT)'):
                cost, model = maxsat.exact_maxsat_solve(encoding, soft_clauses, [1 for _ in range(self.tester.num_pos)])

            if model is None:
                break
            pos_covered = ones(self.tester.num_pos)
            # for all programs selected
            for p in prog_vars:
                if lit_is_true(model, self.program_selected_var[p]):
                    pos_covered = pos_covered & self.pos_exs_covered[p]

            uncovered = uncovered & ~pos_covered
            assert pos_covered.count() > 0
            selected = [p for p in self.program_selected_var if lit_is_true(model, self.program_selected_var[p])]

            if self.join_minimize:
                with stats.duration('minimize_size'):
                    selected = self.minimize_size(pos_covered, self.join_minimize_timeout)
            else:
                with stats.duration('remove_redundancy'):
                    selected = self.remove_redundant_selected_fragment(selected, pos_covered)

            pos_covered = reduce(lambda a, b: a & b, [self.pos_exs_covered[s] for s in selected])
            selected_fragments = [self.progid_to_prog[f] for f in selected]
            unfolded_prog = self.build_unfolded_program(selected)
            folded_prog = self.build_folded_program(selected_fragments)
            # combination = Combination(unfolded_prog, folded_prog)
            # print(f"found fragment {format_prog(unfolded_prog)}")
            fragments.append([unfolded_prog, pos_covered])

            # print('START found fragment folded_prog')
            # print(f"{format_prog(folded_prog)}")
            # print('END')

            # Build constraints to prune other models
            # prevent the solver from finding supersets of the rules selected
            no_superset_clause = [-self.program_selected_var[p] for p in selected]
            encoding.append(no_superset_clause)
            self.constraints.append(no_superset_clause)

            # prevent the solver from finding combinations which cover a subset of the positive examples covered
            # and have greater size
            if (~pos_covered).count() > 0:
                combination_size = calc_prog_size(unfolded_prog)
            #    constraint_coverage = self.consistent_constraint(pos_covered, combination_size)
            #    self.constraints += constraint_coverage
            #    encoding += constraint_coverage
                self.add_consistent_program(pos_covered, combination_size)

        logger.debug(f"number of fragments found with joiner: {len(fragments)}")

        return fragments


    # optimal encoding
    def solve_encoding_optimal(self, encoding):

        fragments = []
        count = 0

        sat_solver = Solver(name="cadical153")
        for clause in encoding:
            sat_solver.add_clause(clause)

        while True:

            model = None
            selected, pos_covered = [], set()
            with stats.duration('join solve optimal'):
                assumptions = []
                # subset-maximize coverage in an iterative loop
                # TODO: do this in a restricted way?
                while True:
                    model_found = sat_solver.solve(assumptions)
                    if not model_found:
                        break
                    model = sat_solver.get_model()
                    assert max(list(self.program_selected_var.values())) < len(model)
                    # assert len(self.pos_index) + len(self.program_selected_var) == len(model)
                    selected = [p for p in self.program_selected_var if lit_is_true(model, self.program_selected_var[p])]
                    pos_covered = reduce(lambda a, b: a & b, [self.pos_exs_covered[s] for s in selected])
                    assumptions = [self.example_covered_var[ex] for i, ex in enumerate(pos_covered) if ex == 1]
                    clause = [self.example_covered_var[ex] for ex in self.example_covered_var if not lit_is_true(model, self.example_covered_var[ex])]
                    sat_solver.add_clause(clause)

            if model is None:
                break

            count += 1
            self.constraints.append(clause)

            # if self.join_minimize:
            #     selected = self.minimize_size(pos_covered, self.join_minimize_timeout)

            if self.join_minimize:
                selected = self.minimize_size(pos_covered, self.join_minimize_timeout)

            # selected_fragments = [self.progid_to_prog[f] for f in selected]
            unfolded_prog = self.build_unfolded_program(selected)
            # folded_prog = self.build_folded_program(selected_fragments)
            # combination = Combination(unfolded_prog, folded_prog)
            fragments.append([unfolded_prog, pos_covered])
            # print(f"found fragment {format_prog(unfolded_prog)}")

            # Build constraints to prune other models
            # prevent the solver from finding supersets of the rules selected
            no_superset_clause = [-self.program_selected_var[p] for p in selected]
            sat_solver.add_clause(no_superset_clause)
            self.constraints.append(no_superset_clause)

            # prevent the solver from finding combinations which a subset of the positive examples covered
            if (~pos_covered).count() > 0:
                combination_size = calc_prog_size(unfolded_prog)
            #    constraint_coverage = self.consistent_constraint(pos_covered, combination_size)
            #    self.constraints += constraint_coverage
            #    encoding += constraint_coverage
                self.add_consistent_program(pos_covered, combination_size)

        return fragments


    def make_consistent_fragments(self, min_size=None, max_size=None):
        # print('make_consistent_fragments', min_size, max_size)
        if not min_size:
            min_size = self.optimal_depth_search
        else:
            min_size = max(min_size, self.optimal_depth_search)

        # if we do not yet have a solution, only try to find at least one fragment which cover each positive example
        if not self.state.solution_found:
            with stats.duration('solve_encoding_subopt'):
                # return self.solve_encoding_suboptimal_maxsat()
                return self.solve_encoding_suboptimal_asp()

        # TMP!!!
        return []

        # otherwise find all possible fragments (up to some size), as we look for an optimal solution
        # else:
        if True:
            self.optimal = True
            if not self.deleted_cover_one:
                # delete programs which cover only one positive example when we switch from suboptimal to optimal joiner
                with stats.duration('delete_cover_one'):
                    self.delete_progs(self.programs_covering_one_positive)
                    for s in self.prog_to_large:
                        self.prog_to_large[s] = [[prog_id, prog, pos_covered, neg_covered] for [prog_id, prog, pos_covered, neg_covered] in self.prog_to_large[s] if prog_id not in self.programs_covering_one_positive]       
                    self.deleted_cover_one = True
            models = []
            for size in range(min_size, max_size+1):
                with stats.duration('update_progs_in_joiner_size'):
                    if not self.state.min_size or not self.state.max_literals:
                        max_size_incomplete = None
                    else:
                        max_size_incomplete = max([self.state.min_size, self.state.max_literals - self.state.min_size])
                    only_complete = False if (not max_size_incomplete or size < max_size_incomplete) else True
                    has_programs = self.update_progs_in_joiner_size(size, only_complete=only_complete)
                if not has_programs:
                    continue
                with stats.duration('build_encoding_opt'):
                    model = self.build_encoding_optimal(max_size=size)
                with stats.duration('solve_encoding_opt'):
                    model = self.solve_encoding_optimal(model)
                    models.extend(model)
                    # if self.settings.onlyaftergenerate and size < self.settings.max_body+1:
                    #     assert len(model) == 0
            self.optimal_depth_search = max(self.optimal_depth_search, max_size+1)
            return models

    def remove_redundant_selected_fragment(self, selected, pos_covered):
        if len(selected) == 1:
            return selected
        for i in range(len(selected)):
            subselected = selected[:i]+selected[i+1:]
            if self.break_datalog(subselected):
                continue
            # if (set.intersection(*[set(self.pos_exs_covered[s]) for s in subselected]).issubset(pos_covered) and set.intersection(*[set(self.neg_exs_covered[s]) for s in subselected]) == set()):
            if set.intersection(*[set(self.neg_exs_covered[s]) for s in subselected]) == set():
                return self.remove_redundant_selected_fragment(subselected, pos_covered)
        return selected

    def break_datalog(self, subselected):
        if self.settings.non_datalog:
            return False
        for x in self.programs_with_arg:
            if not set(subselected).intersection(set(self.programs_with_arg[x])):
                return True
        return False

    def build_folded_program(self, fragments):
        program, top_level_body = [], []
        for fragment_prog in fragments:
            rules, literal = self.build_invented(fragment_prog)
            program.extend(rules)
            top_level_body.append(literal)
        top_level_head = Literal(self.head_pred, self.head_args)
        program.append(tuple((top_level_head, frozenset(top_level_body))))
        return program

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



def inline_logic_rules_prog(program_, target_predicate):
    """
    Inlines invented predicates in a program_ frozenset into the target predicate's rule.
    Returns a frozenset containing the single inlined rule as (head Literal, frozenset body Literals).
    """
    rules = {}
    for head, body in program_:
        rules[head.predicate] = (head, list(body))

    if target_predicate not in rules:
        return None

    # Find the highest variable index to avoid collisions when minting fresh vars.
    max_var = max(
        a
        for head, body in program_
        for lit in [head] + list(body)
        for a in lit.arguments
        if isinstance(a, int)
    )
    var_counter = max_var

    root_head, root_body = rules[target_predicate]
    inlined_body = []

    for lit in root_body:
        if lit.predicate in rules and lit.predicate != target_predicate:
            sub_head, sub_body = rules[lit.predicate]

            mapping = dict(zip(sub_head.arguments, lit.arguments))

            for sub_lit in sub_body:
                new_args = []
                for a in sub_lit.arguments:
                    if a in mapping:
                        new_args.append(mapping[a])
                    else:
                        var_counter += 1
                        mapping[a] = var_counter
                        new_args.append(var_counter)
                inlined_body.append(Literal(sub_lit.predicate, tuple(new_args)))
        else:
            inlined_body.append(lit)

    return frozenset({(root_head, frozenset(inlined_body))})


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