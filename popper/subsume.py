from itertools import permutations
from bitarray import bitarray
from bitarray.util import subset, zeros, any_and
from bisect import bisect_right
from operator import itemgetter
from . import stats
from .util import (
    Constraint,
    calc_prog_size,
    canonicalise,
    has_valid_directions,
    head_connected,
    non_empty_powerset,
)

class SubsumeChecker:
    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state
        self.pruned2 = set()
        self._covers_too_few_true_cache = set()
        self._cached_success_sets = None
        self._cached_sizes = None
        self._cached_counts = None
        self._cached_success_sets_version = -1

    def _get_sorted_success_sets(self):
        if self.state.success_sets_version != self._cached_success_sets_version:
            items = tuple(sorted(self.state.success_sets.items(), key=itemgetter(1)))
            self._cached_success_sets = items
            self._cached_sizes = tuple(size for _, size in items)
            self._cached_counts = tuple(cov.count(1) for cov, _ in items)
            self._cached_success_sets_version = self.state.success_sets_version
        return self._cached_success_sets, self._cached_sizes, self._cached_counts

    def check_subsumed(self, pos_covered, prog_size):
        success_sets = self.state.success_sets

        if self.settings.joiner:
            # Joiner can learn larger rules than ones we have seen, so take program size into account.
            return ((pos_covered in success_sets and success_sets[pos_covered] <= prog_size) or any(size <= prog_size and subset(pos_covered, xs) for xs, size in success_sets.items()))

        return pos_covered in success_sets or any(subset(pos_covered, xs) for xs in success_sets)

    def subsumed_or_covers_too_few(self, prog, seen=None):
        if seen is None:
            seen = set()

        head, body = next(iter(prog))

        if len(body) < 2:
            return []

        state = self.state
        out = set()
        head_vars = set(head.arguments)

        for atom in body:
            new_body = body - {atom}

            if new_body in seen:
                continue
            seen.add(new_body)

            new_rule = (head, new_body)
            new_prog = frozenset({new_rule})

            if not self.settings.non_datalog_flag and not any(x in head_vars for literal in new_body for x in literal.arguments):
                continue

            if any(hash(frozenset(x)) in self.pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule) or not has_valid_directions(new_rule, self.settings):
                out.update(self.subsumed_or_covers_too_few(new_prog, seen))
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = self.tester._test_prog_pos(new_prog)
            subsumed = self.check_subsumed(sub_prog_pos_covered, new_prog_size)
            subsumed_by_two = not subsumed and self.check_subsumed_by_two(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and self.check_covers_too_few(new_prog_size, sub_prog_pos_covered, new_prog)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, seen)
            if xs:
                out.update(xs)
                continue

            self.pruned2.update(self.find_variant_hashes(canonicalise(new_rule)))

            if subsumed:
                out.add((new_prog, "SUBSUMED (GENERALISATION)"))
            elif subsumed_by_two:
                out.add((new_prog, "SUBSUMED BY TWO (GENERALISATION)"))
            else:
                out.add((new_prog, "COVERS TOO FEW (GENERALISATION)"))

        return out




    def check_subsumed_by_two(self, pos_covered, prog_size):
        """
        Key optimisation: symmetry breaking.

        For a valid pair (P1, P2) with sizes sorted ascending (i < j means
        sizes[i] <= sizes[j]), we need sizes[i] + sizes[j] <= prog_size.
        Since sizes[j] >= sizes[i], we have 2*sizes[i] <= prog_size, i.e.
        sizes[i] <= prog_size // 2.  The outer loop therefore only needs to
        iterate items up to that boundary instead of all items up to prog_size.
        On real data this shrinks the outer loop from O(N) to O(few items).

        Secondary optimisation: popcount pruning.
        If counts[j] (total bits in P2) < uncovered_count, P2 cannot possibly
        cover all uncovered bits, so skip the subset() call.
        """
        success_sets, sizes, counts = self._get_sorted_success_sets()
        full_hi = bisect_right(sizes, prog_size)
        sym_hi  = bisect_right(sizes, prog_size // 2)

        for i in range(sym_hi):
            p2, s2 = success_sets[i]
            # from bitarray.util import
            # if not (p2 & pos_covered).any():
            if not any_and(p2, pos_covered):
                continue
            uncovered = pos_covered & ~p2
            unc_count = uncovered.count()
            inner_hi = bisect_right(sizes, prog_size - s2, lo=i + 1, hi=full_hi)
            for j in range(i + 1, inner_hi):
                if counts[j] < unc_count:
                    continue
                if subset(uncovered, success_sets[j][0]):
                    return True
        return False

    def check_covers_too_few(self, prog_size, pos_covered, prog):
        inconsistent = self.tester.test_prog_inconsistent(prog)

        if not self.settings.joiner:
            key = (prog_size, pos_covered, inconsistent)
            if key in self._covers_too_few_true_cache:
                return True

        res  = self.check_covers_too_few_v3(prog_size, pos_covered, inconsistent)

        if res and not self.settings.joiner:
            self._covers_too_few_true_cache.add(key)

        return res

    def _forced_propagation(
        self,
        space_remaining: int,
        uncovered: bitarray,
        affordable_all: list[tuple[int, bitarray]],
    ) -> tuple[int, bitarray, bool, int]:
        """
        Constraint propagation (similar to unit propagation) for the set cover problem.
        Identifies rules that MUST be in any valid hypothesis because they are the only
        ones covering a specific example. Uses fast C-backed bitwise operations.

        Args:
            space_remaining: Current budget of literals.
            uncovered: Bitarray of positive examples yet to be covered.
            affordable_all: List of (size, coverage) tuples representing consistent rules that fit within the initial budget.

        Returns:
            remaining_budget: Updated budget after committing to forced rules.
            remaining_uncovered: Updated uncovered examples.
            pruned: True if it's impossible to cover all examples within the budget.
            forced_count: Number of rules forced and committed.
        """
        remaining_budget = space_remaining
        remaining_uncovered = uncovered.copy()
        forced_count = 0
        num_examples = len(remaining_uncovered)

        while True:
            # Filter affordable rules for the CURRENT remaining budget
            current_affordable = [(size, coverage) for size, coverage in affordable_all if size <= remaining_budget]
            
            # Bitarrays to track example coverage frequencies
            covered_once = zeros(num_examples)
            covered_multiple = zeros(num_examples)
            
            # Efficiently compute which examples are covered exactly once vs multiple times
            for size, coverage in current_affordable:
                overlap = remaining_uncovered & coverage
                if overlap.any():
                    # If an example is already in covered_once, it now moves to covered_multiple
                    covered_multiple |= (covered_once & overlap)
                    # Mark all overlapping examples as covered at least once
                    covered_once |= overlap
                    
            # 1. PRUNING: Check for any example that CANNOT be covered by any affordable rule
            # If there's a bit set in remaining_uncovered that is NOT set in covered_once, it's impossible to cover.
            if any_and(remaining_uncovered, ~covered_once):
                return remaining_budget, remaining_uncovered, True, forced_count
                
            # 2. PROPAGATION: Find examples covered by EXACTLY ONE rule
            exactly_once = (covered_once & ~covered_multiple) & remaining_uncovered
            
            # If no examples are uniquely covered, we can't force any more rules
            if not exactly_once.any():
                return remaining_budget, remaining_uncovered, False, forced_count
                
            # Identify the specific rules that uniquely cover these examples
            forced_rules = []
            for size, coverage in current_affordable:
                if any_and(exactly_once, coverage):
                    forced_rules.append((size, coverage))
                    # Deduplicate: removing this rule's coverage from exactly_once ensures
                    # we don't count the same rule multiple times if it uniquely covers multiple examples
                    exactly_once &= ~coverage
                    if not exactly_once.any():
                        break
                        
            # 3. COMMITMENT: Add forced rules to the hypothesis and update state
            for size, coverage in forced_rules:
                remaining_budget -= size
                forced_count += 1
                if remaining_budget < 0:
                    # Forced rules exceed the remaining budget
                    return remaining_budget, remaining_uncovered, True, forced_count
                remaining_uncovered &= ~coverage  # Update residual uncovered examples
                if not remaining_uncovered.any():
                    # All examples successfully covered
                    return remaining_budget, remaining_uncovered, False, forced_count

    def find_variant_hashes(self, rule):
        head, body = rule
        _head_pred, head_args = head
        head_arity = len(head_args)
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head_arity)
        subset_vars = range(head_arity, self.settings.max_vars)
        for xs in permutations(subset_vars, len(body_vars)):
            xs = head_args + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (pred, new_args)
                new_body.append(new_literal)
            yield hash(frozenset(new_body))

    # given a new program found by the generate stage, this method determines whether the program could be used to find a better (smaller) hypothesis than the current best
    # return value of False means the program is still useful
    # return value of True means the program cannot be in a better hypothesis
    # here we assume no noise and search for a hypothesis that covers all the pos examples, none of the neg examples, and is minimal in size (literals)
    def check_covers_too_few_v3(self, prog_size, pos_covered, inconsistent):
        num_pos = self.tester.num_pos

        # if the program covers all the examples, then
        if pos_covered.count(1) == num_pos:
            return False

        max_literals = self.state.max_literals

        # if we are here, we assume we need this new prog and we now look whether it can only be used with an existing rule

        # determine how much more space there is to improve after using this new prog
        space_remaining = max_literals - prog_size

        # NEW IDEA 1
        # if prog is inconsistent, we need at least one more literal to specialise the prog, so the available space remaining goes down by 1
        # if with_inconsistent:
        # inconsistent = self.tester.test_prog_inconsistent(prog)
        if inconsistent:
            space_remaining -= 1

        # if we could still find a good rule with the available space, then prog could still be good
        if space_remaining >= self.state.search_depth:
            return False

        # the smallest consistent rule we have seen
        min_size = self.state.min_size

        # if we need more space than the smallest rule then new prog is not good
        if min_size > space_remaining:
            return True

        # determine how many additional rules we could use
        max_additional_rules = (space_remaining) // min_size

        # if no space, return
        if max_additional_rules < 1:
            return True

        # we now try to find the smallest rule for an example not covered by this prog
        uncovered = self.tester.pos_examples_ & ~pos_covered

        # sorted_success_sets = sorted(self.state.success_sets.items(), key=lambda x: x[1])
        sorted_success_sets, _, _ = self._get_sorted_success_sets()

        # NEW IDEA 2
        if not self.settings.joiner:
            # best_size_for_ex = [None] * self.tester.num_pos
            # # for cov, size in sorted(self.state.success_sets.items(), key=lambda x: x[1]):
            # for cov, size in sorted_success_sets:
            #     overlap = uncovered & cov
            #     if not overlap.any():
            #         continue
            #     for e in overlap.search(1):
            #         if best_size_for_ex[e] is None:
            #             best_size_for_ex[e] = size
            # # we update the minimum size rule we ever need to learn
            # valid_sizes = [x for x in best_size_for_ex if x is not None]
            # if not valid_sizes:
            #     return True # Cannot cover all examples with existing rules, prune!
            # min_uncovered_size = max(valid_sizes)

            # find the smallest existing rule that covers an example not coverd by this program
            still_uncovered = uncovered.copy()
            min_uncovered_size = 0
            for cov, size in sorted_success_sets:
                if any_and(still_uncovered, cov):
                    min_uncovered_size = size
                    still_uncovered &= ~cov
                    if not still_uncovered.any():
                        break
            if still_uncovered.any():
                return True # Cannot cover all examples, prune!

            # then update the max possible additional rules
            max_additional_rules = (1 + (space_remaining - min_uncovered_size) // min_size)
        else:
            min_uncovered_size = min_size

        # try to skip again early
        if max_additional_rules < 1:
            return True

        # new idea 3
        # this idea is to find an uncovered example that is covered by only one rule, which must therefore be in the hypothesis
        # if with_forced:
        # affordable_all = sorted(((size, cov) for cov, size in self.state.success_sets.items() if size <= space_remaining), key=lambda x: x[0])

        affordable_all = [(size, cov) for cov, size in sorted_success_sets if size <= space_remaining]
        new_budget, new_uncovered, pruned, forced_count = self._forced_propagation(space_remaining, uncovered, affordable_all)

        if pruned:
            return True

        # if any examples are forced, then we update the space, uncovered, and
        if forced_count:
            # print('SOMETHING FORCED!!!!!!!!', forced_count)
            # print('\t space:', space_remaining, new_budget)
            # print('\t uncovered:', uncovered.count(1), new_uncovered.count(1))
            uncovered = new_uncovered
            space_remaining = new_budget
            max_additional_rules = (space_remaining) // min_size

        # check whether everything is covered
        if not uncovered.any():
            return False

        # MAX TOTAL RULES = 1
        # if this prog + the minimum size prog is too big then this prog must be a single rule hypothesis
        if max_additional_rules < 1:
            # if so, this prog must cover all the positive examples, which it does not (see above check)
            return True

        # AC: COULD FILTR HERE BY SPACE SPACE_REMAINING
        success_sets = sorted_success_sets
        n = len(success_sets)

        # MAX TOTAL RULES = 2
        # this prog must be used with an existing prog
        # if so, there must be a rule that covers all the uncovered examples
        if max_additional_rules == 1:
            for pos_covered2, size2 in success_sets:
                if size2 > space_remaining:
                    break
                if subset(uncovered, pos_covered2):
                    return False
            # if we get here, the program cannot be useful
            return True

        # MAX TOTAL RULES = 3
        # this prog must be used with 1 or 2 existing progs
        # we try to determine if it is possible
        if max_additional_rules == 2:
            # loop through other programs that are known to be consistent
            for i in range(n):
                # i denotes prog2
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break

                if not any_and(pos_covered2, uncovered):
                    continue

                # if prog2  covers the uncovered examples, then prog can be good
                if subset(uncovered, pos_covered2):
                    return False

                # assume prog2 is in the hypothesis, so deduct its size
                space_remaining_ = space_remaining - size2

                if space_remaining_ < min_size:
                    continue

                # TMP!!!!
                tmp = min_size
                if size2 < min_uncovered_size:
                    tmp = min_uncovered_size
                if space_remaining_ < tmp:
                    # print("SKIP1")
                    continue

                # assume prog2 is in the hypothesis, so derive a new uncovered set
                uncovered2 = uncovered & ~pos_covered2

                # loop through other programs that are known to be consistent
                for j in range(i + 1, n):
                    # j denotes prog3
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break

                    if not any_and(pos_covered3, uncovered2):
                        continue

                    # if prog3  covers the uncovered examples, then prog can be good
                    if subset(uncovered2, pos_covered3):
                        return False

            # if we get here, the program cannot be useful
            return True

        # MAX TOTAL RULES = 4
        # this prog must be used with 1 or 2 or 3 existing progs
        # we try to determine if it is possible
        if max_additional_rules == 3:
            # loop through other programs that are known to be consistent
            for i in range(n):
                # i denotes prog2
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break

                if not any_and(pos_covered2, uncovered):
                    continue

                # if prog2  covers the uncovered examples, then prog can be good
                if subset(uncovered, pos_covered2):
                    return False

                # assume prog2 is in the hypothesis, so deduct its size
                space_remaining_ = space_remaining - size2

                if space_remaining_ < min_size:
                    continue

                # TMP!!!!
                tmp = min_size
                if size2 < min_uncovered_size:
                    tmp = min_uncovered_size
                if space_remaining_ < tmp:
                    # print("SKIP3")
                    continue

                # assume prog2 is in the hypothesis, so derive a new uncovered set
                uncovered2 = uncovered & ~pos_covered2

                # loop through other programs that are known to be consistent
                for j in range(i + 1, n):
                    # j denotes prog3
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break

                    if not any_and(pos_covered3, uncovered2):
                        continue

                    # if prog3  covers the uncovered examples, then prog can be good
                    if subset(uncovered2, pos_covered3):
                        return False

                    # assume prog3 is in the hypothesis, so deduct its size
                    space_remaining__ = space_remaining_ - size3

                    if space_remaining__ < min_size:
                        continue

                    tmp = min_size
                    if size2 < min_uncovered_size and size3 < min_uncovered_size:
                        tmp = min_uncovered_size
                    if space_remaining__ < tmp:
                        # print("SKIP4")
                        continue

                    # assume prog3 is in the hypothesis, so derive a new uncovered set
                    uncovered3 = uncovered2 & ~pos_covered3

                    for k in range(j + 1, n):
                        pos_covered4, size4 = success_sets[k]
                        if size4 > space_remaining__:
                            break

                        if not any_and(pos_covered4, uncovered3):
                            continue

                        if subset(uncovered3, pos_covered4):
                            return False

            # if we get here, the program cannot be useful
            return True

        return False

    # def check_covers_too_few_v4(self, prog_size, pos_covered, prog, with_inconsistent=False, with_min_size=False, with_forced=False):
    #     num_pos = self.tester.num_pos

    #     # if the program covers all the examples, then
    #     if pos_covered.count(1) == num_pos:
    #         return False

    #     max_literals = self.state.max_literals

    #     # if we are here, we assume we need this new prog and we now look whether it can only be used with an existing rule

    #     # determine how much more space there is to improve after using this new prog
    #     space_remaining = max_literals - prog_size


    #     # NEW IDEA 1
    #     # if prog is inconsistent, we need at least one more literal to specialise the prog, so the available space remaining goes down by 1
    #     if with_inconsistent:
    #         inconsistent = self.tester.test_prog_inconsistent(prog)
    #         if inconsistent:
    #             space_remaining -= 1

    #     # if we could still find a good rule with the available space, then prog could still be good
    #     if space_remaining >= self.state.search_depth:
    #         return False

    #     # the smallest consistent rule we have seen
    #     min_size = self.state.min_size

    #     # if we need more space than the smallest rule then new prog is not good
    #     if min_size > space_remaining:
    #         return True

    #     uncovered = self.tester.pos_examples_ & ~pos_covered

    #     affordable_all = sorted(((size, cov) for cov, size in self.state.success_sets.items() if size <= space_remaining), key=lambda x: x[0])

    #     # NEW IDEA 2: Independent Set and Delta Bounds
    #     if with_min_size and not self.settings.joiner:
    #         min_rule_size_for_ex = {}
    #         shared_rule_neighbourhood = {}
    #         max_rule_coverage = 0
            
    #         for size, cov in affordable_all:
    #             overlap = uncovered & cov
    #             overlap_count = overlap.count(1)
                
    #             if overlap_count == 0:
    #                 continue
                    
    #             if overlap_count > max_rule_coverage:
    #                 max_rule_coverage = overlap_count
                    
    #             for e in overlap.search(1):
    #                 # min_rule_size_for_ex[e] is the minimum cost to cover e
    #                 if e not in min_rule_size_for_ex or size < min_rule_size_for_ex[e]:
    #                     min_rule_size_for_ex[e] = size
                        
    #                 # shared_rule_neighbourhood[e] tracks examples it shares affordable rules with
    #                 if e not in shared_rule_neighbourhood:
    #                     shared_rule_neighbourhood[e] = bitarray(overlap)
    #                 else:
    #                     shared_rule_neighbourhood[e] |= overlap

    #         # 1. Impossible to cover check: if any uncovered example has no affordable rule
    #         if any(e not in min_rule_size_for_ex for e in uncovered.search(1)):
    #             return True

    #         # 2. Delta Bound (LP relaxation)
    #         sum_of_min_costs = sum(min_rule_size_for_ex[e] for e in uncovered.search(1))
    #         lower_bound_delta = -(-sum_of_min_costs // max_rule_coverage) if max_rule_coverage > 0 else 0
    #         if lower_bound_delta > space_remaining:
    #             return True

    #         # 3. Independent Set Bound (Rule-Disjointness)
    #         rule_disjoint_examples = []
    #         covered_by_disjoint_set = zeros(self.tester.num_pos)
            
    #         # Greedily pick hardest-to-cover examples first
    #         for e in sorted(uncovered.search(1), key=lambda x: min_rule_size_for_ex[x], reverse=True):
    #             if not covered_by_disjoint_set[e]:
    #                 rule_disjoint_examples.append(e)
    #                 covered_by_disjoint_set |= shared_rule_neighbourhood[e]
                    
    #         disjoint_set_cost = sum(min_rule_size_for_ex[e] for e in rule_disjoint_examples)
    #         if disjoint_set_cost > space_remaining:
    #             return True
                
    #         max_additional_rules = len(rule_disjoint_examples) + (space_remaining - disjoint_set_cost) // min_size
    #     else:
    #         rule_disjoint_examples = []
    #         min_rule_size_for_ex = {}
    #         max_additional_rules = space_remaining // min_size

    #     if max_additional_rules < 1:
    #         return True

    #     # new idea 3
    #     # this idea is to find an uncovered example that is covered by only one rule, which must therefore be in the hypothesis
    #     if with_forced:
    #         new_budget, new_uncovered, pruned, forced_count = self._forced_propagation(space_remaining, uncovered, affordable_all)

    #         if pruned:
    #             return True

    #         # if any examples are forced, then we update the space, uncovered, and
    #         if forced_count:
    #             uncovered = new_uncovered
    #             space_remaining = new_budget
    #             max_additional_rules = (space_remaining) // min_size

    #         # check whether everything is covered
    #         if not uncovered.any():
    #             return False

    #     # MAX TOTAL RULES = 1
    #     # if this prog + the minimum size prog is too big then this prog must be a single rule hypothesis
    #     if max_additional_rules < 1:
    #         # if so, this prog must cover all the positive examples, which it does not (see above check)
    #         return True

    #     # AC: COULD FILTR HERE BY SPACE SPACE_REMAINING
    #     success_sets = sorted(self.state.success_sets.items(), key=lambda x: x[1])
    #     n = len(success_sets)

    #     # MAX TOTAL RULES = 2
    #     # this prog must be used with an existing prog
    #     # if so, there must be a rule that covers all the uncovered examples
    #     if max_additional_rules == 1:
    #         for pos_covered2, size2 in success_sets:
    #             if size2 > space_remaining:
    #                 break
    #             if subset(uncovered, pos_covered2):
    #                 return False
    #         # if we get here, the program cannot be useful
    #         return True

    #     # MAX TOTAL RULES = 3
    #     # this prog must be used with 1 or 2 existing progs
    #     # we try to determine if it is possible
    #     if max_additional_rules == 2:
    #         # loop through other programs that are known to be consistent
    #         for i in range(n):
    #             # i denotes prog2
    #             pos_covered2, size2 = success_sets[i]
    #             if size2 > space_remaining:
    #                 break

    #             # if prog2  covers the uncovered examples, then prog can be good
    #             if subset(uncovered, pos_covered2):
    #                 return False

    #             # assume prog2 is in the hypothesis, so deduct its size
    #             space_remaining_ = space_remaining - size2

    #             if space_remaining_ < min_size:
    #                 continue

    #             # TIGHTER BOUND: Residual Independent Set Cost
    #             if rule_disjoint_examples:
    #                 residual_cost = sum(min_rule_size_for_ex[e] for e in rule_disjoint_examples if not pos_covered2[e])
    #                 if residual_cost > space_remaining_:
    #                     continue

    #             # assume prog2 is in the hypothesis, so derive a new uncovered set
    #             uncovered2 = uncovered & ~pos_covered2

    #             # loop through other programs that are known to be consistent
    #             for j in range(i + 1, n):
    #                 # j denotes prog3
    #                 pos_covered3, size3 = success_sets[j]
    #                 if size3 > space_remaining_:
    #                     break

    #                 # if prog3  covers the uncovered examples, then prog can be good
    #                 if subset(uncovered2, pos_covered3):
    #                     return False

    #         # if we get here, the program cannot be useful
    #         return True

    #     # MAX TOTAL RULES = 4
    #     # this prog must be used with 1 or 2 or 3 existing progs
    #     # we try to determine if it is possible
    #     if max_additional_rules == 3:
    #         # loop through other programs that are known to be consistent
    #         for i in range(n):
    #             # i denotes prog2
    #             pos_covered2, size2 = success_sets[i]
    #             if size2 > space_remaining:
    #                 break

    #             # if prog2  covers the uncovered examples, then prog can be good
    #             if subset(uncovered, pos_covered2):
    #                 return False

    #             # assume prog2 is in the hypothesis, so deduct its size
    #             space_remaining_ = space_remaining - size2

    #             if space_remaining_ < min_size:
    #                 continue

    #             # TIGHTER BOUND: Residual IS Cost after 1 rule
    #             if rule_disjoint_examples:
    #                 residual_cost_after_1 = sum(min_rule_size_for_ex[e] for e in rule_disjoint_examples if not pos_covered2[e])
    #                 if residual_cost_after_1 > space_remaining_:
    #                     continue

    #             # assume prog2 is in the hypothesis, so derive a new uncovered set
    #             uncovered2 = uncovered & ~pos_covered2

    #             # loop through other programs that are known to be consistent
    #             for j in range(i + 1, n):
    #                 # j denotes prog3
    #                 pos_covered3, size3 = success_sets[j]
    #                 if size3 > space_remaining_:
    #                     break

    #                 # if prog3  covers the uncovered examples, then prog can be good
    #                 if subset(uncovered2, pos_covered3):
    #                     return False

    #                 # assume prog3 is in the hypothesis, so deduct its size
    #                 space_remaining__ = space_remaining_ - size3

    #                 if space_remaining__ < min_size:
    #                     continue

    #                 # TIGHTER BOUND: Residual IS Cost after 2 rules
    #                 if rule_disjoint_examples:
    #                     residual_cost_after_2 = sum(min_rule_size_for_ex[e] for e in rule_disjoint_examples if not pos_covered2[e] and not pos_covered3[e])
    #                     if residual_cost_after_2 > space_remaining__:
    #                         continue

    #                 # assume prog3 is in the hypothesis, so derive a new uncovered set
    #                 uncovered3 = uncovered2 & ~pos_covered3

    #                 for k in range(j + 1, n):
    #                     pos_covered4, size4 = success_sets[k]
    #                     if size4 > space_remaining__:
    #                         break
    #                     if subset(uncovered3, pos_covered4):
    #                         return False

    #         # if we get here, the program cannot be useful
    #         return True

    #     return False
