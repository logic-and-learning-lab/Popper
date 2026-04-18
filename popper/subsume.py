from itertools import permutations

from bitarray import bitarray
from bitarray.util import subset, zeros

from . import stats
from .util import (
    Constraint,
    Literal,
    calc_prog_size,
    canonicalise,
    connected,
    format_literal,
    format_prog,
    format_rule,
    generalisations,
    get_raw_prog,
    has_valid_directions,
    head_connected,
    mdl_score,
    non_empty_powerset,
    prog_has_invention,
    prog_is_recursive,
    rule_is_recursive,
    theory_subsumes,
    timeout,
)
from .weighted_set_cover import solve_weighted_set_cover_cp_sat

# ==========================================================================
# Debug flag: set to False to silence the [SUBSUME] logging completely.
# Aggregate counters (self.bound_stats) are updated regardless, so you can
# still call checker.dump_bound_stats() at the end of a run.
# ==========================================================================
SUBSUME_DEBUG = False


class SubsumeChecker:
    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state
        self.tmp = {}
        self.tmp2 = set()
        self.pruned2 = set()

    def subsumed_or_covers_too_few(self, prog, seen=None):
        # print('subsumed_or_covers_too_few', format_prog(prog))
        if seen is None:
            seen = set()

        head, body = next(iter(prog))

        if len(body) < 2:
            # print('return []')
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

            if not self.settings.non_datalog_flag and not any(
                x in head_vars for literal in new_body for x in literal.arguments
            ):
                continue

            if any(
                hash(frozenset(x)) in self.pruned2 for x in non_empty_powerset(new_body)
            ):
                continue

            # if not head_connected(new_rule) or not has_valid_directions(new_rule) or self.tester.has_redundant_literal(new_prog):
            #     out.update(self.subsumed_or_covers_too_few(new_prog, seen))
            #     continue

            if not head_connected(new_rule) or not has_valid_directions(new_rule):
                out.update(self.subsumed_or_covers_too_few(new_prog, seen))
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = self.tester.get_pos_covered(new_prog)

            if self.settings.joiner:
                subsumed = (sub_prog_pos_covered in state.success_sets and state.success_sets[sub_prog_pos_covered] <= new_prog_size)
                subsumed = subsumed or any(size <= new_prog_size and subset(sub_prog_pos_covered, xs) for xs, size in state.success_sets.items())
            else:
                subsumed = sub_prog_pos_covered in state.success_sets or any(subset(sub_prog_pos_covered, xs) for xs in state.success_sets)

            subsumed_by_two = not subsumed and self.check_subsumed_by_two(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and self.check_covers_too_few(new_prog_size, sub_prog_pos_covered, new_prog)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, seen)
            if xs:
                out.update(xs)
                continue

            for x in self.find_variants(canonicalise(new_rule)):
                self.pruned2.add(hash(x))

            if subsumed:
                out.add((new_prog, "SUBSUMED (GENERALISATION)"))
            elif subsumed_by_two:
                out.add((new_prog, "SUBSUMED BY TWO (GENERALISATION)"))
            else:
                out.add((new_prog, "COVERS TOO FEW (GENERALISATION)"))

        return out

    def check_subsumed_by_two(self, pos_covered, prog_size):
        for i in range(2, prog_size + 1):
            if pos_covered in self.state.paired_success_sets[i]:
                return True
            for x in self.state.paired_success_sets[i]:
                if subset(pos_covered, x):
                    return True
        return False

    def check_covers_too_few(self, prog_size, pos_covered, prog):
        # a = self.check_covers_too_few_v3(prog_size, pos_covered, prog, with_inconsistent=True, with_min_size=True, with_forced=True)
        return self.check_covers_too_few_v3(prog_size, pos_covered, prog, with_inconsistent=True, with_min_size=True, with_forced=True)

    def _forced_propagation(
        self,
        space_remaining: int,
        uncovered: bitarray,
        affordable_all: list[tuple[int, bitarray]],
    ) -> tuple[int, bitarray, bool, int]:
        """
        Constraint propagation (similar to unit propagation) for the set cover problem.
        Identifies rules that MUST be in any valid hypothesis because they are the only
        ones covering a specific example.

        Args:
            space_remaining: Current budget of literals.
            uncovered: Bitarray of positive examples yet to be covered.
            affordable_all: List of (size, coverage) tuples representing consistent rules that fit within the initial budget.

        Returns:
            S: Updated budget after committing to forced rules.
            U: Updated uncovered examples.
            pruned: True if it's impossible to cover all examples within the budget.
            forced_count: Number of rules forced and committed.
        """
        S = space_remaining
        U = bitarray(uncovered)
        forced_count = 0

        while True:
            # Filter affordable rules for the CURRENT remaining budget S
            current = [(size, cov) for size, cov in affordable_all if size <= S]

            cover_count = {}  # example index -> number of rules that cover it
            unique_cover = {}  # example index -> (size, cov) of the only rule that covers it
            for size, cov in current:
                inter = U & cov
                if not inter.any():
                    continue
                # For each example covered by this rule
                for e in inter.search(1):
                    c = cover_count.get(e, 0) + 1
                    cover_count[e] = c
                    if c == 1:
                        # First time seeing this example, potential unique cover
                        unique_cover[e] = (size, cov)
                    elif c == 2:
                        # Second time seeing it, not unique anymore
                        unique_cover[e] = None

            # 1. PRUNING: Check for any example that CANNOT be covered
            for e in U.search(1):
                if cover_count.get(e, 0) == 0:
                    # No affordable rule covers this uncovered example
                    return S, U, True, forced_count

            # 2. PROPAGATION: Find examples covered by only ONE rule
            seen_keys = set()
            forced = []
            for _e, rule in unique_cover.items():
                if rule is None:
                    continue
                size, cov = rule
                # Deduplicate forced rules (multiple examples might force the same rule)
                key = (size, cov.tobytes())
                if key in seen_keys:
                    continue
                seen_keys.add(key)
                forced.append(rule)

            # If no propagation is possible, stop iterating
            if not forced:
                return S, U, False, forced_count

            # 3. COMMITMENT: Add forced rules to the hypothesis
            for size, cov in forced:
                S -= size
                forced_count += 1
                if S < 0:
                    # Forced rules exceed the budget
                    return S, U, True, forced_count
                U &= ~cov  # Update residual uncovered examples
                if not U.any():
                    # All examples covered
                    return S, U, False, forced_count


    def find_variants(self, rule):
        head, body = rule
        _head_pred, head_args = head
        head_arity = len(head_args)
        body_vars = frozenset(
            x for literal in body for x in literal.arguments if x >= head_arity
        )
        subset_vars = range(head_arity, self.settings.max_vars)
        for xs in permutations(subset_vars, len(body_vars)):
            xs = head_args + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (pred, new_args)
                new_body.append(new_literal)
            yield frozenset(new_body)

    # given a new program found by the generate stage, this method determines whether the program could be used to find a better (smaller) hypothesis than the current best
    # return value of False means the program is still useful
    # return value of True means the program cannot be in a better hypothesis
    # here we assume no noise and search for a hypothesis that covers all the pos examples, none of the neg examples, and is minimal in size (literals)
    def check_covers_too_few_v3(
        self, prog_size, pos_covered, prog, with_inconsistent=False, with_min_size=False, with_forced=False):
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
        if with_inconsistent:
            inconsistent = self.tester.test_prog_inconsistent(prog)
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

        # NEW IDEA 2
        if with_min_size and not self.settings.joiner:

            # find the smallest existing rule that covers an example not coverd by this program
            best_size_for_ex = [None] * self.tester.num_pos
            for cov, size in sorted(self.state.success_sets.items(), key=lambda x: x[1]):
                overlap = uncovered & cov
                if not overlap.any():
                    continue
                for e in overlap.search(1):
                    if best_size_for_ex[e] is None:
                        best_size_for_ex[e] = size
            # we update the minimum size rule we ever need to learn
            valid_sizes = [x for x in best_size_for_ex if x is not None]
            if not valid_sizes:
                return True # Cannot cover all examples with existing rules, prune!
            min_uncovered_size = max(valid_sizes)

            # then update the max possible additional rules
            max_additional_rules = (1 + (space_remaining - min_uncovered_size) // min_size)
        else:
            min_uncovered_size = min_size

        # try to skip again early
        if max_additional_rules < 1:
            return True

        # new idea 3
        # this idea is to find an uncovered example that is covered by only one rule, which must therefore be in the hypothesis
        if with_forced:
            affordable_all = sorted(((size, cov) for cov, size in self.state.success_sets.items() if size <= space_remaining), key=lambda x: x[0])
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
        success_sets = sorted(self.state.success_sets.items(), key=lambda x: x[1])
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

                    # tmp = min_size
                    # if size2 < min_uncovered_size and size3 < min_uncovered_size:
                    #     tmp = min_uncovered_size
                    # if space_remaining_ < tmp:
                    #     print('SKIP2')
                    #     continue

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
                    print("SKIP3")
                    continue

                # assume prog2 is in the hypothesis, so derive a new uncovered set
                uncovered2 = uncovered & ~pos_covered2

                # loop through other programs that are known to be consistent
                for j in range(i + 1, n):
                    # j denotes prog3
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break

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
                        if subset(uncovered3, pos_covered4):
                            return False

            # if we get here, the program cannot be useful
            return True

        return False