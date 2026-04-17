from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, get_raw_prog, Literal, canonicalise, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions
from bitarray import bitarray
from bitarray.util import subset, any_and, ones, zeros
from itertools import chain, combinations, permutations
from . import stats


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

        # per-checker counters for debugging the new bounds
        self.bound_stats = {
            'calls': 0,
            'keep_full_cover': 0,
            'keep_search_depth': 0,
            'prune_min_size': 0,
            'joiner_calls': 0,
            'forced_total_rules': 0,
            'forced_calls_with_commit': 0,
            'forced_prune': 0,
            'forced_covers_U': 0,
            'post_forced_min_size_prune': 0,
            'bound_A_prune': 0,
            'bound_B_prune_beyond_A': 0,
            'bound_C_prune_beyond_AB': 0,
            'card_refined': 0,
            'card_prune_beyond_old': 0,
            'enum_prune': 0,
            'enum_prune_beyond_old_givenup': 0,
            'enum_keep': 0,
        }

    def _dbg(self, msg):
        if SUBSUME_DEBUG:
            print(f'[SUBSUME] {msg}')

    def dump_bound_stats(self):
        """Print aggregate counters of how often each bound fired."""
        print('[SUBSUME] -------- aggregate bound stats --------')
        width = max(len(k) for k in self.bound_stats)
        for k, v in self.bound_stats.items():
            print(f'[SUBSUME]   {k.ljust(width)}  {v}')

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

            if not head_connected(new_rule) or not has_valid_directions(new_rule) or self.tester.has_redundant_literal(new_prog):
                out.update(self.subsumed_or_covers_too_few(new_prog, seen))
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = self.tester.get_pos_covered(new_prog)

            if self.settings.joiner:
                subsumed = sub_prog_pos_covered in state.success_sets and state.success_sets[sub_prog_pos_covered] <= new_prog_size
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
                out.add((new_prog, 'SUBSUMED (GENERALISATION)'))
            elif subsumed_by_two:
                out.add((new_prog, 'SUBSUMED BY TWO (GENERALISATION)'))
            else:
                out.add((new_prog, 'COVERS TOO FEW (GENERALISATION)'))

        return out

    def check_subsumed_by_two(self, pos_covered, prog_size):
        for i in range(2, prog_size + 1):
            if pos_covered in self.state.paired_success_sets[i]:
                return True
            for x in self.state.paired_success_sets[i]:
                if subset(pos_covered, x):
                    return True
        return False

    # -------------------------------------------------------------------
    # Debug log tags emitted by check_covers_too_few:
    #
    #   FORCED committed N rules                   -> propagation fired
    #   FORCED infeasible                          -> OLD WOULD NOT CATCH
    #   FORCED covers U                            -> keep, no enum needed
    #   PRUNE Bound-A  (M > S)                     -> fair-old catches this too
    #   PRUNE Bound-B  (lb_B > S, M <= S)          -> OLD WOULD NOT CATCH
    #   PRUNE Bound-C  (lb_C > S, A & B don't)     -> OLD WOULD NOT CATCH
    #   CARD refined   (old_max_k -> new_max_k)    -> tighter enumeration depth
    #   CARD prune     (new_max_k < 1)             -> may go beyond old
    #   ENUM prune                                 -> residual enum failed
    #   ENUM prune beyond old_max_k >= 4           -> OLD WOULD NOT CATCH
    #                                                 (propagation shrank the
    #                                                  problem into enum range)
    # -------------------------------------------------------------------
    def check_covers_too_few(self, prog_size, pos_covered, prog):
        self.bound_stats['calls'] += 1
        call_id = self.bound_stats['calls']

        num_pos = self.tester.num_pos

        if pos_covered.count(1) == num_pos:
            self.bound_stats['keep_full_cover'] += 1
            return False

        max_literals = self.state.max_literals
        space_remaining = max_literals - prog_size

        if self.tester.test_prog_inconsistent(prog):
            space_remaining -= 1

        if space_remaining >= self.state.search_depth:
            self.bound_stats['keep_search_depth'] += 1
            return False

        min_size = self.state.min_size

        if min_size > space_remaining:
            self.bound_stats['prune_min_size'] += 1
            return True

        uncovered = self.tester.pos_examples_ & ~pos_covered
        pre_U_card = uncovered.count(1)

        affordable_all = sorted(
            ((size, cov) for cov, size in self.state.success_sets.items()
             if size <= space_remaining),
            key=lambda x: x[0]
        )

        if self.settings.joiner:
            self.bound_stats['joiner_calls'] += 1
            max_additional_rules = space_remaining // min_size
            if max_additional_rules < 1:
                return True
            return self._enumerate_extensions(
                uncovered, space_remaining, affordable_all, min_size,
                max_additional_rules, bottleneck=None
            )

        # ---- (1) Forced-set propagation ----
        S, U, pruned, forced_count = self._forced_propagation(
            space_remaining, uncovered, affordable_all
        )
        post_U_card = U.count(1)
        committed_cost = space_remaining - S

        if forced_count > 0:
            self.bound_stats['forced_calls_with_commit'] += 1
            self.bound_stats['forced_total_rules'] += forced_count
            self._dbg(
                f'#{call_id} FORCED committed {forced_count} rules, '
                f'space {space_remaining}->{S} (cost {committed_cost}), '
                f'|U| {pre_U_card}->{post_U_card}'
            )

        if pruned:
            self.bound_stats['forced_prune'] += 1
            self._dbg(
                f'#{call_id} PRUNE forced-propagation infeasible '
                f'(OLD WOULD NOT CATCH: no propagation in old code)'
            )
            return True

        if not U.any():
            self.bound_stats['forced_covers_U'] += 1
            if forced_count > 0:
                self._dbg(f'#{call_id} KEEP forced-propagation covered U entirely')
            return False

        if S < min_size:
            self.bound_stats['post_forced_min_size_prune'] += 1
            self._dbg(
                f'#{call_id} PRUNE post-forced S={S} < min_size={min_size}'
                + (' (OLD WOULD NOT CATCH: propagation created the gap)'
                   if forced_count > 0 else '')
            )
            return True

        affordable = [
            (size, cov) for size, cov in affordable_all
            if size <= S and (cov & U).any()
        ]
        if not affordable:
            self._dbg(f'#{call_id} PRUNE no affordable rule intersects residual U')
            return True

        # ---- (2) Bound A: per-example true minimum ----
        U_indices = list(U.search(1))
        m = {}
        for size, cov in affordable:
            inter = U & cov
            if not inter.any():
                continue
            for e in inter.search(1):
                if e not in m:
                    m[e] = size

        if any(e not in m for e in U_indices):
            self._dbg(f'#{call_id} PRUNE some e in residual U has no affordable cover')
            return True

        M = max(m[e] for e in U_indices)
        if M > S:
            self.bound_stats['bound_A_prune'] += 1
            self._dbg(f'#{call_id} PRUNE Bound-A M={M} > S={S} (fair-old catches this too)')
            return True

        # ---- (3) Bound B: rule-disjoint independent set ----
        neighbourhood = {e: None for e in U_indices}
        for size, cov in affordable:
            inter = cov & U
            if not inter.any():
                continue
            for e in inter.search(1):
                if neighbourhood[e] is None:
                    neighbourhood[e] = bitarray(inter)
                else:
                    neighbourhood[e] |= inter

        excluded = zeros(num_pos)
        I = []
        for e in sorted(U_indices, key=lambda x: -m[x]):
            if excluded[e]:
                continue
            I.append(e)
            excluded |= neighbourhood[e]

        lb_B = sum(m[e] for e in I)
        if lb_B > S:
            self.bound_stats['bound_B_prune_beyond_A'] += 1
            self._dbg(
                f'#{call_id} PRUNE Bound-B lb_B={lb_B} > S={S} '
                f'(|I|={len(I)}, m_I={[m[e] for e in I]}, '
                f'Bound-A M={M} <= S, OLD WOULD NOT CATCH)'
            )
            return True

        # ---- (4) Bound C: LP-style sum / Delta ----
        total_m = sum(m[e] for e in U_indices)
        Delta = max((cov & U).count(1) for _, cov in affordable)
        lb_C = -(-total_m // Delta) if Delta > 0 else 0
        if lb_C > S:
            self.bound_stats['bound_C_prune_beyond_AB'] += 1
            self._dbg(
                f'#{call_id} PRUNE Bound-C lb_C={lb_C} > S={S} '
                f'(sum_m={total_m}, Delta={Delta}; '
                f'A M={M}, B lb_B={lb_B} both <= S, OLD WOULD NOT CATCH)'
            )
            return True

        # cardinality comparison: old fair bottleneck bound vs new refined one
        old_max_k = 1 + (S - M) // min_size
        new_max_k = len(I) + (S - lb_B) // min_size

        if new_max_k < old_max_k:
            self.bound_stats['card_refined'] += 1
            self._dbg(
                f'#{call_id} CARD refined: old_max_k={old_max_k} -> '
                f'new_max_k={new_max_k} (|I|={len(I)}, M={M}, lb_B={lb_B})'
            )

        if new_max_k < 1:
            if old_max_k >= 1:
                self.bound_stats['card_prune_beyond_old'] += 1
                self._dbg(
                    f'#{call_id} PRUNE cardinality new_max_k={new_max_k} '
                    f'(OLD WOULD NOT CATCH: old_max_k={old_max_k})'
                )
            else:
                self._dbg(
                    f'#{call_id} PRUNE cardinality new_max_k={new_max_k} '
                    f'(old would also catch)'
                )
            return True

        # ---- Exact enumeration on residual (U, S) ----
        result = self._enumerate_extensions(
            U, S, affordable, min_size, new_max_k, bottleneck=M
        )
        if result:
            self.bound_stats['enum_prune'] += 1
            if old_max_k >= 4 and new_max_k <= 3:
                self.bound_stats['enum_prune_beyond_old_givenup'] += 1
                self._dbg(
                    f'#{call_id} PRUNE enum at residual max_k={new_max_k} '
                    f'(OLD WOULD NOT CATCH: old_max_k={old_max_k}>=4 would have kept)'
                )
            elif forced_count > 0:
                self._dbg(
                    f'#{call_id} PRUNE enum at residual max_k={new_max_k} '
                    f'after {forced_count} forced commits'
                )
        else:
            self.bound_stats['enum_keep'] += 1
        return result

    def _forced_propagation(self, space_remaining, uncovered, affordable_all):
        S = space_remaining
        U = bitarray(uncovered)
        forced_count = 0

        while True:
            current = [(size, cov) for size, cov in affordable_all if size <= S]

            cover_count = {}
            unique_cover = {}
            for size, cov in current:
                inter = U & cov
                if not inter.any():
                    continue
                for e in inter.search(1):
                    c = cover_count.get(e, 0) + 1
                    cover_count[e] = c
                    if c == 1:
                        unique_cover[e] = (size, cov)
                    elif c == 2:
                        unique_cover[e] = None

            for e in U.search(1):
                if cover_count.get(e, 0) == 0:
                    return S, U, True, forced_count

            seen_keys = set()
            forced = []
            for e, rule in unique_cover.items():
                if rule is None:
                    continue
                size, cov = rule
                key = (size, cov.tobytes())
                if key in seen_keys:
                    continue
                seen_keys.add(key)
                forced.append(rule)

            if not forced:
                return S, U, False, forced_count

            for size, cov in forced:
                S -= size
                forced_count += 1
                if S < 0:
                    return S, U, True, forced_count
                U &= ~cov
                if not U.any():
                    return S, U, False, forced_count

    def _enumerate_extensions(self, U, S, affordable, min_size, max_k, bottleneck=None):
        if max_k >= 4:
            return False

        M = bottleneck if (bottleneck is not None and bottleneck > min_size) else min_size

        succ = [(cov, size) for size, cov in affordable]
        n = len(succ)

        def need(chosen_sizes):
            if M > min_size and all(s < M for s in chosen_sizes):
                return M
            return min_size

        if max_k == 1:
            for cov2, size2 in succ:
                if size2 > S:
                    break
                if subset(U, cov2):
                    return False
            return True

        if max_k == 2:
            for i in range(n):
                cov2, size2 = succ[i]
                if size2 > S:
                    break
                if subset(U, cov2):
                    return False
                S_ = S - size2
                if S_ < need([size2]):
                    continue
                U2 = U & ~cov2
                if not U2.any():
                    return False
                for j in range(i + 1, n):
                    cov3, size3 = succ[j]
                    if size3 > S_:
                        break
                    if subset(U2, cov3):
                        return False
            return True

        # max_k == 3
        for i in range(n):
            cov2, size2 = succ[i]
            if size2 > S:
                break
            if subset(U, cov2):
                return False
            S_ = S - size2
            if S_ < need([size2]):
                continue
            U2 = U & ~cov2
            if not U2.any():
                return False
            for j in range(i + 1, n):
                cov3, size3 = succ[j]
                if size3 > S_:
                    break
                if subset(U2, cov3):
                    return False
                S__ = S_ - size3
                if S__ < need([size2, size3]):
                    continue
                U3 = U2 & ~cov3
                if not U3.any():
                    return False
                for k in range(j + 1, n):
                    cov4, size4 = succ[k]
                    if size4 > S__:
                        break
                    if subset(U3, cov4):
                        return False
        return True

    def find_variants(self, rule):
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
            yield frozenset(new_body)