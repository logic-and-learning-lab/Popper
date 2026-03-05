from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions
from bitarray.util import subset, any_and, ones
from itertools import chain, combinations, permutations
class SubsumeChecker:
    
    def __init__(self, settings, tester, state):
        self.settings = settings
        self.tester = tester
        self.state = state
        self.tmp = {}
        self.pruned2 = set()

    def subsumed_or_covers_too_few(self, prog, seen=None):
        if seen is None:
            seen = set()

        head, body = list(prog)[0]
        body = list(body)

        if len(body) == 0:
            return []

        out = set()
        head_vars = set(head.arguments)

        for i in range(len(body)):
            new_body = body[:i] + body[i + 1:]
            new_body = frozenset(new_body)

            if len(new_body) == 0:
                continue

            k1 = frozenset(new_body)
            if k1 in seen:
                continue
            seen.add(k1)

            new_rule = (head, new_body)
            new_prog = frozenset({new_rule})

            # ensure at least one head variable is in the body
            if not self.settings.non_datalog_flag and not any(x in head_vars for literal in new_body for x in literal.arguments):
                continue

            # check whether we have pruned any subset (HORRIBLE CODE)
            if any(hash(frozenset(x)) in self.pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if not has_valid_directions(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if self.tester.has_redundant_literal(new_prog):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = self.tester.get_pos_covered(new_prog)

            subsumed = sub_prog_pos_covered in self.state.success_sets or any(subset(sub_prog_pos_covered, xs) for xs in self.state.success_sets)
            subsumed_by_two = not subsumed and self.check_subsumed_by_two(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and self.check_covers_too_few(new_prog_size, sub_prog_pos_covered)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, seen)
            if len(xs) > 0:
                out.update(xs)
                continue

            for x in self.find_variants(remap_variables(new_rule)):
                self.pruned2.add(hash(x))

            if subsumed:
                out.add((new_prog, 'SUBSUMED (GENERALISATION)'))
            elif subsumed_by_two:
                out.add((new_prog, 'SUBSUMED BY TWO (GENERALISATION)'))
            elif covers_too_few:
                out.add((new_prog, 'COVERS TOO FEW (GENERALISATION)'))
            else:
                assert False

        return out

    def check_subsumed_by_two(self, pos_covered, prog_size):
        for i in range(2, prog_size + 2):
            if pos_covered in self.state.paired_success_sets[i]:
                return True
            for x in self.state.paired_success_sets[i]:
                if subset(pos_covered, x):
                    return True
        return False

    def check_subsumed_by_two_v2(self, prog_size, prog2_size, pos_covered, pos_covered2):
        space = prog2_size - prog_size

        if space < self.state.min_size:
            return False
        uncovered = pos_covered2 & ~pos_covered

        for xs, size in self.state.success_sets.items():
            if size > space:
                continue
            if subset(xs, uncovered):
                return True
        return False

    def check_covers_too_few(self, prog_size, pos_covered):
        k1 = hash((prog_size, pos_covered))
        if k1 in self.tmp:
            v = self.tmp[k1]
            if v:
                return True

        k2 = hash((prog_size, pos_covered, self.state.max_literals, self.state.search_depth))
        if k2 in self.tmp:
            return self.tmp[k2]

        v = self.check_covers_too_few_(prog_size, pos_covered)

        self.tmp[k1] = v
        self.tmp[k2] = v
        return v

    def check_covers_too_few_(self, prog_size, pos_covered):
        # nonlocal self.state.min_size
        len_pos_covered = pos_covered.count(1)
        if len_pos_covered == self.tester.num_pos:
            return False

        max_literals = self.state.max_literals

        # MAX RULES = 1
        if (prog_size + self.state.min_size) > max_literals:
            return True

        # MAX RULES = 2
        if (prog_size + (self.state.min_size * 2)) > max_literals:
            space_remaining = max_literals - prog_size
            if space_remaining > self.state.search_depth:
                return False

            uncovered = self.tester.pos_examples_ & ~pos_covered
            for pos_covered2, size2 in self.state.success_sets.items():
                if size2 > space_remaining:
                    continue
                if subset(uncovered, pos_covered2):
                    return False
            return True

        # MAX RULES = 3
        if (prog_size + (self.state.min_size * 3)) > max_literals:
            space_remaining = max_literals - prog_size

            if space_remaining - self.state.min_size > self.state.search_depth:
                return False

            uncovered = self.tester.pos_examples_ & ~pos_covered
            self.state.success_sets_sorted = sorted(((pos_covered_, size) for (pos_covered_, size) in self.state.success_sets.items()),
                                         key=lambda x: x[1])
            n = len(self.state.success_sets_sorted)

            for i in range(n):
                pos_covered2, size2 = self.state.success_sets_sorted[i]
                if size2 > space_remaining:
                    break
                if subset(uncovered, pos_covered2):
                    return False
                space_remaining_ = space_remaining - size2
                if space_remaining_ < self.state.min_size:
                    continue
                uncovered2 = uncovered & ~pos_covered2
                for j in range(i + 1, n):
                    pos_covered3, size3 = self.state.success_sets_sorted[j]
                    if size3 > space_remaining_:
                        break
                    if subset(uncovered2, pos_covered3):
                        return False
            return True

        # MAX RULES = 4
        if prog_size + (self.state.min_size * 4) > max_literals:
            space_remaining = max_literals - prog_size
            space_remaining -= (self.state.min_size * 2)

            if space_remaining > self.state.search_depth:
                return False

            missing = self.tester.pos_examples_ & ~pos_covered

            self.state.success_sets_sorted = sorted(((pos_covered_, size) for (pos_covered_, size) in self.state.success_sets.items()),
                                         key=lambda x: x[1])
            space_remaining = max_literals - prog_size

            n = len(self.state.success_sets_sorted)

            for i in range(n):
                pos_covered2, size2 = self.state.success_sets_sorted[i]
                if size2 > space_remaining:
                    break
                if subset(missing, pos_covered2):
                    return False
                space_remaining_ = space_remaining - size2
                if space_remaining_ < self.state.min_size:
                    continue
                missing2 = missing & ~pos_covered2
                for j in range(i + 1, n):
                    pos_covered3, size3 = self.state.success_sets_sorted[j]
                    if size3 > space_remaining_:
                        break
                    if subset(missing2, pos_covered3):
                        return False
                    space_remaining__ = space_remaining_ - size3
                    if space_remaining__ < self.state.min_size:
                        continue
                    missing3 = missing2 & ~pos_covered3
                    for k in range(j + 1, n):
                        pos_covered4, size4 = self.state.success_sets_sorted[k]
                        if size4 > space_remaining__:
                            break
                        if subset(missing3, pos_covered4):
                            return False
            return True

        return False

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
