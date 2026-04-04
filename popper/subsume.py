from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions
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

        state = self.state
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

            if self.settings.joiner:
                subsumed = sub_prog_pos_covered in state.success_sets and state.success_sets[sub_prog_pos_covered] <= new_prog_size
                subsumed = subsumed or any(size <= new_prog_size and subset(sub_prog_pos_covered, xs) for xs, size in state.success_sets.items())
            else:
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
        for i in range(2, prog_size + 1):
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
        num_pos = self.tester.num_pos

        len_pos_covered = pos_covered.count(1)

        if len_pos_covered == num_pos:
            return False

        max_literals = self.state.max_literals

        # if new prog can be used with a new unseen prog, return False
        space_remaining = max_literals - prog_size
        if space_remaining >= self.state.search_depth:
            return False

        min_size = self.state.min_size
        max_additional_rules = (max_literals-prog_size) // min_size

        # MAX TOTAL RULES = 1
        # if this prog + the minimum size prog is too big then this prog must be used in a single rule hypothesis
        if max_additional_rules == 0:
            # if so, this prog must cover all the positive examples
            # print('new1')
            return True

        # check which examples this prog does not cover
        uncovered = self.tester.pos_examples_ & ~pos_covered

        success_sets = sorted(self.state.success_sets.items(), key=lambda x: x[1])

        # MAX TOTAL RULES = 2
        # this prog must be used with an existing prog
        if max_additional_rules == 1:
            for pos_covered2, size2 in success_sets:
                if size2 > space_remaining:
                    break
                if subset(uncovered, pos_covered2):
                    return False
            # if no prog exists, return true
            # print('new2')
            return True


        # MAX TOTAL RULES = 3
        # this prog must be used with 1 or 2 existing progs
        if max_additional_rules == 2:
            # success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.state.success_sets.items()), key=lambda x: x[1])
            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break

                # if not (uncovered & pos_covered2):
                #     print('asda price3')
                #     continue

                if subset(uncovered, pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                uncovered2 = uncovered & ~pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    if subset(uncovered2, pos_covered3):
                        return False
            # print('new3')
            return True

        # MAX TOTAL RULES = 4
        # this prog must be used with 1 or 2 or 3 existing progs
        elif max_additional_rules == 3:
            # success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.state.success_sets.items()), key=lambda x: x[1])
            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break

                # if not (uncovered & pos_covered2):
                #     print('asda price4')
                #     continue

                if subset(uncovered, pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                uncovered2 = uncovered & ~pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    if subset(uncovered2, pos_covered3):
                        return False
                    space_remaining__ = space_remaining_-size3
                    if space_remaining__ < min_size:
                        continue
                    uncovered3 = uncovered2 & ~pos_covered3
                    for k in range(j+1, n):
                        pos_covered4, size4 = success_sets[k]
                        if size4 > space_remaining__:
                            break
                        if subset(uncovered3, pos_covered4):
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
