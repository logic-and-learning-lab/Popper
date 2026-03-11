# Code and idea from the paper:
# Andrew Cropper, Céline Hocquette:
# Learning Logic Programs by Finding Minimal Unsatisfiable Subprograms. ECAI 2024: 4295-4302


from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions

class UnsatCoreFinder:
    def __init__(self, settings, tester):
        self.settings = settings
        self.seen_prog = set()
        self.unsat_set = set()
        self.tester = tester

# find unsat cores
    def explain_incomplete(self,prog):
        unsat_cores = self.explain_totally_incomplete(prog)

        for subprog, unsat_body in unsat_cores:
            if self.settings.showcons:
                if len(subprog) > 1:
                    print('\n')
                for rule in enumerate(subprog):
                    pass
                for rule in subprog:
                    print('\t', 'UNSAT:', '\t', format_rule(rule))

            if unsat_body:
                _, body = list(subprog)[0]
                yield (Constraint.UNSAT, body)
                continue

            if not (self.settings.recursion_enabled or self.settings.pi_enabled):
                yield (Constraint.SPECIALISATION, [remap_variables(rule) for rule in subprog])
                continue

            if len(subprog) == 1:
                yield (Constraint.REDUNDANCY_CONSTRAINT1, [remap_variables(rule) for rule in subprog])

            yield (Constraint.REDUNDANCY_CONSTRAINT2, [remap_variables(rule) for rule in subprog])

    def explain_totally_incomplete(self, prog):
        return list(self.explain_totally_incomplete_aux2(prog, None, None))

    def explain_totally_incomplete_aux2(self, prog, unsat2=None, unsat=None):
        if unsat2 is None:
            unsat2 = set()
        if unsat is None:
            unsat = set()

        has_recursion = prog_is_recursive(prog)
        out = []

        for subprog in generalisations(prog, allow_headless=True, recursive=has_recursion):
            subprog = frozenset(subprog)
            if hash(subprog) in self.seen_prog:
                continue

            raw_prog = get_raw_prog(subprog)
            if hash(raw_prog) in self.seen_prog:
                continue

            self.seen_prog.add(hash(subprog))
            self.seen_prog.add(hash(raw_prog))

            def should_skip():
                if len(subprog) > 0:
                    return False
                h_, b_ = list(subprog)[0]
                for x in non_empty_powerset(b_):
                    sub_ = [(None, x)]
                    if frozenset(sub_) in self.unsat_set:
                        return True
                    if get_raw_prog(sub_) in self.unsat_set:
                        return True
                    sub_ = [(h_, x)]
                    if frozenset(sub_) in self.unsat_set:
                        return True
                    if get_raw_prog(sub_) in self.unsat_set:
                        return True
                return False

            if should_skip():
                continue

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if seen_more_general_unsat(subprog, unsat2):
                continue

            if not self.prog_is_ok(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
                out.extend(xs)
                continue

            if self.tester.has_redundant_literal(frozenset(subprog)):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
                out.extend(xs)
                continue

            test_prog = build_test_prog(subprog)

            headless = any(head is None for head, body in subprog)

            if headless:
                body = list(test_prog)[0][1]
                if self.tester.is_body_sat(body):
                    continue
            else:
                if self.tester.is_sat(test_prog):
                    continue

            unsat.add(raw_prog)
            unsat2.add(subprog)
            self.unsat_set.add(raw_prog)
            self.unsat_set.add(subprog)

            xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))

        return out

    def prog_is_ok(self,prog):
        for rule in prog:
            head, body = rule
            if head and not head_connected(rule):
                return False

            if not head and not connected(body):
                return False

            if not has_valid_directions(rule):
                return False

        if len(prog) == 1:
            return True

        has_recursion = False
        for rule in prog:
            h, b = rule

            if h is None:
                return False

            if rule_is_recursive(rule):
                has_recursion = True
                h, b = rule
                if len(b) == 1:
                    return False

        if not has_recursion:
            return False

        if self.needs_datalog(prog) and not tmp(prog):
            return False

        return True

    def needs_datalog(self, prog):
        if not self.settings.has_directions:
            return False
        for rule in prog:
            rec_outputs = set()
            non_rec_inputs = set()
            head, body = rule
            head_pred, _head_args = head
            for literal in body:
                pred, args = literal
                if pred == head_pred:
                    literal_outputs = self.settings.literal_outputs[(pred, args)]
                    rec_outputs.update(literal_outputs)
                else:
                    literal_inputs = self.settings.literal_inputs[(pred, args)]
                    non_rec_inputs.update(literal_inputs)
            if any(x in rec_outputs for x in non_rec_inputs):
                return True
        return False

def seen_more_general_unsat(prog, unsat):
    return any(theory_subsumes(seen, prog) for seen in unsat)

def build_test_prog(subprog):
    test_prog = []
    for head, body in subprog:
        if head:
            head_pred, head_args = head
            head_literal = Literal(head_pred, head_args)
        else:
            head_literal = False
        body_literals = set()
        for pred, args in body:
            body_literals.add(Literal(pred, args))
        rule = head_literal, frozenset(body_literals)
        test_prog.append(rule)
    return frozenset(test_prog)


def tmp(prog):
    for rule in prog:
        head, body = rule
        _head_pred, head_args = head
        body_args = set(x for _pred, args in body for x in args)
        if any(x not in body_args for x in head_args):
            return False
    return True