# Code and idea from the paper: Andrew Cropper, Céline Hocquette: Learning Logic Programs by Finding Minimal Unsatisfiable Subprograms. ECAI 2024: 4295-4302

from . import logger
from . util import rule_is_recursive, prog_is_recursive, format_rule, GENERALISATION, SPECIALISATION, UNSAT, REDUNDANCY_CONSTRAINT1, REDUNDANCY_CONSTRAINT2, get_raw_prog, canonicalise, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions, Literal, canonicalise_prog_hash

class UnsatCoreFinder:
    def __init__(self, settings, tester):
        self.settings = settings
        self.seen_prog_hash = set()
        self.unsat_prog = set()
        self.unsat_raw_prog = set()
        self.tester = tester

    def explain_incomplete(self, prog):
        """Find and yield constraints based on minimal unsatisfiable subprograms."""
        unsat_cores = self.explain_totally_incomplete(prog)

        for subprog, is_headless_unsat in unsat_cores:
            if self.settings.verbosity > 2:
                if len(subprog) > 1:
                    print('\n')
                for rule in subprog:
                    logger.debug(f'\t UNSAT: \t {format_rule(rule)}')

            if is_headless_unsat:
                # If a headless version (just the body) is unsat, it's a strong UNSAT constraint
                _, body = next(iter(subprog))
                yield (UNSAT, body)
                continue

            # Standard specialisation/redundancy constraints
            if not (self.settings.recursion_enabled or self.settings.pi_enabled):
                yield (SPECIALISATION, [canonicalise(rule) for rule in subprog])
                continue

            if len(subprog) == 1:
                yield (REDUNDANCY_CONSTRAINT1, [canonicalise(rule) for rule in subprog])

            yield (REDUNDANCY_CONSTRAINT2, [canonicalise(rule) for rule in subprog])

    def explain_totally_incomplete(self, prog):
        """Entry point for the recursive unsat core search."""
        return list(self.explain_totally_incomplete_aux(prog, set(), set()))

    def explain_totally_incomplete_aux(self, prog, seen_unsat_theory, seen_raw_unsat):
        """Recursive search for minimal unsatisfiable generalisations."""
        has_recursion = prog_is_recursive(prog)
        out = []

        for subprog in generalisations(prog, allow_headless=True, recursive=has_recursion):

            # check we have not seen this subprog or a variant before
            prog_hash = canonicalise_prog_hash(subprog, self.settings.max_vars)
            if prog_hash in self.seen_prog_hash:
                continue
            self.seen_prog_hash.add(prog_hash)

            subprog = frozenset(subprog)
            if self._should_skip(subprog):
                continue

            if seen_more_general_unsat(subprog, seen_unsat_theory):
                continue

            raw_prog = get_raw_prog(subprog)

            if seen_more_general_unsat(raw_prog, seen_raw_unsat):
                continue

            if not self.prog_is_ok(subprog):
                out.extend(self.explain_totally_incomplete_aux(subprog, seen_unsat_theory, seen_raw_unsat))
                continue

            if self.tester.has_redundant_literal(subprog):
                out.extend(self.explain_totally_incomplete_aux(subprog, seen_unsat_theory, seen_raw_unsat))
                continue

            # Check satisfiability of the generalisation
            headless = any(head is None for head, body in subprog)

            if headless:
                _, body = next(iter(subprog))
                if self.tester.is_body_sat(body):
                    continue
            else:
                if self.tester.is_sat(subprog):
                    continue

            # Found an unsatisfiable generalisation
            seen_raw_unsat.add(raw_prog)
            seen_unsat_theory.add(subprog)
            self.unsat_raw_prog.add(raw_prog)
            self.unsat_prog.add(subprog)

            xs = self.explain_totally_incomplete_aux(subprog, seen_unsat_theory, seen_raw_unsat)
            if xs:
                out.extend(xs)
            else:
                # If no further generalisations are unsat, this is a minimal core
                out.append((subprog, headless))

        return out

    def _should_skip(self, subprog):
        """Check if any subset of a single-rule program is already known to be unsat."""
        if len(subprog) != 1:
            return False

        h_, b_ = next(iter(subprog))
        for x in non_empty_powerset(b_):
            # Check both headless and headed variants in the global unsat set
            sub_variants = [[(None, x)], [(h_, x)]]
            for sub_ in sub_variants:
                fs_sub = frozenset(sub_)
                if fs_sub in self.unsat_prog:
                    return True
                if get_raw_prog(fs_sub) in self.unsat_raw_prog:
                    return True
        return False

    def prog_is_ok(self, prog):
        """Check if a program satisfies basic sanity constraints (connectivity, directions)."""
        has_recursion = False
        for rule in prog:
            head, body = rule
            if head and not head_connected(rule):
                return False

            if not head and not connected(body):
                return False

            if not has_valid_directions(rule, self.settings):
                return False
                
            if head is None: # Headless rules not allowed in multi-rule programs here
                if len(prog) > 1:
                    return False
            elif rule_is_recursive(rule):
                has_recursion = True
                if len(body) == 1:
                    return False

        if len(prog) == 1:
            return True

        if not has_recursion:
            return False

        if self.needs_datalog_check(prog) and not self.head_vars_in_body(prog):
            return False

        return True

    def needs_datalog_check(self, prog):
        """Check if recursive variables appear in input positions of other literals."""
        if not self.settings.has_directions:
            return False

        for rule in prog:
            rec_outputs = set()
            non_rec_inputs = set()
            head, body = rule
            head_pred, _head_args = head
            for pred, args in body:
                if pred == head_pred:
                    rec_outputs.update(self.settings.literal_outputs[(pred, args)])
                else:
                    non_rec_inputs.update(self.settings.literal_inputs[(pred, args)])
            if not rec_outputs.isdisjoint(non_rec_inputs):
                return True
        return False

    def head_vars_in_body(self, prog):
        """Check if all head variables appear in the body for every rule."""
        for head, body in prog:
            _head_pred, head_args = head
            body_args = {x for _pred, args in body for x in args}
            if not set(head_args).issubset(body_args):
                return False
        return True

def seen_more_general_unsat(prog, unsat_set):
    """Check if any theory in the unsat_set subsumes the given program."""
    return any(theory_subsumes(seen, prog) for seen in unsat_set)
