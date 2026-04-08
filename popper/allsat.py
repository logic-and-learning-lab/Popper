# Code and idea from the paper: # Andrew Cropper, David M. Cerna: # Efficient rule induction by ignoring pointless rules. AAAI 2026.
# allsay.py takes a rule and tries to identify literals implied by the other literals in the rule, e.g. in r = f(A):- int(A), even(A)  then even(A) implies int(A), so int(A) is redundant

from . util import connected, has_valid_directions, canonicalise

class AllSatCoreFinder:
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_allsat = set()

    def check_redundant_literal(self, prog):
        if len(prog) > 1:
            return [], False

        (rule,) = prog
        head, body = rule

        if len(body) == 1:
            return [], False

        out = []

        # loop through each body literal
        for redundant_literal in body:
            allsat_cache, not_all_sat_cache = set(), set()
            new_body = body - {redundant_literal}
            out.extend(self.check_redundant_literal_aux(new_body, redundant_literal, allsat_cache, not_all_sat_cache, pruned_smaller=False))

        pruned_smaller = False
        new_out = []
        seen_bodies = set()
        for rule_body, smaller in out:
            pruned_smaller = pruned_smaller or smaller
            if rule_body not in seen_bodies:
                seen_bodies.add(rule_body)
                new_out.append(rule_body)
        return new_out, pruned_smaller

    def check_redundant_literal_aux(self, body, literal, allsat_cache, not_all_sat_cache, pruned_smaller):
        out = []

        if len(body) == 0:
            return out

        prog_key = hash((body, literal))
        if prog_key in self.seen_allsat:
            return out

        _, b = canonicalise((None, body | frozenset([literal])))
        b_key = hash(b)
        if b_key in self.seen_allsat:
            return out

        self.seen_allsat.add(prog_key)
        self.seen_allsat.add(b_key)

        body_vars = {x for atom in body for x in atom.arguments}

        if not all(x in body_vars for x in literal.arguments):
            return out

        if not connected(body | {literal}):
            for atom in body:
                new_body = body - {atom}
                out.extend(self.check_redundant_literal_aux(new_body, literal, allsat_cache, not_all_sat_cache, True))
            return out

        if any(body.issubset(seen_body) for seen_body in not_all_sat_cache):
            return out

        if any(seen_body.issubset(body) for seen_body in allsat_cache):
            return out

        if not has_valid_directions((None, body)):
            return out

        if not self.tester.is_literal_redundant(body, literal):
            not_all_sat_cache.add(body)
            return out

        allsat_cache.add(body)

        for atom in body:
            new_body = body - {atom}
            out.extend(self.check_redundant_literal_aux(new_body, literal, allsat_cache, not_all_sat_cache, True))

        if len(out) > 0:
            return out

        to_prune = frozenset(body | {literal})
        return [(to_prune, pruned_smaller)]

    def check_neg_reducible(self, prog):
        for rule in prog:
            head, body = rule

            head_vars = set(head.arguments)

            for literal in body:
                literal_p, literal_args = literal
                literal_args = set(literal_args)

                if len(body) == 1:
                    # AC: SPECIAL CASE FOR A SINGLE BODY LITERAL IMPLIED BY THE HEAD
                    if self.settings.non_datalog_flag and literal_args.issubset(head_vars) and self.tester.diff_subs_single(literal):
                        bad_rule = (head, frozenset([literal]))
                        bad_prog = frozenset([bad_rule])
                        return bad_prog
                    continue

                body_ = frozenset(body) - {literal}
                body_vars = {x for p, args in body_ for x in args}

                if not literal_args.issubset(body_vars | head_vars):
                    continue

                if not self.settings.non_datalog_flag and any(x not in body_vars for x in head_vars):
                    continue

                new_rule = (head, body_)
                new_prog = frozenset([new_rule])

                if not has_valid_directions(new_rule):
                    continue

                if self.tester.is_neg_reducible(body_, literal):
                    return frozenset([rule])
        return None
