# Code and idea from the paper: # Andrew Cropper, David M. Cerna: # Efficient rule induction by ignoring pointless rules. AAAI 2026.
# allsay.py takes a rule and tries to identify literals implied by the other literals in the rule, e.g. in r = f(A):- int(A), even(A)  then even(A) implies int(A), so int(A) is redundant

from popper.util import connected, has_valid_directions, canonicalise_rule_hash

class AllSatCoreFinder:
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_prog_hash = set()

    def check_redundant_literal(self, prog):
        if len(prog) > 1:
            return [], False

        (rule,) = prog
        _, body = rule

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

        if not body:
            return out

        rule_hash = hash((canonicalise_rule_hash((None, body), self.settings.max_vars), literal))
        if rule_hash in self.seen_prog_hash:
            return out
        self.seen_prog_hash.add(rule_hash)

        body_vars = {x for atom in body for x in atom.arguments}

        if not set(literal.arguments).issubset(body_vars):
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

        if out:
            return out

        to_prune = body | {literal}
        return [(to_prune, pruned_smaller)]

    def check_neg_reducible(self, prog):
        for rule in prog:
            head, body = rule

            head_vars = set(head.arguments)

            for literal in body:
                _, literal_args = literal
                literal_args = set(literal_args)

                if len(body) == 1:
                    # AC: SPECIAL CASE FOR A SINGLE BODY LITERAL IMPLIED BY THE HEAD
                    if self.settings.non_datalog_flag and literal_args.issubset(head_vars) and self.tester.diff_subs_single(literal):
                        bad_rule = (head, frozenset([literal]))
                        bad_prog = frozenset([bad_rule])
                        return bad_prog
                    continue

                body_ = body - {literal}
                body_vars = {x for p, args in body_ for x in args}

                if not literal_args.issubset(body_vars | head_vars):
                    continue

                if not self.settings.non_datalog_flag and not set(head_vars).issubset(body_vars):
                    continue

                new_rule = (head, body_)

                if not has_valid_directions(new_rule):
                    continue

                if self.tester.is_neg_reducible(body_, literal):
                    return frozenset([rule])
        return None
