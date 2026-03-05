from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations, has_valid_directions, settings
from itertools import chain, combinations, permutations

class AllSatCoreFinder:
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_allsat = set()

    def check_redundant_literal(self, prog):
        if len(prog) > 1:
            return [], False

        rule = list(prog)[0]
        head, body = rule

        if len(body) == 1:
            return [], False

        body = tuple(body)
        out = []
        allsat1, allsat2 = set(), set()

        # loop through each body literal
        for i in range(len(body)):
            redundant_literal = body[i]
            new_body = frozenset(body) - {redundant_literal}
            out.extend(self.check_redundant_literal_aux(new_body, redundant_literal, allsat1, allsat2, depth=0))

        pruned_smaller = False
        new_out = []
        for rule_body, smaller in out:
            pruned_smaller = pruned_smaller or smaller
            new_out.append(rule_body)
        return new_out, pruned_smaller

    def check_redundant_literal_aux(self, body, literal, allsat1, not_all_sat1, depth):
        out = []
        prog = frozenset([(None, body)])

        if len(body) == 0:
            return out

        prog_key = (body, literal)
        if hash(prog_key) in self.seen_allsat:
            return out

        h_, b = remap_variables((None, body | frozenset([literal])))
        if hash(b) in self.seen_allsat:
            return out

        self.seen_allsat.add(hash(prog_key))
        self.seen_allsat.add(hash(b))

        body_vars = set()
        for atom in body:
            body_vars.update(atom.arguments)

        if not all(x in body_vars for x in literal.arguments):
            return out

        if not connected(body | frozenset([literal])):
            for new_body in combinations(body, len(body) - 1):
                new_body = frozenset(new_body)
                out.extend(self.check_redundant_literal_aux(new_body, literal, allsat1, not_all_sat1, depth + 1))
            return out

        if any(body.issubset(seen_body) for seen_body in not_all_sat1):
            return out

        if any(seen_body.issubset(body) for seen_body in allsat1):
            return out

        if not has_valid_directions((None, body)):
            return out

        if not self.tester.is_literal_redundant(body, literal):
            not_all_sat1.add(body)
            return out

        allsat1.add(body)

        for new_body in combinations(body, len(body) - 1):
            new_body = frozenset(new_body)
            out.extend(self.check_redundant_literal_aux(new_body, literal, allsat1, not_all_sat1, depth + 1))

        if len(out) > 0:
            return out

        to_prune = frozenset(body | {literal})
        return [(to_prune, depth > 0)]

    def check_neg_reducible(self, prog):
        for rule in prog:
            head, body = rule

            head_vars = set()
            for arg in head.arguments:
                head_vars.add(arg)

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
                body_vars = set()

                for p, args in body_:
                    for arg in args:
                        body_vars.add(arg)

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