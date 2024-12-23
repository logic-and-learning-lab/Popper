import os
import time
import pkg_resources
from janus_swi import query_once, consult
from functools import cache
from contextlib import contextmanager
from . util import order_prog, prog_is_recursive, rule_is_recursive, calc_rule_size, calc_prog_size, prog_hash, format_rule, format_literal, Literal
from bitarray import bitarray, frozenbitarray
from bitarray.util import ones
from collections import defaultdict
from itertools import product

def format_literal_janus(literal):
    args = ','.join(f'_V{i}' for i in literal.arguments)
    return f'{literal.predicate}({args})'

def bool_query(query):
    return query_once(query)['truth']

class Tester():

    def __init__(self, settings):
        self.settings = settings

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        if not settings.pi_enabled:
            consult('prog', f':- dynamic {settings.head_literal.predicate}/{len(settings.head_literal.arguments)}.')

        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            consult(x)

        query_once('load_examples')

        neg_literal = Literal('neg_fact', tuple(range(len(self.settings.head_literal.arguments))))
        self.neg_fact_str = format_literal_janus(neg_literal)
        self.neg_literal_set = frozenset([neg_literal])

        q = 'findall(_Atom2, (neg_index(_K, _Atom1), term_string(_Atom1, _Atom2)), S)'
        res = query_once(q)['S']
        atoms = []
        for x in res:
            x = x[:-1].split('(')[1].split(',')
            atoms.append(x)

        if atoms:
            try:
                settings.recall = settings.recall | deduce_neg_example_recalls(settings, atoms)
            except Exception as e:
                print(e)

        self.num_pos = query_once('findall(_K, pos_index(_K, _Atom), _S), length(_S, N)')['N']
        self.num_neg = query_once('findall(_K, neg_index(_K, _Atom), _S), length(_S, N)')['N']

        self.pos_examples_ = ones(self.num_pos)

        self.cached_pos_covered = {}
        self.cached_inconsistent = {}

        if self.settings.recursion_enabled:
            query_once(f'assert(timeout({self.settings.eval_timeout})), fail')

    def janus_clear_cache(self):
        return query_once('retractall(janus:py_call_cache(_String,_Input,_TV,_M,_Goal,_Dict,_Truth,_OutVars))')

    def parse_single_rule(self, prog):
        rule = next(iter(prog))
        head, ordered_body = self.settings.order_rule(rule)
        atom_str = format_literal_janus(head)
        body_str = ','.join(format_literal_janus(literal) for literal in ordered_body)
        return atom_str, body_str

    @cache
    def parse_body(self, body):
        _, ordered_body = self.settings.order_rule((None, body))
        body_str = ','.join(format_literal_janus(literal) for literal in ordered_body)
        return body_str

    def test_prog_noisy(self, prog, prog_size):
        settings = self.settings
        neg_covered = None
        skipped, skip_early_neg = False, False
        inconsistent = False

        if settings.recursion_enabled or settings.pi_enabled:
            pos_covered, neg_covered = self.test_prog_all(prog)
            inconsistent = neg_covered.any()
        else:
            # AC: we could push all this reasoning to Prolog to only need a single call
            pos_covered = self.test_prog_pos(prog)
            tp = pos_covered.count(1)
            # assert(tp == tp_)
            if tp > prog_size:
                # maximum size of specialisations allowed
                test_at_most_k_neg1 = min([settings.max_body-(prog_size-1), settings.max_literals-prog_size])
                # conditions which determine whether a program can be part of a solution
                test_at_most_k_neg2 = min([settings.best_mdl - prog_size, tp-prog_size])
                test_at_most_k_neg = max([test_at_most_k_neg1, test_at_most_k_neg2])
                neg_covered = self.test_single_rule_neg_at_most_k(prog, test_at_most_k_neg)
                if neg_covered.count(1) == test_at_most_k_neg:
                    skip_early_neg = True
                inconsistent = neg_covered.any()
            else:
                skipped = True
                # neg_covered

        return pos_covered, neg_covered, inconsistent, skipped, skip_early_neg


    def test_prog(self, prog):

        if self.settings.recursion_enabled or self.settings.pi_enabled:

            if len(prog) == 1:
                atom_str, body_str = self.parse_single_rule(prog)
                q = f'findall(_ID, (pos_index(_ID, {atom_str}), ({body_str} ->  true)), S)'
                pos_covered = query_once(q)['S']
                inconsistent = False
                if self.num_neg > 0:
                    q = f'neg_index(_ID, {atom_str}), {body_str}'
                    inconsistent = bool_query(q)
            else:
                with self.using(prog):
                    pos_covered = query_once('pos_covered(S)')['S']
                    inconsistent = False
                    if self.num_neg > 0:
                        inconsistent = bool_query("inconsistent")

            pos_covered_bits = bitarray(self.num_pos)
            pos_covered_bits[pos_covered] = 1
            pos_covered = frozenbitarray(pos_covered_bits)
        else:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
            pos_covered = query_once(q)['S']
            pos_covered_bits = bitarray(self.num_pos)
            pos_covered_bits[pos_covered] = 1
            pos_covered = frozenbitarray(pos_covered_bits)

            inconsistent = False
            if self.num_neg == 0:
                inconsistent = False
            elif pos_covered.any():
                head, body = next(iter(prog))
                head, ordered_body = self.settings.order_rule((None, body | self.neg_literal_set))
                body_str = ','.join(format_literal_janus(literal) for literal in ordered_body)
                inconsistent = bool_query(body_str)

        self.cached_pos_covered[hash(prog)] = pos_covered
        return pos_covered, inconsistent

    def test_prog_all(self, prog):

        if len(prog) == 1:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'findall(_ID, (pos_index(_ID, {atom_str}), ({body_str}->  true)), S)'
            pos_covered = query_once(q)['S']
            neg_covered = []
            if self.num_neg > 0:
                q = f'findall(_ID, (neg_index(_ID, {atom_str}),({body_str}->  true)), S)'
                neg_covered = query_once(q)['S']
        else:
            with self.using(prog):
                res = query_once(f'pos_covered(S1), neg_covered(S2)')
            pos_covered = res['S1']
            neg_covered = res['S2']

        pos_covered_bits = bitarray(self.num_pos)
        pos_covered_bits[pos_covered] = 1
        pos_covered = frozenbitarray(pos_covered_bits)

        neg_covered_bits = bitarray(self.num_neg)
        neg_covered_bits[neg_covered] = 1
        neg_covered = frozenbitarray(neg_covered_bits)

        return pos_covered, neg_covered

    def test_prog_pos(self, prog):

        if len(prog) == 1:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
            pos_covered = query_once(q)['S']
        else:
            with self.using(prog):
                pos_covered = query_once('pos_covered(S)')['S']

        pos_covered_bits = bitarray(self.num_pos)
        pos_covered_bits[pos_covered] = 1
        pos_covered = frozenbitarray(pos_covered_bits)
        return pos_covered

    def test_prog_inconsistent(self, prog):
        if self.num_neg == 0:
            return False

        if len(prog) == 1:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'neg_index(_ID, {atom_str}), {body_str}'
            return bool_query(q)

        with self.using(prog):
            return bool_query("inconsistent")

    def test_single_rule_neg_at_most_k(self, prog, k):

        neg_covered = []
        if self.num_neg > 0:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'findfirstn(K, _ID, (neg_index(_ID, {atom_str}),({body_str}->  true)), S)'
            neg_covered = query_once(q, {'K':k})['S']

        neg_covered_bits = bitarray(self.num_neg)
        neg_covered_bits[neg_covered] = 1
        neg_covered = frozenbitarray(neg_covered_bits)
        return neg_covered

    # why twice???
    def get_pos_covered(self, prog):

        k1 = hash(prog)
        if k1 in self.cached_pos_covered:
            return self.cached_pos_covered[k1]

        k = prog_hash(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k]


        if len(prog) == 1:
            atom_str, body_str = self.parse_single_rule(prog)
            q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
            pos_covered = query_once(q)['S']
        else:
            with self.using(prog):
                pos_covered = query_once('pos_covered(S)')['S']

        pos_covered_bits = bitarray(self.num_pos)
        pos_covered_bits[pos_covered] = 1
        pos_covered = frozenbitarray(pos_covered_bits)

        self.cached_pos_covered[k] = pos_covered
        self.cached_pos_covered[k1] = pos_covered

        return pos_covered

    @cache
    def parse_rule_for_recursion(self, rule):
        return format_rule(self.settings.order_rule(rule))[:-1]

    @contextmanager
    def using(self, prog):

        str_prog = [':- style_check(-singleton)']

        if self.settings.recursion_enabled:
            prog = order_prog(prog)

        current_clauses = set()
        for rule in prog:
            head, _body = rule
            x = self.parse_rule_for_recursion(rule)
            str_prog.append(x)
            current_clauses.add((head.predicate, len(head.arguments)))

        if self.settings.pi_enabled:
            for p, a in current_clauses:
                str_prog.append(f':- dynamic {p}/{a}')

        str_prog = '.\n'.join(str_prog) +'.'
        consult('prog', str_prog)
        yield
        for predicate, arity in current_clauses:
            args = ','.join(['_'] * arity)
            x = query_once(f"retractall({predicate}({args}))")

    def is_non_functional(self, prog):
        with self.using(prog):
            return bool_query('non_functional')

    def reduce_inconsistent(self, program):
        if len(program) < 3:
            return program
        for i in range(len(program)):
            subprog = program[:i] + program[i+1:]
            if not prog_is_recursive(subprog):
                continue
            with self.using(subprog):
                if self.test_prog_inconsistent(subprog):
                    return self.reduce_inconsistent(subprog)
        return program

    def is_sat(self, prog):

        k1 = hash(prog)
        if k1 in self.cached_pos_covered:
            return self.cached_pos_covered[k1].any()

        k = prog_hash(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k].any()

        if len(prog) == 1:
            rule = next(iter(prog))
            head, _body = rule
            new_head = f'pos_index(_ID, {format_literal_janus(head)})'
            _, ordered_body = self.parse_single_rule(prog)
            if self.settings.noisy:
                q = f'succeeds_k_times({new_head},({ordered_body}),K)'
                return query_once(q, {'K':calc_rule_size(rule)})['truth']
            else:
                if self.settings.min_coverage == 1:
                    q = f'{new_head},{ordered_body}'
                    return bool_query(q)
                else:
                    q = f'succeeds_k_times({new_head},({ordered_body}),K)'
                    return query_once(q, {'K':self.settings.min_coverage})['truth']
        else:
            with self.using(prog):
                if self.settings.noisy:
                    return query_once(f'covers_at_least_k_pos(K)',{'K':calc_prog_size(prog)})['truth']
                else:
                    return bool_query('sat')

    def is_body_sat(self, body):
        if len(body) > 1:
            q = self.parse_body(body)
        else:
            q = format_literal_janus(next(iter(body)))

        return bool_query(q)

    def is_literal_redundant(self, body, literal):
        literal_str = format_literal_janus(literal)
        if len(body) > 1:
            x = self.parse_body(body)
        else:
            x = format_literal_janus(next(iter(body)))
        q = f'{x}, \+ {literal_str}'
        return not bool_query(q)

    def diff_subs_single(self, literal):
        literal_str = format_literal_janus(literal)
        q = f'{self.neg_fact_str}, \+ {literal_str}'
        return not bool_query(q)

    def is_neg_reducible(self, body, literal):
        # AC: we do not cache as we can never see body + neg_literal again
        head, ordered_body = self.settings.order_rule((None, body | self.neg_literal_set))
        body_str = ','.join(format_literal_janus(literal) for literal in ordered_body)
        literal_str = format_literal_janus(literal)
        q = f'{body_str}, \+ {literal_str}'
        return not bool_query(q)

    @cache
    def has_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal_janus(head),) + tuple(format_literal_janus(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal_janus(lit) for lit in body))}]"
            q = f'redundant_literal({c})'
            if query_once(q)['truth']:
                # print(q, True)
                return True
            # print(q, False)
        return False

    # # WE ASSUME THAT THERE IS A REUNDANT RULE
    def find_redundant_rule_(self, prog):
        prog_ = []
        for i, (head, body) in enumerate(prog):
            c = f"{i}-[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        prog_ = janus_format_rule(prog_)
        q = f'find_redundant_rule({prog_}, K1, K2)'
        res = query_once(q)
        k1 = res['K1']
        k2 = res['K2']
        return prog[k1], prog[k2]

    def find_redundant_rules(self, prog):
        # assert(False)
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
        base = []
        step = []
        for rule in prog:
            if rule_is_recursive(rule):
                step.append(rule)
            else:
                base.append(rule)
        if len(base) > 1 and self.has_redundant_rule(base):
            return self.find_redundant_rule_(base)
        if len(step) > 1 and self.has_redundant_rule(step):
            return self.find_redundant_rule_(step)
        return None

    def find_pointless_relations(self):
        settings = self.settings
        keep = set()
        pointless = set()

        missing = set()
        arities = {}

        for p, pa in settings.body_preds:
            arities[p] = pa

            query = f'current_predicate({p}/{pa})'
            try:
                if not query_once(query)['truth']:
                    pointless.add((p, pa))
                    # print(p, pa)
                    missing.add(p)
            except Err:
                print(Err)
                return pointless

        for p, pa in settings.body_preds:
            arities[p] = pa

            if p in missing:
                continue

            for q, qa in settings.body_preds:
                if p == q:
                    continue
                if pa != qa:
                    continue
                if settings.body_types and settings.body_types[p] != settings.body_types[q]:
                    continue

                if q in missing:
                    continue

                arg_str = ','.join(f'_V{i}' for i in range(pa))
                query1 = f'{p}({arg_str}), \+ {q}({arg_str})'
                query2 = f'{q}({arg_str}), \+ {p}({arg_str})'
                # print(query1)
                # print(query2)
                try:
                    if query_once(query1)['truth'] or query_once(query2)['truth']:
                        continue
                except Exception as Err:
                    print('ERROR detecting pointless relations', Err)
                    return pointless

                a, b = (p,pa), (q,qa)

                # WTF IS GOING ON HERE?
                if a in keep and b in keep:
                    assert(False)
                if a not in pointless and b not in pointless:
                    if a in keep:
                        pointless.add(b)
                    elif b in keep:
                        pointless.add(a)
                    else:
                        keep.add(a)
                        pointless.add(b)
                elif a in pointless or b in pointless:
                    if a not in keep:
                        pointless.add(a)
                    if b not in keep:
                        pointless.add(b)
                elif a not in pointless and b not in pointless:
                    keep.add(a)
                    pointless.add(b)
                elif a in pointless:
                    pointless.add(b)
                elif b in pointless:
                    pointless.add(b)
        # return frozenset((p, arities[p]) for p in pointless)
        return pointless

def deduce_neg_example_recalls(settings, atoms):
    # Jan Struyf, Hendrik Blockeel: Query Optimization in Inductive Logic Programming by Reordering Literals. ILP 2003: 329-346

    arity = len(settings.head_literal.arguments)
    binary_strings = generate_binary_strings(arity)
    counts = {var_subset: defaultdict(set) for var_subset in binary_strings}

    for var_subset in binary_strings:
        # print(var_subset)
        d1 = counts[var_subset]
        for args in atoms:
            # print(args)
            key = []
            value = []
            for i in range(arity):
                if var_subset[i]:
                    key.append(args[i])
                else:
                    value.append(args[i])
            key = tuple(key)
            value = tuple(value)
            d2 = d1[key]
            d2.add(value)

    all_recalls = {}
    pred = 'neg_fact'
    all_recalls[(pred, (0,)*arity)] = len(atoms)
    # print(counts.items())
    for args, d2 in counts.items():
        recall = max(len(xs) for xs in d2.values())
        all_recalls[(pred, args)] = recall
    return all_recalls

def generate_binary_strings(bit_count):
    return list(product((0,1), repeat=bit_count))[1:-1]