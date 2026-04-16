import os
from importlib import resources
from janus_swi import query_once, consult
from functools import cache
from contextlib import contextmanager
from . util import order_prog, prog_is_recursive, rule_is_recursive, calc_rule_size, calc_prog_size, get_raw_prog, format_rule, Literal, mdl_score, order_rule, generate_binary_strings
from bitarray import frozenbitarray
from bitarray.util import ones, zeros
from collections import defaultdict
from itertools import combinations
from typing import NamedTuple
from . recalls import recalls
from . import logger

# MAXIMUM TESTING TIME FOR A RECURSIVE HYPOTHESIS
EVAL_TIMEOUT=0.001

# should be immutable
class TestResult(NamedTuple):
    tp: int
    fn: int
    tn: int
    fp: int
    pos_covered : frozenbitarray
    neg_covered : frozenbitarray
    inconsistent: bool
    conf_matrix: tuple
    mdl: int = None
    too_few_tp: bool = False
    too_many_fp: bool = False

def bool_query(query):
    return query_once(query)['truth']

@cache
def format_literal_janus(literal):
    args = ','.join(f'_V{i}' for i in literal.arguments)
    return f'{literal.predicate}({args})'

def _parse_rule_cached(rule):
    head, ordered_body = order_rule(rule)
    atom_str = format_literal_janus(head) if head else ""
    body_str = ','.join(format_literal_janus(lit) for lit in ordered_body)
    return atom_str, body_str

@cache
def parse_body(body):
    return _parse_rule_cached((None, body))[1]

def parse_single_rule(prog):
    (rule,) = prog
    return _parse_rule_cached(rule)

@cache
def parse_rule_for_recursion(rule):
    return format_rule(order_rule(rule))[:-1]

@cache
def rule_has_redundant_literal(rule):
    head, body = rule
    lits = tuple(format_literal_janus(lit) for lit in body)
    if head:
        lits = (f"not_{format_literal_janus(head)}",) + lits
    return query_once(f"redundant_literal([{','.join(lits)}])")["truth"]

def janus_clear_cache():
    return query_once('retractall(janus:py_call_cache(_String,_Input,_TV,_M,_Goal,_Dict,_Truth,_OutVars))')

def frozen_bits_from_indices(size, indices):
    bits = zeros(size)
    bits[indices] = 1
    return frozenbitarray(bits)

class Tester():

    def __init__(self, settings, state):
        self.settings = settings
        self.state = state

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = str(resources.files(__package__).joinpath("lp/test.pl"))

        if not settings.pi_enabled:
            consult('prog', f':- dynamic {settings.head_literal.predicate}/{len(settings.head_literal.arguments)}.')

        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            logger.info(f'Consulting {x}')
            consult(x)

        logger.info(f'Loading examples')
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
                logger.info(f'Deducing neg example recalls')
                deduce_neg_example_recalls(settings, atoms)
            except Exception as e:
                print(e)

        logger.info(f'Determining number of examples')
        self.num_pos = query_once('findall(_K, pos_index(_K, _Atom), _S), length(_S, N)')['N']
        self.num_neg = query_once('findall(_K, neg_index(_K, _Atom), _S), length(_S, N)')['N']

        self.pos_examples_ = ones(self.num_pos)

        self.empty_pos_covered = frozenbitarray(self.num_pos)
        self.empty_neg_covered = frozenbitarray(self.num_neg)

        self.cached_pos_covered = {}

        if self.settings.recursion_enabled:
            query_once(f'assert(timeout({EVAL_TIMEOUT})), fail')

    # main entry point for calling prolog without noise
    # we call this method for every program
    def test_prog(self, prog, prog_size=None):
        inconsistent = False

        if len(prog) == 1:
            pos_covered = self._test_prog_pos(prog)
            if self.num_neg > 0 and pos_covered.any():
                if self.settings.has_directions:
                    atom_str, body_str = parse_single_rule(prog)
                    q = f'neg_index(_ID, {atom_str}), {body_str}'
                else:
                    (rule,) = prog
                    _, body = rule
                    q = parse_body(body.union(self.neg_literal_set))
                inconsistent = bool_query(q)
        else:
            with self.using(prog):
                pos_covered_list = query_once('pos_covered(S)')['S']
                if self.num_neg > 0:
                    inconsistent = bool_query("inconsistent")
            
            if not pos_covered_list:
                pos_covered = self.empty_pos_covered
            else:
                pos_covered = frozen_bits_from_indices(self.num_pos, pos_covered_list)

        # cache results
        self.cached_pos_covered[prog] = pos_covered

        tp = pos_covered.count(1)
        fn = self.num_pos - tp

        return TestResult(
            tp=tp,
            fn=fn,
            tn=None,
            fp=None,
            pos_covered=pos_covered,
            neg_covered=None,
            inconsistent=inconsistent,
            conf_matrix=(tp, fn, None, None)
        )

    # main entry point for calling prolog with noise
    # we call this method for every noisy program
    def test_prog_noisy(self, prog, prog_size):
        settings = self.settings
        neg_covered = None
        too_few_tp = False
        too_many_fp = False
        inconsistent = False

        if len(prog) == 1:
            # AC: we could push all this reasoning to Prolog to only need a single call
            pos_covered = self._test_prog_pos(prog)
            tp = pos_covered.count(1)

            if tp > prog_size:
                # maximum size of specialisations allowed
                max_k_neg1 = min(settings.max_body - (prog_size - 1), self.state.max_literals - prog_size)
                # conditions which determine whether a program can be part of a solution
                max_k_neg2 = min(self.state.best_hypothesis_mdl - prog_size, tp - prog_size)
                max_k_neg = max(max_k_neg1, max_k_neg2)
                neg_covered = []
                if self.num_neg > 0:
                    atom_str, body_str = parse_single_rule(prog)
                    q = f'findfirstn(K, _ID, (neg_index(_ID, {atom_str}),({body_str}->  true)), S)'
                    neg_covered = query_once(q, {'K':max_k_neg})['S']
                neg_covered = frozen_bits_from_indices(self.num_neg, neg_covered)
                if neg_covered.count(1) == max_k_neg:
                    too_many_fp = True

                inconsistent = neg_covered.any()
            else:
                too_few_tp = True
        else:
            pos_covered, neg_covered = self.test_prog_all(prog)
            inconsistent = neg_covered.any()
            tp = pos_covered.count(1)

        # Calculate final metrics
        fn = self.num_pos - tp
        fp = None
        tn = None
        mdl = None

        if not too_few_tp:
            fp = neg_covered.count(1)
            tn = self.num_neg - fp
            mdl = mdl_score(fn, fp, prog_size)

        return TestResult(
            tp=tp,
            fn=fn,
            tn=tn,
            fp=fp,
            pos_covered=pos_covered,
            neg_covered=neg_covered,
            inconsistent=inconsistent,
            conf_matrix=(tp, fn, tn, fp),
            mdl=mdl,
            too_few_tp=too_few_tp,
            too_many_fp=too_many_fp
        )

    # used when learning programs with recursion
    # just checks whether they entail a negative example
    def test_prog_inconsistent(self, prog):
        if self.num_neg == 0:
            return False

        if len(prog) == 1:
            atom_str, body_str = parse_single_rule(prog)
            q = f'neg_index(_ID, {atom_str}), {body_str}'
            return bool_query(q)

        with self.using(prog):
            return bool_query("inconsistent")

    # used by the unsat core checker to see if a body is satisfiable
    def is_body_sat(self, body):
        if len(body) > 1:
            q = parse_body(body)
        else:
            (lit,) = body
            q = format_literal_janus(lit)

        return bool_query(q)

    # used by the unsat core checker to see if a rule is satisfiable
    def is_sat(self, prog):

        if prog in self.cached_pos_covered:
            return self.cached_pos_covered[prog].any()

        k = get_raw_prog(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k].any()

        if len(prog) == 1:
            (rule,) = prog
            head, _body = rule
            new_head = f'pos_index(_ID, {format_literal_janus(head)})'
            _, ordered_body = _parse_rule_cached(rule)
            if self.settings.noisy:
                q = f'succeeds_k_times({new_head},({ordered_body}),K)'
                return query_once(q, {'K':calc_rule_size(rule)})['truth']
            else:
                if self.state.min_pos_coverage == 1:
                    q = f'{new_head},{ordered_body}'
                    return bool_query(q)
                else:
                    q = f'succeeds_k_times({new_head},({ordered_body}),K)'
                    return query_once(q, {'K':self.state.min_pos_coverage})['truth']
        else:
            with self.using(prog):
                if self.settings.noisy:
                    return query_once(f'covers_at_least_k_pos(K)',{'K':calc_prog_size(prog)})['truth']
                else:
                    return bool_query('sat')

    # called by the allsat code
    # tries to determine whether literal is implied by body for the negative examples
    # AC: we do not cache as we can never see body + neg_literal again
    def is_neg_reducible(self, body, literal):
        body_str = parse_body(body.union(self.neg_literal_set))
        literal_str = format_literal_janus(literal)
        q = f'{body_str}, \\+ {literal_str}'
        return not bool_query(q)

    # called by the allsat code
    # checks whether a literal is implied by the body
    def is_literal_redundant(self, body, literal):
        q = f'{parse_body(body)}, \\+ {format_literal_janus(literal)}'
        return not bool_query(q)

    # also called by the allsat code
    def diff_subs_single(self, literal):
        literal_str = format_literal_janus(literal)
        q = f'{self.neg_fact_str}, \\+ {literal_str}'
        return not bool_query(q)

    # ONLY CALLED BY THE COMBINER WHEN THERE IS MORE THAN ONE RULE
    # also called internally by test_prog_noisy
    def test_prog_all(self, prog):
        pos_covered = self._test_prog_pos(prog)
        neg_covered = self.test_prog_neg(prog)
        return pos_covered, neg_covered

    # ONLY CALLED BY THIS CLASS
    def _test_prog_pos(self, prog):

        if len(prog) == 1:
            atom_str, body_str = parse_single_rule(prog)
            q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
            pos_covered = query_once(q)['S']
        else:
            with self.using(prog):
                pos_covered = query_once('pos_covered(S)')['S']

        if not pos_covered:
            return self.empty_pos_covered

        return frozen_bits_from_indices(self.num_pos, pos_covered)

    # ONLY CALLED BY JOINER AND THIS CLASS
    def test_prog_neg(self, prog):

        if len(prog) == 1:
            atom_str, body_str = parse_single_rule(prog)
            neg_covered = []
            if self.num_neg > 0:
                q = f'findall(_ID, (neg_index(_ID, {atom_str}),({body_str}->  true)), S)'
                neg_covered = query_once(q)['S']
        else:
            with self.using(prog):
                res = query_once(f'neg_covered(S2)')
            neg_covered = res['S2']
        
        if not neg_covered:
            return self.empty_neg_covered

        return frozen_bits_from_indices(self.num_neg, neg_covered)

    def has_redundant_literal(self, prog):
        return any(rule_has_redundant_literal(rule) for rule in prog)

    # THIS IS CALLED BY THE SUBSUMER CHECKER
    # FOR EACH RULE, WE CHECK WHAT THE SUBRULES ENTAIL:
    def get_pos_covered(self, prog):

        if prog in self.cached_pos_covered:
            return self.cached_pos_covered[prog]

        k = get_raw_prog(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k]

        pos_covered = self._test_prog_pos(prog)
        self.cached_pos_covered[k] = pos_covered
        self.cached_pos_covered[prog] = pos_covered
        return pos_covered

    @contextmanager
    def using(self, prog):

        str_prog = [':- style_check(-singleton)']

        if self.settings.recursion_enabled:
            prog = order_prog(prog)

        current_clauses = set()
        for rule in prog:
            head, _body = rule
            x = parse_rule_for_recursion(rule)
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
            query_once(f"retractall({predicate}({args}))")

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


    def find_redundant_rules(self, prog):
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

        for p, pa in settings.body_preds:
            try:
                if not query_once(f'current_predicate({p}/{pa})')['truth']:
                    pointless.add((p, pa))
                    missing.add(p)
            except Exception as Err:
                print(f"Error in find_pointless_relations: {Err}")
                return pointless

        preds = [(p, pa) for p, pa in settings.body_preds if p not in missing]

        for (p, pa), (q, qa) in combinations(preds, 2):
            if pa != qa:
                continue
            if settings.body_types and settings.body_types[p] != settings.body_types[q]:
                continue

            a, b = (p, pa), (q, qa)
            if a in pointless and b in pointless:
                continue

            arg_str = ','.join(f'_V{i}' for i in range(pa))
            query = f'({p}({arg_str}), \\+ {q}({arg_str})) ; ({q}({arg_str}), \\+ {p}({arg_str}))'
            try:
                if query_once(query)['truth']:
                    continue
            except Exception as Err:
                print('ERROR detecting pointless relations', Err)
                return pointless

            if a in keep and b in keep:
                raise ValueError(f'Both {a} and {b} are in keep — invariant violated')
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

        return pointless

def deduce_neg_example_recalls(settings, atoms):
    # Jan Struyf, Hendrik Blockeel: Query Optimization in Inductive Logic Programming by Reordering Literals. ILP 2003: 329-346

    arity = len(settings.head_literal.arguments)
    binary_strings = generate_binary_strings(arity)
    counts = {var_subset: defaultdict(set) for var_subset in binary_strings}

    for var_subset in binary_strings:
        d1 = counts[var_subset]
        for args in atoms:
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
    for args, d2 in counts.items():
        recall = max(len(xs) for xs in d2.values())
        all_recalls[(pred, args)] = recall

    recalls.update(all_recalls)
