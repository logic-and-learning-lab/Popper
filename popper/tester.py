import os
import time
import pkg_resources
from janus_swi import query_once, consult
from functools import cache
from contextlib import contextmanager
from . util import order_prog, prog_is_recursive, rule_is_recursive, calc_rule_size, calc_prog_size, prog_hash, format_rule, format_literal
from bitarray import bitarray, frozenbitarray

def format_literal_janus(literal):
    args = ','.join(f'_V{i}' for i in literal.arguments)
    return f'{literal.predicate}({args})'

def format_rule_janus(rule):
    head, body = rule
    head_str = ''
    if head:
        head_str = format_literal_janus(head)
    body_str = ','.join(format_literal_janus(literal) for literal in body)
    return f'{head_str}:- {body_str}.'

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

        self.num_pos = query_once('findall(_K, pos_index(_K, _Atom), _S), length(_S, N)')['N']
        self.num_neg = query_once('findall(_K, neg_index(_K, _Atom), _S), length(_S, N)')['N']

        self.pos_examples_ = bitarray(self.num_pos)
        self.pos_examples_.setall(1)

        self.cached_pos_covered = {}
        self.cached_inconsistent = {}

        if self.settings.recursion_enabled:
            query_once(f'assert(timeout({self.settings.eval_timeout})), fail')

    # AC: THIS METHOD IS VERY EXPENSIVE, ESPECIALLY THE ORDER_RULE CALL
    @cache
    def parse_single_rule(self, prog):
        rule = list(prog)[0]
        head, ordered_body = self.settings.order_rule(rule)
        atom_str = format_literal_janus(head)
        body_str = format_rule_janus((None, ordered_body))[2:-1]
        return atom_str, body_str

    def test_prog(self, prog):

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
            # print('tester', q)
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
    def get_pos_covered(self, prog, ignore=True):
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
        return pos_covered

    @contextmanager
    def using(self, prog):

        str_prog = [':- style_check(-singleton)']

        if self.settings.recursion_enabled:
            prog = order_prog(prog)

        current_clauses = set()
        for rule in prog:
            head, _body = rule
            x = format_rule(self.settings.order_rule(rule))[:-1]
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
        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = self.settings.order_rule(rule)
            if self.settings.noisy:
                new_head = f'pos_index(_ID, {format_literal_janus(head)})'
                x = format_rule_janus((None, ordered_body))[2:-1]
                q = f'succeeds_k_times({new_head},({x}),K)'
                return query_once(q, {'K':calc_rule_size(rule)})['truth']
            else:
                head = f'pos_index(_,{format_literal_janus(head)})'
                x = format_rule_janus((None, ordered_body))[2:-1]
                x = f'{head},{x}'
                return bool_query(x)
        else:
            with self.using(prog):
                if self.settings.noisy:
                    return query_once(f'covers_at_least_k_pos(K)',{'K':calc_prog_size(prog)})['truth']
                else:
                    return bool_query('sat')

    def is_body_sat(self, body):
        _, ordered_body = self.settings.order_rule((None, body))
        query = ','.join(format_literal(literal) for literal in ordered_body)
        return bool_query(query)

    # def has_redundant_rule_(self, prog):
    #     assert(False)
    #     prog_ = []
    #     for head, body in prog:
    #         c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
    #         prog_.append(c)
    #     prog_ = f"[{','.join(prog_)}]"
    #     prog_ = janus_format_rule(prog_)
    #     q = f'redundant_clause({prog_})'
    #     return bool_query(q)

    # def has_redundant_rule(self, prog):
    #     assert(False)
    #     # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses

    #     base = []
    #     step = []
    #     for rule in prog:
    #         if rule_is_recursive(rule):
    #             step.append(rule)
    #         else:
    #             base.append(rule)
    #     if len(base) > 1 and self.has_redundant_rule_(base):
    #         return True
    #     if len(step) > 1 and self.has_redundant_rule_(step):
    #         return True
    #     return False

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


    # def has_redundant_rule_(self, prog):
    #     prog_ = []
    #     for head, body in prog:
    #         c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
    #         prog_.append(c)
    #     prog_ = f"[{','.join(prog_)}]"
    #     return len(list(self.prolog.query(f'redundant_clause({prog_})'))) > 0
    #     # return self.bool_query(f'redundant_clause({prog_})')

    # def has_redundant_rule(self, prog):
    #     # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses

    #     base = []
    #     step = []
    #     for rule in prog:
    #         if rule_is_recursive(rule):
    #             step.append(rule)
    #         else:
    #             base.append(rule)
    #     if len(base) > 1 and self.has_redundant_rule_(base):
    #         return True
    #     if len(step) > 1 and self.has_redundant_rule_(step):
    #         return True
    #     return False


    # def test_single_rule_neg(self, prog):
    #     # pos_covered = frozenset()
    #     neg_covered = frozenset()
    #     try:
    #         rule = list(prog)[0]
    #         head, _body = rule
    #         head, ordered_body = order_rule(rule, self.settings)
    #         atom_str = format_literal(head)
    #         body_str = format_rule((None,ordered_body))[2:-1]
    #         if len(self.neg_index) > 0:
    #             q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
    #             xs = next(self.prolog.query(q))
    #             neg_covered = frozenset(xs['Xs'])

    #     except PrologError as err:
    #         print('PROLOG ERROR',err)
    #     return neg_covered

    # def test_single_rule_neg_at_most_k2(self, prog, k):
    #     # pos_covered = frozenset()
    #     neg_covered = frozenset()
    #     try:
    #         rule = list(prog)[0]
    #         head, _body = rule
    #         head, ordered_body = order_rule(rule, self.settings)
    #         atom_str = format_literal(head)
    #         body_str = format_rule((None,ordered_body))[2:-1]
    #         if len(self.neg_index) > 0:
    #             q = f'findfirstn({k}, ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
    #             # q = f'findfirstn(ID, limit({k},(neg_index(ID,{atom_str}),({body_str}->  true))), Xs)'
    #             xs = next(self.prolog.query(q))
    #             neg_covered = frozenset(xs['Xs'])

    #     except PrologError as err:
    #         print('PROLOG ERROR',err)
    #     return neg_covered


    # def is_complete(self, prog):
    #     with self.using(prog):
    #         pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
    #         return len(pos_covered) == len(self.pos_index)

# def get_neg_covered(self, prog):
    #      with self.using(prog):
    #         return frozenset(self.query('neg_covered(S)', 'S'))

    # def get_neg_covered2(self, prog):
    #     k = prog_hash(prog)
    #     assert(k not in self.cached_neg_covers)

    #     if len(prog) == 1:
    #         rule = list(prog)[0]
    #         head, _body = rule
    #         head, ordered_body = order_rule(rule, self.settings)
    #         atom_str = janus_format_rule(format_literal(head))
    #         body_str = janus_format_rule(format_rule((None,ordered_body))[2:-1])
    #         q = f'findall(_ID, (neg_index(_ID, {atom_str}),({body_str}->  true)), S)'
    #         xs = self.bool_query(q, 'S')
    #         self.cached_neg_covers[k] = xs
    #         return frozenset(xs)
    #     else:
    #         with self.using(prog):
    #             xs = frozenset(self.query('neg_covered(S)', 'S'))
    #             self.cached_neg_covers[k] = xs
    #             return xs


    # def get_neg_uncovered(self, prog):
    #     with self.using(prog):
    #         return frozenset(self.query('neg_uncovered(Xs)', 'Xs'))

    # def is_more_inconsistent(self, prog, neg_covered):
    #     with self.using(prog):
    #         return len(list(self.prolog.query(f"is_more_inconsistent({neg_covered})"))) > 0


    # def covers_any3(self, prog, neg):
    #     # k = rule_hash(rule)
    #     rule = list(prog)[0]
    #     # k = prog_hash(prog)
    #     # if k in self.cached_covers_any:
    #     #     for x in self.cached_covers_any[k]:
    #     #         if x in neg:
    #     #             return True
    #     # if k in self.cached_covers_any2:
    #     #     for x in self.cached_covers_any2[k]:
    #     #         if x in neg:
    #     #             return True
    #     #             # print('MOOCOWJONES!!!!!!!!')
    #     # self.cached_covers_any[k] = set()

    #     # for rule in prog:
    #         # print('\tcalling prolog', format_rule(rule))

    #     rule = list(prog)[0]
    #     head, _body = rule
    #     head, ordered_body = order_rule(rule, self.settings)
    #     atom_str = format_literal(head)
    #     body_str = format_rule((None,ordered_body))[2:-1]
    #     # q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
    #     q = f'member(Id,{neg}),neg_index(Id,{atom_str}),{body_str},!'
    #     # print(q)
    #     xs = list(self.prolog.query(q))
    #     if len(xs) > 0:
    #         ex = xs[0]['Id']
    #         # self.cached_covers_any[k].add(ex)
    #         return True
    #     # print(xs)
    #     return False

        # return frozenset(xs['Xs'])

        # with self.using(prog):
        #     xs = list(self.prolog.query(f"covers_any({neg},ID)"))
        #     if len(xs) > 0:
        #         ex = xs[0]['ID']
        #         # print(ex)
        #         self.cached_covers_any[k].add(ex)
        #         return True
        #     return False

    # def get_num_neg_covered(self, prog):
    #     assert(len(prog) == 1)

    #     rule = list(prog)[0]
    #     head, _body = rule
    #     head, ordered_body = order_rule(rule, self.settings)
    #     atom_str = format_literal(head)
    #     body_str = format_rule((None,ordered_body))[2:-1]
    #     q = f'findall(ID, (pos_index(ID,{atom_str}),({body_str}->  true)), Xs)'
    #     xs = next(self.prolog.query(q))
    #     pos_covered = frozenset(xs['Xs'])
    #     inconsistent = False
    #     q = f'neg_index(_,{atom_str}),{body_str},!'
    #     if len(self.neg_index) > 0:
    #     inconsistent = len(list(self.prolog.query(q))) > 0



# def check_redundant_literal(self, prog):
    #     for rule in prog:
    #         head, body = rule
    #         if head:
    #             c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
    #         else:
    #             c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
    #         res = list(self.prolog.query(f'redundant_literal({c})'))
    #         if res:
    #             yield rule

    # @profile
    # def has_redundant_literal(self, prog):
    #     k = prog_hash(prog)
    #     out = None
    #     if k in self.cached_redundant:
    #         out = self.cached_redundant[k]
    #     else:
    #         for rule in prog:
    #             head, body = rule
    #             if head:
    #                 c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
    #             else:
    #                 c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
    #             c = janus_format_rule(c)
    #             # print(c)
    #             res = janus.query_once(f'redundant_literal({c})')['truth']
    #             # print('res', res)
    #             if res:
    #                 print(res)
    #                 self.cached_redundant[k] = True
    #                 break
    #     self.cached_redundant[k] = False
    #     out = self.cached_redundant[k]
    #     return out


# # WE ASSUME THAT THERE IS A REUNDANT RULE
    # def subsumes(self, r1, r2):
    #     r2 = str(r2)
    #     r2 = r2.replace('A','X')
    #     r2 = r2.replace('B','Y')
    #     r2 = r2.replace('C','Z')
    #     q = f'subsumes_term({r1},{r2})'.replace("'",'')
    #     # q = f'subsumes({r1},{r2})'.replace("'",'')
    #     # print(q)
    #     res = list(self.prolog.query(q))
    #     return len(res) > 0

    # def find_redundant_rule_2(self, rules):
    #     prog_ = []
    #     for i, rule in enumerate(rules):
    #         c = f"{i}-[{','.join(rule)}]"
    #         prog_.append(c)
    #         # print(c)
    #     prog_ = f"[{','.join(prog_)}]"
    #     # print(prog_)
    #     q = f'reduce_theory({prog_},K2)'
    #     print(len(rules))

    #     res = list(self.prolog.query(q))
    #     # print(res)
    #     # k1 = res[0]['K1']
    #     k2 = res[0]['K2']
    #     print(len(k2))
    #     # pr
    #     # return prog[k1], prog[k2]

    # def find_redundant_rules(self, prog):
    #     # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
    #     base = []
    #     step = []
    #     for rule in prog:
    #         if rule_is_recursive(rule):
    #             step.append(rule)
    #         else:
    #             base.append(rule)
    #     if len(base) > 1 and self.has_redundant_rule(base):
    #         return self.find_redundant_rule_(base)
    #     if len(step) > 1 and self.has_redundant_rule(step):
    #         return self.find_redundant_rule_(step)
    #     return None
