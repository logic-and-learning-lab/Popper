import os
import time
import numpy as np
import pkg_resources
# from pyswip import Prolog
# from pyswip.prolog import PrologError
import janus_swi as janus
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive, rule_size, calc_prog_size

import clingo
import clingo.script
import pkg_resources
from . core import Literal
from . explain import prog_hash, get_raw_prog
from collections import defaultdict

rule_vars = set(['A','B','C','D','E','F'])
def janus_format_rule(rule):
    out = []
    for x in rule:
        if x in rule_vars:
            out.append('_')
        out.append(x)
    return ''.join(out)

class Tester():



    def query(self, query, key):
        # print(query)
        x = janus.query_once(query)
        result = x[key]
        # print(result)
        # result = next(self.prolog.query(query))[key]
        return set(result)

    def bool_query(self, query,):
        # print(query)
        x = janus.query_once(query)
        return x['truth']

    def __init__(self, settings):
        self.settings = settings

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        # with self.settings.stats.duration('load data'):
        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            janus.consult(x)

        # load examples
        janus.query_once('load_examples')
        # >>> janus.query_once("findall(_GP, parent(Me, _P), parent(_P, _GP), GPs)",
               # {'Me':'Jan'})["GPs"]

        self.pos_index = self.query('findall(_K, pos_index(_K, _Atom), Xs)', 'Xs')
        self.neg_index = self.query('findall(_K, neg_index(_K, _Atom), Xs)', 'Xs')

        self.num_pos = len(self.pos_index)
        self.num_neg = len(self.neg_index)


        # self.cached_covers_any = {}
        # self.cached_covers_any2 = {}
        self.cached_pos_covered = {}
        self.cached_inconsistent = {}
        self.cached_redundant = {}

        self.cached_neg_covers = {}
        self.savings = 0

        # weird
        self.settings.pos_index = self.pos_index
        self.settings.neg_index = self.neg_index

        if self.settings.recursion_enabled:
            janus.query_once(f'assert(timeout({self.settings.eval_timeout})),fail')

    def test_prog(self, prog):
        if len(prog) == 1:
            return self.test_single_rule(prog)
        try:
            with self.using(prog):
                pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
                inconsistent = False
                if len(self.neg_index) > 0:
                    inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
        except PrologError as err:
            print('PROLOG ERROR',err)
            pos_covered = set()
            inconsistent = True

        # self.cached_pos_covered[k] = pos_covered
        return pos_covered, inconsistent

    def test_prog_all(self, prog):
        if len(prog) == 1:
            return self.test_single_rule_all(prog)
        # try:
        with self.using(prog):
            pos_covered = frozenset(janus.query_once(f'pos_covered(S)')['S'])
            neg_covered = frozenset(janus.query_once(f'neg_covered(S)')['S'])
                # neg_covered = frozenset(self.query('neg_covered(Xs)', 'Xs'))
        # except PrologError as err:
        #     print('PROLOG ERROR',err)
        #     pos_covered = set()
        #     neg_covered = set()
        return pos_covered, neg_covered

    def test_prog_pos(self, prog):
        if len(prog) == 1:
            return self.test_single_rule_pos(prog)
        try:
            with self.using(prog):
                pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
        except PrologError as err:
            print('PROLOG ERROR',err)
            pos_covered = set()
        return pos_covered

    # @profile
    def test_prog_inconsistent(self, prog):
        if len(prog) == 1:
            return self.test_single_inconsistent(prog)
        try:
            with self.using(prog):
                if len(self.neg_index) > 0:
                    return len(list(self.prolog.query("inconsistent"))) > 0
        except PrologError as err:
            print('PROLOG ERROR',err)
            # pos_covered = set()
            # inconsistent = True
        # self.cached_pos_covered[k] = pos_covered
        return True

    # @profile
    def test_single_rule(self, prog):
        pos_covered = frozenset()
        inconsistent = False
        try:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            q = f'findall(ID, (pos_index(ID,{atom_str}),({body_str}->  true)), Xs)'
            xs = next(self.prolog.query(q))
            pos_covered = frozenset(xs['Xs'])
            inconsistent = False
            if len(self.neg_index) > 0:
                q = f'neg_index(Id,{atom_str}),{body_str},!'
                xs = list(self.prolog.query(q))
                if len(xs) > 0:
                    inconsistent = True

        except PrologError as err:
            print('PROLOG ERROR',err)
        return pos_covered, inconsistent

    def test_single_inconsistent(self, prog):
        inconsistent = False
        # try:
        rule = list(prog)[0]
        head, _body = rule
        head, ordered_body = order_rule(rule, self.settings)
        atom_str = janus_format_rule(format_literal(head))
        body_str = janus_format_rule(format_rule((None,ordered_body))[2:-1])
        inconsistent = False
        if len(self.neg_index) > 0:
            q = f'neg_index(_Id,{atom_str}),{body_str},!'
            inconsistent = self.bool_query(q)
        # except PrologError as err:
            # print('PROLOG ERROR',err)
        return inconsistent

    def test_single_rule_all(self, prog):
        pos_covered = frozenset()
        neg_covered = frozenset()
        try:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            q = f'findall(ID, (pos_index(ID,{atom_str}),({body_str}->  true)), Xs)'
            xs = next(self.prolog.query(q))
            pos_covered = frozenset(xs['Xs'])
            if len(self.neg_index) > 0:
                q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
                xs = next(self.prolog.query(q))
                neg_covered = frozenset(xs['Xs'])

        except PrologError as err:
            print('PROLOG ERROR',err)
        return pos_covered, neg_covered



    def test_single_rule_pos(self, prog):
        pos_covered = frozenset()
        # try:
        rule = list(prog)[0]
        head, _body = rule
        head, ordered_body = order_rule(rule, self.settings)
        atom_str = janus_format_rule(format_literal(head))
        body_str = janus_format_rule(format_rule((None,ordered_body))[2:-1])
        q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
        xs = self.query(q, 'S')
        pos_covered = frozenset(xs)

        # except PrologError as err:
        #     print('PROLOG ERROR',err)
        return pos_covered

    def test_single_rule_neg(self, prog):
        # pos_covered = frozenset()
        neg_covered = frozenset()
        try:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            if len(self.neg_index) > 0:
                q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
                xs = next(self.prolog.query(q))
                neg_covered = frozenset(xs['Xs'])

        except PrologError as err:
            print('PROLOG ERROR',err)
        return neg_covered

    def test_single_rule_neg_at_most_k(self, prog, k):
        # pos_covered = frozenset()
        neg_covered = frozenset()
        try:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            if len(self.neg_index) > 0:
                # q = f'findall(ID, limit({k},(neg_index(ID,{atom_str}),({body_str}->  true))), Xs)'
                q = f'findfirstn({k}, ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
                xs = next(self.prolog.query(q))
                neg_covered = frozenset(xs['Xs'])

        except PrologError as err:
            print('PROLOG ERROR',err)
        return neg_covered

    def test_single_rule_neg_at_most_k2(self, prog, k):
        # pos_covered = frozenset()
        neg_covered = frozenset()
        try:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            if len(self.neg_index) > 0:
                q = f'findfirstn({k}, ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
                # q = f'findfirstn(ID, limit({k},(neg_index(ID,{atom_str}),({body_str}->  true))), Xs)'
                xs = next(self.prolog.query(q))
                neg_covered = frozenset(xs['Xs'])

        except PrologError as err:
            print('PROLOG ERROR',err)
        return neg_covered

# succeeds_k_times(Goal,Body,Times):-
# Counter = counter(0),
# Goal,
# once(Body),
# arg(1, Counter, N0),
# N is N0 + 1,
# ((N>=Times -> true,!);
# (nb_setarg(1, Counter, N),
# fail)).

    def is_inconsistent(self, prog):
        if len(self.neg_index) == 0:
            return False
        k = prog_hash(prog)
        if k in self.cached_inconsistent:
            return self.cached_inconsistent[k]
        with self.using(prog):
            inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
            self.cached_inconsistent[k] = inconsistent
            return inconsistent

    def is_complete(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            return len(pos_covered) == len(self.pos_index)

    def get_pos_covered(self, prog, ignore=True):
        k = prog_hash(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k]

        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = janus_format_rule(format_literal(head))
            body_str = janus_format_rule(format_rule((None,ordered_body))[2:-1])
            q = f'findall(_ID, (pos_index(_ID, {atom_str}),({body_str}->  true)), S)'
            xs = self.query(q, 'S')
            # x = janus.query_once('redundant_literal(X)', {'
            # xs = next(self.prolog.query(q))
            pos_covered = frozenset(xs)
        else:
            with self.using(prog):
                pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
        self.cached_pos_covered[k] = pos_covered
        return pos_covered

    def get_neg_covered(self, prog):
         with self.using(prog):
            return frozenset(self.query('neg_covered(Xs)', 'Xs'))

    def get_neg_covered2(self, prog):
        k = prog_hash(prog)
        assert(k not in self.cached_neg_covers)

        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
            xs = next(self.prolog.query(q))
            self.cached_neg_covers[k] = xs
            return frozenset(xs['Xs'])
        else:
            with self.using(prog):
                xs = frozenset(self.query('neg_covered(Xs)', 'Xs'))
                self.cached_neg_covers[k] = xs
                return xs


    def get_neg_uncovered(self, prog):
        with self.using(prog):
            return frozenset(self.query('neg_uncovered(Xs)', 'Xs'))

    def is_more_inconsistent(self, prog, neg_covered):
        with self.using(prog):
            return len(list(self.prolog.query(f"is_more_inconsistent({neg_covered})"))) > 0

    # def tmp(self, prog1, prog2):
    #     current_clauses = set()
    #     try:
    #         for rule in prog1:
    #             head, _body = rule
    #             head.predicate = 'prog1'
    #             x = format_rule(order_rule(rule, self.settings))[:-1]
    #             self.prolog.assertz(x)
    #             current_clauses.add((head.predicate, head.arity))
    #         for rule in prog2:
    #             head, _body = rule
    #             head.predicate = 'prog2'
    #             x = format_rule(order_rule(rule, self.settings))[:-1]
    #             self.prolog.assertz(x)
    #             current_clauses.add((head.predicate, head.arity))
    #         return len(list(self.prolog.query(f"covers_more"))) > 0
    #     finally:
    #         for predicate, arity in current_clauses:
    #             args = ','.join(['_'] * arity)
    #             self.prolog.retractall(f'{predicate}({args})')

        # with self.using(prog):


    # def covers_any(self, prog, neg):
    #     rule = list(prog)[0]
    #     # k = rule_hash(rule)
    #     # if k in self.cached_covers_any:
    #     #     for x in self.cached_covers_any[k]:
    #     #         if x in neg:
    #     #             return True
    #     # self.cached_covers_any[k] = set()

    #     with self.using(prog):
    #         xs = list(self.prolog.query(f"covers_any({neg},ID)"))
    #         if len(xs) > 0:
    #             ex = xs[0]['ID']
    #             # print(ex)
    #             # self.cached_covers_any[k].add(ex)
    #             return True
    #         return False

    # def covers_any2(self, prog, neg):
    #     rule = list(prog)[0]
    #     # k = rule_hash(rule)
    #     # if k in self.cached_covers_any:
    #     #     for x in self.cached_covers_any[k]:
    #     #         if x in neg:
    #     #             return True
    #     # self.cached_covers_any[k] = set()

    #     rule = list(prog)[0]
    #     head, _body = rule
    #     head, ordered_body = order_rule(rule, self.settings)
    #     atom_str = format_literal(head)
    #     body_str = format_rule((None,ordered_body))[2:-1]
    #     # q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
    #     q = f'member(Id,{neg}),neg_index(Id,{atom_str}),{body_str},!'
    #     # print(q)
    #     xs = list(self.prolog.query(q))
    #     # print(xs)
    #     return len(xs) > 0

    #     # return frozenset(xs['Xs'])

    #     # with self.using(prog):
    #     #     xs = list(self.prolog.query(f"covers_any({neg},ID)"))
    #     #     if len(xs) > 0:
    #     #         ex = xs[0]['ID']
    #     #         # print(ex)
    #     #         self.cached_covers_any[k].add(ex)
    #     #         return True
    #     #     return False


    def covers_any3(self, prog, neg):
        # k = rule_hash(rule)
        rule = list(prog)[0]
        # k = prog_hash(prog)
        # if k in self.cached_covers_any:
        #     for x in self.cached_covers_any[k]:
        #         if x in neg:
        #             return True
        # if k in self.cached_covers_any2:
        #     for x in self.cached_covers_any2[k]:
        #         if x in neg:
        #             return True
        #             # print('MOOCOWJONES!!!!!!!!')
        # self.cached_covers_any[k] = set()

        # for rule in prog:
            # print('\tcalling prolog', format_rule(rule))

        rule = list(prog)[0]
        head, _body = rule
        head, ordered_body = order_rule(rule, self.settings)
        atom_str = format_literal(head)
        body_str = format_rule((None,ordered_body))[2:-1]
        # q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
        q = f'member(Id,{neg}),neg_index(Id,{atom_str}),{body_str},!'
        # print(q)
        xs = list(self.prolog.query(q))
        if len(xs) > 0:
            ex = xs[0]['Id']
            # self.cached_covers_any[k].add(ex)
            return True
        # print(xs)
        return False

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


    @contextmanager
    def using(self, prog):
        if self.settings.recursion_enabled:
            prog = order_prog(prog)
        current_clauses = set()
        # try:
        str_prog = []
        for rule in prog:
            head, _body = rule
            x = format_rule(order_rule(rule, self.settings))[:-1]
            str_prog.append(x)
            current_clauses.add((head.predicate, head.arity))
        # next_value/2'
        for head, arity in current_clauses:
            str_prog.append(f':- dynamic {head}/{arity}')
        str_prog = '.\n'.join(str_prog) +'.'
        print('prog', str_prog)
        janus.consult('prog', str_prog)
        yield
        # finally:
        for predicate, arity in current_clauses:
            args = ','.join(['_'] * arity)
            x = janus.query_once(f"retractall({predicate}({args}))")
            # self.prolog.retractall(f'({args})')

    def is_non_functional(self, prog):
        with self.using(prog):
            return self.bool_query('non_functional')

    def reduce_inconsistent(self, program):
        if len(program) < 3:
            return program
        for i in range(len(program)):
            subprog = program[:i] + program[i+1:]
            if not prog_is_recursive(subprog):
                continue
            with self.using(subprog):
                if self.is_inconsistent(subprog):
                    return self.reduce_inconsistent(subprog)
        return program

    def is_sat(self, prog, noise=False):
        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            if noise:
                new_head = f'pos_index(ID,{format_literal(head)})'
                x = format_rule((None,ordered_body))[2:-1]
                x = f'succeeds_k_times({new_head},({x}),{rule_size(rule)}),!'
                return self.bool_query(x)
            else:
                head = f'pos_index(_,{format_literal(head)})'
                x = format_rule((None,ordered_body))[2:-1]
                x = f'{head},{x},!'
                return self.bool_query(x)
        else:
            with self.using(prog):
                if noise:
                    return self.bool_query(f'covers_at_least_k_pos({calc_prog_size(prog)})')
                else:
                    return self.bool_query('sat')

    def is_body_sat(self, body):
        _, ordered_body = order_rule((None,body), self.settings)
        body_str = ','.join(format_literal(literal) for literal in ordered_body)
        query = body_str + ',!'
        query = f'catch(call_with_time_limit(0.1, ({query})),time_limit_exceeded,true)'
        return self.bool_query(query)

    def check_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
            res = list(self.prolog.query(f'redundant_literal({c})'))
            if res:
                yield rule

    # @profile
    def has_redundant_literal(self, prog):
        # t1 = time.time()
        k = prog_hash(prog)
        out = None
        if k in self.cached_redundant:
            out = self.cached_redundant[k]
        else:
            for rule in prog:
                head, body = rule
                if head:
                    c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
                else:
                    c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
                # res = list(self.prolog.query(f'redundant_literal({c})'))
                res = self.bool_query('redundant_literal(X)', {'X':c})
                if res:
                    self.cached_redundant[k] = True
                    break
                    # return True
        self.cached_redundant[k] = False
        out = self.cached_redundant[k]
        # d1 = time.time()-t1

        # t1 = time.time()
        # for rule in prog:
        #     head, body = rule
        #     if head:
        #         c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
        #     else:
        #         c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
        #     res = list(self.prolog.query(f'redundant_literal({c})'))
        #     if res:
        #         break
        #         # return True
        # # return False
        # d2 = time.time()-t1
        # self.savings += d1-d2
        # print(self.savings)
        return out
        # if k in self.cached_redundant:
            # return self.cached_redundant[k]



    def has_redundant_rule_(self, prog):
        prog_ = []
        for head, body in prog:
            c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        return len(list(self.prolog.query(f'redundant_clause({prog_})'))) > 0
        # return self.bool_query(f'redundant_clause({prog_})')

    def has_redundant_rule(self, prog):
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses

        base = []
        step = []
        for rule in prog:
            if rule_is_recursive(rule):
                step.append(rule)
            else:
                base.append(rule)
        if len(base) > 1 and self.has_redundant_rule_(base):
            return True
        if len(step) > 1 and self.has_redundant_rule_(step):
            return True
        return False

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








    def has_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
            # res = list(self.prolog.query(f'redundant_literal({c})'))
            # res = self.bool_query(
            # print(c)
            x = janus.query_once('redundant_literal(X)', {'X':c})
            res = x['truth']
            if res:
                return True
        return False

    def has_redundant_rule_(self, prog):
        prog_ = []
        for head, body in prog:
            c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        return len(list(self.prolog.query(f'redundant_clause({prog_})'))) > 0
        # return self.bool_query(f'redundant_clause({prog_})')

    def has_redundant_rule(self, prog):
        # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses

        base = []
        step = []
        for rule in prog:
            if rule_is_recursive(rule):
                step.append(rule)
            else:
                base.append(rule)
        if len(base) > 1 and self.has_redundant_rule_(base):
            return True
        if len(step) > 1 and self.has_redundant_rule_(step):
            return True
        return False

    # WE ASSUME THAT THERE IS A REUNDANT RULE
    def find_redundant_rule_(self, prog):
        prog_ = []
        for i, (head, body) in enumerate(prog):
            c = f"{i}-[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            prog_.append(c)
        prog_ = f"[{','.join(prog_)}]"
        # print(prog_)
        q = f'find_redundant_rule({prog_},K1,K2)'
        res = list(self.prolog.query(q))
        k1 = res[0]['K1']
        k2 = res[0]['K2']
        return prog[k1], prog[k2]
        # return len(res) > 0

    def find_redundant_rules(self, prog):
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