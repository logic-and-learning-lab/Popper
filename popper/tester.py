import os
import time
import numpy as np
import pkg_resources
from pyswip import Prolog
from pyswip.prolog import PrologError
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive

import clingo
import clingo.script
import pkg_resources
from . core import Literal
from . explain import rule_hash, prog_hash
from . generate import parse_model
from collections import defaultdict

class Tester():

    def query(self, query, key):
        result = next(self.prolog.query(query))[key]
        return set(result)

    def bool_query(self, query,):
        return len(list(self.prolog.query(query))) > 0

    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        with self.settings.stats.duration('load data'):
            for x in [exs_pl_path, bk_pl_path, test_pl_path]:
                if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                    x = x.replace('\\', '\\\\')
                self.prolog.consult(x)

        # load examples
        self.bool_query(f'load_examples')
        self.pos_index = self.query('findall(K,pos_index(K,Atom),Xs)', 'Xs')
        self.neg_index = self.query('findall(K,neg_index(K,Atom),Xs)', 'Xs')

        self.num_pos = len(self.pos_index)
        self.num_neg = len(self.neg_index)


        self.cached_covers_any = {}
        self.cached_pos_covered = {}

        # weird
        self.settings.pos_index = self.pos_index
        self.settings.neg_index = self.neg_index

        if self.settings.recursion_enabled:
            self.prolog.assertz(f'timeout({self.settings.eval_timeout})')

        if self.settings.datalog:
            with self.settings.stats.duration('load recalls2'):
                self.load_recalls2()


            out = []

            for (pred, key), recall in self.settings.recall.items():
                # if recall == 1:
                    # continue
                if recall > 4:
                    continue
                if '1' not in key:
                    continue
                arity = len(key)
                args = [f'V{i}' for i in range(arity)]
                args_str = ','.join(args)
                subset = []
                fixer = []
                # fixer = [y for (x,y) in zip(key, args) if x == '1']

                for x, y in zip(key, args):
                    if x == '0':
                        subset.append(y)
                        fixer.append('_')
                    else:
                        fixer.append(y)


                subset_str = ','.join(subset)
                fixer_str = ','.join(fixer)
                # if len(subset) == 1:
                    # subset_str+= ','
                if len(fixer) == 1:
                    fixer_str+= ','

                # con = ':- clause(Rule), #count{' + subset_str + f': body_literal(Rule,{pred},_,({args_str}))' + '} > ' + str(recall) + '.'

                con2 = f':- clause(Rule), body_literal(Rule,{pred},_,({fixer_str})), #count{{{subset_str}: body_literal(Rule,{pred},_,({args_str}))}} > {recall}.'
                # con2 = f':- clause(Rule), #count{{{subset_str}: body_literal(Rule,{pred},_,({args_str}))}} > {recall}.'
                # print(con2)
                # con = f':- clause(Rule), {subset}'
                # print(con2)
                out.append(con2)

            self.settings.deduced_bkcons += '\n' + '\n'.join(out)

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

        self.cached_pos_covered[prog_hash(prog)] = pos_covered
        return pos_covered, inconsistent

    def test_single_rule(self, prog):
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
            q = f'neg_index(_,{atom_str}),{body_str},!'
            if len(self.neg_index) > 0:
                inconsistent = len(list(self.prolog.query(q))) > 0
        except PrologError as err:
            print('PROLOG ERROR',err)
        return pos_covered, inconsistent

    def is_inconsistent(self, prog):
        if len(self.neg_index) == 0:
            return False
        with self.using(prog):
            return len(list(self.prolog.query("inconsistent"))) > 0

    def is_complete(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            return len(pos_covered) == len(self.pos_index)

    def get_pos_covered(self, prog):
        k = prog_hash(prog)
        if k in self.cached_pos_covered:
            return self.cached_pos_covered[k]

        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            q = f'findall(ID, (pos_index(ID,{atom_str}),({body_str}->  true)), Xs)'
            xs = next(self.prolog.query(q))
            pos_covered = frozenset(xs['Xs'])
        else:
            with self.using(prog):
                pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
        self.cached_pos_covered[k] = pos_covered
        return pos_covered

    def get_neg_covered(self, prog):
         with self.using(prog):
            return frozenset(self.query('neg_covered(Xs)', 'Xs'))

    def get_neg_covered2(self, prog):
        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            atom_str = format_literal(head)
            body_str = format_rule((None,ordered_body))[2:-1]
            q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
            xs = next(self.prolog.query(q))
            return frozenset(xs['Xs'])
        else:
            with self.using(prog):
                return frozenset(self.query('neg_covered(Xs)', 'Xs'))




    def get_neg_uncovered(self, prog):
        with self.using(prog):
            return frozenset(self.query('neg_uncovered(Xs)', 'Xs'))

    def is_more_inconsistent(self, prog, neg_covered):
        with self.using(prog):
            return len(list(self.prolog.query(f"is_more_inconsistent({neg_covered})"))) > 0

    def tmp(self, prog1, prog2):
        current_clauses = set()
        try:
            for rule in prog1:
                head, _body = rule
                head.predicate = 'prog1'
                x = format_rule(order_rule(rule, self.settings))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            for rule in prog2:
                head, _body = rule
                head.predicate = 'prog2'
                x = format_rule(order_rule(rule, self.settings))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            return len(list(self.prolog.query(f"covers_more"))) > 0
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

        # with self.using(prog):


    def covers_any(self, prog, neg):
        rule = list(prog)[0]
        # k = rule_hash(rule)
        # if k in self.cached_covers_any:
        #     for x in self.cached_covers_any[k]:
        #         if x in neg:
        #             return True
        # self.cached_covers_any[k] = set()

        with self.using(prog):
            xs = list(self.prolog.query(f"covers_any({neg},ID)"))
            if len(xs) > 0:
                ex = xs[0]['ID']
                # print(ex)
                # self.cached_covers_any[k].add(ex)
                return True
            return False

    def covers_any2(self, prog, neg):
        rule = list(prog)[0]
        # k = rule_hash(rule)
        # if k in self.cached_covers_any:
        #     for x in self.cached_covers_any[k]:
        #         if x in neg:
        #             return True
        # self.cached_covers_any[k] = set()

        rule = list(prog)[0]
        head, _body = rule
        head, ordered_body = order_rule(rule, self.settings)
        atom_str = format_literal(head)
        body_str = format_rule((None,ordered_body))[2:-1]
        # q = f'findall(ID, (neg_index(ID,{atom_str}),({body_str}->  true)), Xs)'
        q = f'member(Id,{neg}),neg_index(Id,{atom_str}),{body_str},!'
        # print(q)
        xs = list(self.prolog.query(q))
        # print(xs)
        return len(xs) > 0

        # return frozenset(xs['Xs'])

        # with self.using(prog):
        #     xs = list(self.prolog.query(f"covers_any({neg},ID)"))
        #     if len(xs) > 0:
        #         ex = xs[0]['ID']
        #         # print(ex)
        #         self.cached_covers_any[k].add(ex)
        #         return True
        #     return False


    def covers_any3(self, prog, neg):
        rule = list(prog)[0]
        k = rule_hash(rule)
        if k in self.cached_covers_any:
            for x in self.cached_covers_any[k]:
                if x in neg:
                    return True
        self.cached_covers_any[k] = set()

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
            self.cached_covers_any[k].add(ex)
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
        try:
            for rule in prog:
                head, _body = rule
                x = format_rule(order_rule(rule, self.settings))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

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

    def is_sat(self, prog):
        if len(prog) == 1:
            rule = list(prog)[0]
            head, _body = rule
            head, ordered_body = order_rule(rule, self.settings)
            head = f'pos_index(_,{format_literal(head)})'
            x = format_rule((None,ordered_body))[2:-1]
            x = f'{head},{x},!'
            return self.bool_query(x)
        else:
            with self.using(prog):
                return self.bool_query('sat')

    def is_body_sat(self, body):
        _, ordered_body = order_rule((None,body), self.settings)
        body_str = ','.join(format_literal(literal) for literal in ordered_body)
        query = body_str + ',!'
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

    def has_redundant_literal(self, prog):
        for rule in prog:
            head, body = rule
            if head:
                c = f"[{','.join(('not_'+ format_literal(head),) + tuple(format_literal(lit) for lit in body))}]"
            else:
                c = f"[{','.join(tuple(format_literal(lit) for lit in body))}]"
            res = list(self.prolog.query(f'redundant_literal({c})'))
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

    # def load_recalls(self):
    #     # recall for a subset of arguments, e.g. when A and C are ground in a call to add(A,B,C)
    #     counts = {}
    #     # maximum recall for a predicate symbol
    #     counts_all = {}

    #     for pred, arity in self.settings.body_preds:
    #         counts_all[pred] = 0
    #         counts[pred] = {}

    #         args = [chr(ord('A') + i) for i in range(arity)]
    #         args_str = ','.join(args)
    #         atom1 = f'{pred}({args_str})'
    #         q = f'{atom1}'
    #         # nasty but works ok for long-running problems
    #         # we find all facts for a given predicate symbol
    #         for x in self.prolog.query(q):
    #             counts_all[pred] +=1
    #             x_args = [x[arg] for arg in args]

    #             print('A', x, x_args)

    #             # we now enumerate all subsets of possible input/ground arguments
    #             # for instance, for a predicate symbol p/2 we consider p(10) and p(01), where 1 denotes input
    #             # note that p(00) is the max recall and p(11) is 1 since it is a boolean check
    #             binary_strings = generate_binary_strings(arity)[1:-1]

    #             for var_subset in binary_strings:
    #                 if var_subset not in counts[pred]:
    #                     counts[pred][var_subset] = {}
    #                 key = []
    #                 value = []
    #                 for i in range(len(args)):
    #                     if var_subset[i] == '1':
    #                         key.append(args[i])
    #                     else:
    #                         value.append(args[i])
    #                 key = tuple(key)
    #                 value = tuple(value)
    #                 print(key, value)
    #                 if key not in counts[pred][var_subset]:
    #                     counts[pred][var_subset][key] = set()
    #                 counts[pred][var_subset][key].add(value)

    #     # we now calculate the maximum recall
    #     self.settings.recall = {}
    #     for pred, arity in self.settings.body_preds:
    #         d1 = counts[pred]
    #         self.settings.recall[(pred, '0'*arity)] = counts_all[pred]
    #         for args, d2 in d1.items():
    #             recall = max(len(xs) for xs in d2.values())
    #             self.settings.recall[(pred, args)] = recall

    def load_recalls2(self):
        # Jan Struyf, Hendrik Blockeel: Query Optimization in Inductive Logic Programming by Reordering Literals. ILP 2003: 329-346

        # recall for a subset of arguments, e.g. when A and C are ground in a call to add(A,B,C)
        counts = {}
        # maximum recall for a predicate symbol
        counts_all = {}

        with open(self.settings.bk_file) as f:
            bk = f.read()
        solver = clingo.Control(['-Wnone'])
        solver.add('base', [], bk)
        solver.ground([('base', [])])



        for pred, arity in self.settings.body_preds:
            counts_all[pred] = 0
            counts[pred] = {}
            # we find all facts for a given predicate symbol

            for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
                args = []
                for i in range(arity):
                    arg = atom.symbol.arguments[i]
                    t = arg.type
                    if t == clingo.SymbolType.Number:
                        x = arg.number
                    else:
                        x = arg.name
                    args.append(x)

                # print('X', pred, args)
                counts_all[pred] +=1
                # x_args = [x[arg] for arg in args]
                # we now enumerate all subsets of possible input/ground arguments
                # for instance, for a predicate symbol p/2 we consider p(10) and p(01), where 1 denotes input
                # note that p(00) is the max recall and p(11) is 1 since it is a boolean check
                binary_strings = generate_binary_strings(arity)[1:-1]

                for var_subset in binary_strings:
                    # print('var_subset', var_subset)
                    if var_subset not in counts[pred]:
                        counts[pred][var_subset] = {}
                    key = []
                    value = []
                    for i in range(arity):
                        if var_subset[i] == '1':
                            key.append(args[i])
                        else:
                            value.append(args[i])
                    key = tuple(key)
                    value = tuple(value)
                    # print('\t', key, value)
                    if key not in counts[pred][var_subset]:
                        counts[pred][var_subset][key] = set()
                    counts[pred][var_subset][key].add(value)

        # we now calculate the maximum recall
        self.settings.recall = {}
        for pred, arity in self.settings.body_preds:
            d1 = counts[pred]
            self.settings.recall[(pred, '0'*arity)] = counts_all[pred]
            for args, d2 in d1.items():
                recall = max(len(xs) for xs in d2.values())
                self.settings.recall[(pred, args)] = recall

        # for k, v in self.settings.recall.items():
            # print(k, v)

def generate_binary_strings(bit_count):
    binary_strings = []
    def genbin(n, bs=''):
        if len(bs) == n:
            binary_strings.append(bs)
        else:
            genbin(n, bs + '0')
            genbin(n, bs + '1')
    genbin(bit_count)
    return binary_strings
