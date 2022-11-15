import os
import copy
import time
import numbers
import numpy as np
from clingo import Function, Number, Tuple_
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive
from . core import Literal
import clingo
import clingo.script

def prog_hash(prog):
    rules = set()
    for rule in prog:
        head, body = rule
        body = frozenset((lit.predicate, lit.arguments) for lit in body)
        if head:
            new_rule = ((head.predicate, head.arguments), body)
        else:
            new_rule = (False, body)
        rules.add(new_rule)
    return hash(frozenset(rules))

def literal_hash(literal):
    return hash((literal.predicate, literal.arguments))

def vars_to_ints(vars):
    return tuple(ord(v) - 65 for v in vars)

def arg_to_symbol(arg):
    if isinstance(arg, numbers.Number):
        return Number(arg)
    if isinstance(arg, str):
        return Function(arg)
    assert False, f'Unhandled argtype({type(arg)}) in aspsolver.py arg_to_symbol()'

def atom_to_symbol(pred, args):
    xs = tuple(arg_to_symbol(arg) for arg in args)
    return Function(name = pred, arguments = xs)

def tmp_get_new_body(head_vars, next_var, body):
    new_body = []
    lookup = {}
    for body_literal in sorted(body, key=lambda x: x.predicate):
        new_args = []
        for var in body_literal.arguments:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = chr(ord('A') + next_var)
                next_var+=1
            new_args.append(lookup[var])
        new_body.append((body_literal.predicate, tuple(new_args)))
    return new_body

def prog_hash4(prog):
    rules = []
    for rule in prog:
        head, body = rule
        if head:
            head_vars = set(head.arguments)
        else:
            head_vars = set()
        next_var = len(head_vars)
        new_body = frozenset(tmp_get_new_body(head_vars, next_var, body))
        # print(new_body)
        if head:
            new_rule = ((head.predicate, head.arguments), new_body)
        else:
            new_rule = (False, new_body)

        rules.append(new_rule)
    k3 = hash(frozenset(rules))
    return k3

def rule_hash(prog):
    rules = []
    for rule in prog:
        head, body = rule
        head_vars = set()
        next_var = len(head_vars)
        new_body = tmp_get_new_body(head_vars, next_var, body)
        new_rule = frozenset(new_body)
        rules.append(new_rule)
    k3 = hash(frozenset(rules))
    return k3

def standardise_vars(rule):
    head, body = rule
    head_vars = set(head.arguments)
    next_var = len(head_vars)
    new_body = set()
    lookup = {}
    for body_literal in sorted(list(body), key=lambda x: x.predicate):
        new_args = []
        for var in body_literal.arguments:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = chr(ord('A') + next_var)
                next_var+=1
            new_args.append(lookup[var])
        body_literal.arguments = tuple(new_args)

class Explainer:
    def load_types(self):
        enc = """
#defined clause/1.
#defined clause_var/2.
#defined var_type/3."""
        solver = clingo.Control()
        with open(self.settings.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], enc)
        solver.ground([('bias', [])])

        for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
            head_pred = x.symbol.arguments[0].name
            head_arity = x.symbol.arguments[1].number

        head_types = None
        body_types = {}
        for x in solver.symbolic_atoms.by_signature('type', arity=2):
            pred = x.symbol.arguments[0].name
            # xs = (str(t) for t in )
            xs = [y.name for y in x.symbol.arguments[1].arguments]
            if pred == head_pred:
                head_types = xs
            else:
                body_types[pred] = xs

        return head_types, body_types



    def load_directions(self):
        enc = """
#defined clause/1.
#defined clause_var/2.
#defined var_type/3."""
        solver = clingo.Control()
        with open(self.settings.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], enc)
        solver.ground([('bias', [])])
        encoding = set()
        for x in solver.symbolic_atoms.by_signature('direction', arity=2):
            pred = x.symbol.arguments[0]
            directions = x.symbol.arguments[1]
            for i, arg in enumerate(directions.arguments):
                encoding.add(f'direction({pred},{i},{arg}).')
        return encoding

    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester

        self.seen_prog = set()
        self.cached_sat = set()
        self.cached_unsat = set()

        self.cached_satbody = set()
        self.cached_unsatbody = set()
        self.cached_sat1 = set()

        self.explain_encoding = pkg_resources.resource_string(__name__, "lp/explain.pl").decode()
        self.explain_dir_encoding = pkg_resources.resource_string(__name__, "lp/explain-dirs.pl").decode()
        # self.directions = self.load_directions()
        # self.has_directions = len(self.directions) > 0

        self.tmp_count = 0
        self.seen_body = set()

    def add_seen_unsat(self, prog):
        k = prog_hash(prog)
        k2 = prog_hash4(prog)
        self.cached_unsat.add(k)
        self.cached_unsat.add(k2)

    def add_seen_sat(self, prog):
        k = prog_hash(prog)
        k2 = prog_hash4(prog)
        self.cached_sat.add(k)
        self.cached_sat.add(k2)

    # @profile
    def explain_totally_incomplete2(self, prog, directions, depth):
        return self.explain_totally_incomplete2_aux(prog, directions, depth, set(), set())

    # @profile
    def explain_totally_incomplete2_aux(self, prog, directions, depth, sat=set(), unsat=set()):
        rule = list(prog)[0]
        head, body = rule

        if len(body) == 1:
            return

        tmp = []
        tmp.extend([(head, b) for b in combinations(body, len(body)-1)])
        if head != None and len(body) > 1:
            asda = [(None, b) for b in combinations(body, len(body)) if len(b) > 1]
            tmp.extend(asda)

        # format_rule(rule)



        # for subbody in combinations(body, len(body)-1):
        for new_rule in tmp:
            # print('-'*depth, format_rule(new_rule), len(unsat))

            h, b = new_rule
            # b = frozenset(b)
            if h == None and len(b) == 1:
                continue

            head_key = None
            if h:
                head_key = (h.predicate, h.arguments)
            body_key = frozenset((lit.predicate, lit.arguments) for lit in b)


            # new_rule = head, subbody

            subprog = [new_rule]

            k1 = prog_hash(subprog)
            if k1 in self.seen_prog or k1 in self.cached_sat:
                continue

            if k1 in self.cached_unsat:
                yield subprog, False
                continue

            k2 = prog_hash4(subprog)

            if k2 in self.seen_prog or k2 in self.cached_sat:
                continue

            if k2 in self.cached_unsat:
                yield subprog, False
                continue


            # for h_, b_ in unsat:
            #     if h == h_ and b_.issubset(b):
            #         print('WTF'*10)


            k3 = False
            if len(subprog) == 1:
                _head, _body = subprog[0]
                if _head == None:
                    k3 = hash(frozenset(tmp_get_new_body(set(), 0, _body)))
                    if k3 in self.seen_prog or k3 in self.cached_satbody:
                        continue
                    if k3 in self.cached_unsatbody:
                        # build_nogood()
                        yield subprog, True
                        # continue
                    # TODO: PUSH TO SOLVER!!
                    if not connected(_body):
                        self.seen_prog.add(k1)
                        self.seen_prog.add(k2)
                        self.seen_prog.add(k3)
                        continue

            skip = False
            for _head, _body in subprog:
                if _head != None and not head_connected((_head, _body)):
                    self.seen_prog.add(k1)
                    self.seen_prog.add(k2)
                    self.seen_prog.add(k3)
                    skip = True
                if skip:
                    break
            if skip:
                continue

            for rule in subprog:
                if not has_valid_directions(rule):
                    self.seen_prog.add(k1)
                    self.seen_prog.add(k2)
                    self.seen_prog.add(k3)
                    skip = True
                if skip:
                    break
            if skip:
                continue

            # print('-')
            # print('b', ','.join(map(str, b)))
            seen_smaller_unsat = False
            for x_h, x_b in unsat:
                if x_h == None or head_key == x_h and x_b.issubset(body_key):
                    # print('SEEN SMALLER UNSAT!!!!')
                    if head_key:
                        self.cached_unsat.add(k1)
                        self.cached_unsat.add(k2)
                    else:
                        self.cached_unsat.add(k3)
                    seen_smaller_unsat = True
                    break
            if seen_smaller_unsat:
                continue

            seen_bigger_sat = False
            for x_h, x_b in sat:
                # if head_key == None or head_key == x_h and x_b.issubset(body_key):
                if head_key == None or head_key == x_h and body_key.issubset(x_b):
                    # print('SEEN BIGGER SAT!!!!')
                    if head_key:
                        self.cached_sat.add(k1)
                        self.cached_sat.add(k2)
                    else:
                        self.cached_sat.add(k3)
                    seen_bigger_sat = True
                    break
            if seen_bigger_sat:
                continue

            # with self.settings.stats.duration('check_redundant_literal'):
            if len(subprog) == 1 and len(list(self.tester.check_redundant_literal(subprog))) > 0:
                self.seen_prog.add(k1)
                self.seen_prog.add(k2)
                self.seen_prog.add(k3)
                continue

            # with self.settings.stats.duration('check_redundant_rule'):
            if len(subprog) > 1 and self.tester.has_redundant_rule(subprog):
                self.seen_prog.add(k1)
                self.seen_prog.add(k2)
                self.seen_prog.add(k3)
                continue

            # with self.settings.stats.duration('explain_prolog'):
            test_prog = []

            for head, body in subprog:
                if head:
                    head_modes = tuple(directions[head.predicate][i] for i in range(head.arity))
                    head_literal = Literal(head.predicate, head.arguments, head_modes)
                else:
                    head_literal = False
                body_literals = set()
                for body_literal in body:
                    body_modes = tuple(directions[body_literal.predicate][i] for i in range(body_literal.arity))
                    body_literals.add(Literal(body_literal.predicate, body_literal.arguments, body_modes))
                rule = head_literal, body_literals
                test_prog.append(rule)


            # for rule in subprog:
                # print(format_rule(rule))

            # prune = False

            # SPECIAL CASE WHEN THERE IS ONLY ONE HEADLESS RULE
            if len(test_prog) == 1 and test_prog[0][0] == False:
                # pass
                if self.tester.is_body_sat(order_body(test_prog[0][1])):
                    self.cached_satbody.add(k3)
                    sat.add((head_key, body_key))
                    continue
                else:
                    self.cached_unsatbody.add(k3)
                    # print('UNSAT1!', head_key, body_key)
                    # exit()
                    unsat.add((head_key, body_key))
                    yield subprog, True
                    # continue
            else:
                if self.tester.is_sat(test_prog):
                    self.cached_sat.add(k1)
                    self.cached_sat.add(k2)
                    sat.add((head_key, body_key))
                    continue
                else:
                    self.cached_unsat.add(k1)
                    self.cached_unsat.add(k2)
                    # print('UNSAT2!', head_key, body_key)
                    # print('adding', (head_key, body_key))
                    unsat.add((head_key, body_key))
                    yield subprog, False
                    # continue

            # print(format_rule(subprog[0]))

            # yield subprog
            yield from self.explain_totally_incomplete2_aux(subprog, directions, depth+2, sat, unsat)


from itertools import chain, combinations
def hacky_powerset(iterable):
    list(chain.from_iterable(combinations(s, r) for r in range(len(s)-1,1,-1)))
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(2, len(s)+1))

def has_valid_directions(rule):
    head, body = rule

    if head:
        grounded_variables = head.inputs
        body_literals = set(body)

        if head.inputs == []:
            return rule

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                if not literal.inputs.issubset(grounded_variables):
                    continue
                if literal.predicate != head.predicate:
                    # find the first ground non-recursive body literal and stop
                    selected_literal = literal
                    break
                elif selected_literal == None:
                    # otherwise use the recursive body literal
                    selected_literal = literal

            if selected_literal == None:
                return False

            grounded_variables = grounded_variables.union(selected_literal.outputs)
            body_literals = body_literals.difference({selected_literal})
        return True
    else:
        if all(len(literal.inputs) == 0 for literal in body):
            return True

        body_literals = set(body)
        grounded_variables = set()

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                if len(literal.outputs) == len(literal.arguments):
                    selected_literal = literal
                    break
                if literal.inputs.issubset(grounded_variables):
                    selected_literal = literal
                    break

            if selected_literal == None:
                return False

            grounded_variables = grounded_variables.union(selected_literal.arguments)
            body_literals = body_literals.difference({selected_literal})

        return True

def order_body(body):
    ordered_body = []
    grounded_variables = set()
    body_literals = set(body)

    while body_literals:
        selected_literal = None
        for literal in body_literals:
            if len(literal.outputs) == len(literal.arguments):
                selected_literal = literal
                break
            if literal.inputs.issubset(grounded_variables):
                selected_literal = literal
                break

        if selected_literal == None:
            message = f'{selected_literal} in clause {format_rule(rule)} could not be grounded'
            raise ValueError(message)

        ordered_body.append(selected_literal)
        grounded_variables = grounded_variables.union(selected_literal.arguments)
        body_literals = body_literals.difference({selected_literal})

    return tuple(ordered_body)

def head_connected(rule):
    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    while body_literals:
        changed = False
        for literal in body_literals:
            if any (x in head_connected_vars for x in literal.arguments):
                head_connected_vars.update(literal.arguments)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            return False

    return True



def connected(body):
    if len(body) == 1:
        return True

    body = list(body)
    connected_vars = set(body[0].arguments)
    body_literals = set(body[1:])

    while body_literals:
        changed = False
        for literal in body_literals:
            if any (x in connected_vars for x in literal.arguments):
                connected_vars.update(literal.arguments)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            return False

    return True



