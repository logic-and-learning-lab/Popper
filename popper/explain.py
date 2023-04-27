import os
import copy
import time
import numbers
import numpy as np
from clingo import Function, Number, Tuple_
from itertools import chain, combinations
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive, theory_subsumes
from . core import Literal
import clingo
import clingo.script

def rule_hash(rule):
    head, body = rule
    body = frozenset((lit.predicate, lit.arguments) for lit in body)
    if head:
        new_rule = ((head.predicate, head.arguments), body)
    else:
        new_rule = (False, body)
    return hash(new_rule)

def rename_variables(rule):
    head, body = rule
    if head:
        head_vars = set(head.arguments)
        head = (head.predicate, head.arguments)
    else:
        head_vars = set()
    next_var = len(head_vars)
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
    return (head, new_body)

def get_raw_prog(prog):
    xs = set()
    for rule in prog:
        h, b = rename_variables(rule)
        xs.add((h, frozenset(b)))
    return frozenset(xs)

def prog_hash(prog):
    new_prog = get_raw_prog(prog)
    return hash(new_prog)


def prog_hash2(prog):
    xs = set()
    for rule in prog:
        h, b = rule
        xs.add((h, frozenset(b)))
    return frozenset(xs)

def headless_hash(subprog):
    rule = subprog[0]
    head, body = rename_variables(rule)
    return hash(frozenset(body))

def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))


pruned = set()
seen = set()

def get_my_key(body):

    # h = None
    # if head:
        # h = (head.predicate, head.arguments)
    return frozenset((x.predicate, x.arguments) for x in body)

# def get_my_key(body):

    # h = None
    # if head:
        # h = (head.predicate, head.arguments)
    # return frozenset((x[0], x.arguments) for x in body)

class Explainer:

    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_prog = set()
        self.savings = 0

    def add_seen(self, prog):
        k = prog_hash(prog)
        self.seen_prog.add(k)

    def build_test_prog(self, subprog, directions):
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
        return test_prog

    def explain_totally_incomplete(self, prog, directions):
        # return list(self.explain_totally_incomplete_aux(prog, directions, 0, set(), set()))
        return list(self.explain_totally_incomplete_aux2(prog, directions, set(), set()))


    def my_tmp(self, rule):
        head, body = rule

        out = []

        for b in powerset(body):

            if len(b) == 0:
                continue

            for h in [head, None]:
                new_rule = (h, b)

                if not has_valid_directions(new_rule):
                    continue

                if h == None and not connected(b):
                    continue

                if h != None and not head_connected(new_rule):
                    continue

                if h != None and len(b) == len(body):
                    continue

                out.append(new_rule)

        def order_by_size(rule):
            h, b = rule
            if h:
                return 1 + len(b)
            return len(b)

        xs = sorted(out, key=order_by_size)
        # for x in xs:
            # print('X\t',format_rule(x))
        return xs

    def explain_totally_incomplete_aux2(self, prog, directions, sat=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        # print('--')
        # print(format_prog(prog))
        # print('\t', format_prog(x))

        out = []
        for subprog in generalisations(prog, has_recursion):

            headless = is_headless(subprog)

            if headless:
                k = headless_hash(subprog)
            else:
                k = prog_hash(subprog)

            if k in self.seen_prog:
                continue

            raw_prog = get_raw_prog(subprog)

            self.seen_prog.add(k)

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if not prog_is_ok(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat)
                out.extend(xs)
                continue

            if self.tester.has_redundant_literal(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat)
                out.extend(xs)
                continue

            if len(subprog) > 2 and self.tester.has_redundant_rule(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat)
                out.extend(xs)
                continue

            test_prog = self.build_test_prog(subprog, directions)

            # print('asda1', format_prog(subprog), raw_prog)
            # print('asda2', test_prog)

            if headless:
                body = test_prog[0][1]
                if self.tester.is_body_sat(order_body(body)):
                    sat.add(raw_prog)
                    continue
            else:
                if self.tester.is_sat(test_prog):
                    sat.add(raw_prog)
                    continue

            # print(format_rule())
            # print('UNSAT', format_prog(subprog), raw_prog)
            unsat.add(raw_prog)
            xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))
        return out

    def explain_totally_incomplete_aux(self, prog, directions, depth, sat=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        # print('--')
        # print(format_prog(prog))
        # for x in find_subprogs(prog, has_recursion):
        #     print('\t', 'a', format_prog(x))
        #     for rule in x:
        #         print('\t', a, format_rule(rule))

        # for subprog in generalisations(prog, has_recursion):
        #     if not prog_is_ok(subprog):
        #         continue
        #     print('\t', '')
        #     for rule in subprog:
        #         print('\t', 'b', format_rule(rule))

        for subprog in find_subprogs(prog, has_recursion):
        # for subprog in generalisations(prog, has_recursion):

            headless = is_headless(subprog)

            if headless:
                k = headless_hash(subprog)
            else:
                k = prog_hash(subprog)

            if k in self.seen_prog:
                continue

            self.seen_prog.add(k)
            # print('prog', k)
            # for rule in subprog:
                # print(format_rule(rule))

            raw_prog = get_raw_prog(subprog)

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if seen_more_specific_sat(raw_prog, sat):
                continue

            if self.tester.has_redundant_literal(subprog):
                continue

            if len(subprog) > 2 and self.tester.has_redundant_rule(subprog):
                continue

            if len(subprog) > 1 and has_recursion and any(not recursive_input_is_ok(rule) for rule in subprog):
                continue

            test_prog = self.build_test_prog(subprog, directions)

            if headless:
                body = test_prog[0][1]
                if self.tester.is_body_sat(order_body(body)):
                    sat.add(raw_prog)
                    continue
            else:
                if self.tester.is_sat(test_prog):
                    sat.add(raw_prog)
                    continue

            # print(format_rule())
            unsat.add(raw_prog)
            xs = list(self.explain_totally_incomplete_aux(subprog, directions, depth+1, sat, unsat))
            if len(xs):
                yield from xs
            else:
                yield subprog, headless

# @profile
def has_valid_directions(rule):
    head, body = rule

    if head:
        if len(head.inputs) == 0:
            return True

        grounded_variables = head.inputs
        body_literals = set(body)

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

def find_subrules(rule, force_head, recursive):

    for subrule in find_subrules_aux(rule, force_head, recursive):
        head, body = subrule

        if head and not head_connected(subrule):
            yield from find_subrules(subrule, force_head, recursive)
            continue

        if not head and not connected(body):
            yield from find_subrules(subrule, force_head, recursive)
            continue

        if not has_valid_directions(subrule):
            yield from find_subrules(subrule, force_head, recursive)
            continue

        if head and recursive and not rule_is_recursive(subrule) and singleton_head(subrule):
            yield from find_subrules(subrule, force_head, recursive)
            continue

        # %% head input arg in a recursive rule must appear in the bod
        yield subrule

def find_subrules_aux(rule, force_head, recursive):
    head, body = rule

    if recursive and rule_is_recursive(rule):
        # recursive literals
        b1 = frozenset(literal for literal in body if literal.predicate == head.predicate)
        # non-recursive literals
        b2 = frozenset(literal for literal in body if literal.predicate != head.predicate)

        if len(b2) > 2:
            for b in combinations(b2, len(b2)-1):
                yield (head, b1.union(b))
        return

    if head != None and len(body) > 1:
        for b in combinations(body, len(body)-1):
            yield (head, b)

        if not force_head and len(body) > 1:
            yield (None, body)

    if head == None and len(body) > 2:
        for b in combinations(body, len(body)-1):
            yield (None, b)

def find_subprogs(prog, recursive):
    prog = list(prog)
    force_head = len(prog) > 1

    for i in range(len(prog)):
        rule = prog[i]
        for subrule in find_subrules(rule, force_head, recursive):
            yield prog[:i] + [subrule] + prog[i+1:]


def generalisations(prog, recursive):

    if len(prog) == 1:
        rule = list(prog)[0]
        head, body = rule

        if head and len(body) > 0:
            new_rule = (None, body)
            new_prog = [new_rule]
            yield new_prog

        if len(body) > 1:
            body = list(body)
            for i in range(len(body)):
                new_body = body[:i] + body[i+1:]
                new_rule = (head, frozenset(new_body))
                new_prog = [new_rule]
                yield new_prog

    else:

        prog = list(prog)
        for i in range(len(prog)):
            subrule = prog[i]
            for new_subrule in generalisations([subrule],rule_is_recursive(subrule)):
                new_prog = prog[:i] + new_subrule + prog[i+1:]
                yield new_prog


def prog_is_ok(prog):
    for rule in prog:
        head, body = rule
        if head and not head_connected(rule):
            return False

        if not head and not connected(body):
            return False

        if not has_valid_directions(rule):
            return False

    if len(prog) > 1 and any(h == None for h, b in prog):
        return False

    if len(prog) > 1 and sum(1 for rule in prog if not rule_is_recursive(rule)) > 1:
        return False

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

cached_head_connected = {}
def head_connected(rule):
    k = prog_hash([rule])
    if k in cached_head_connected:
        return cached_head_connected[k]

    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    if not any(x in head_connected_vars for literal in body for x in literal.arguments):
        cached_head_connected[k] = False
        return False

    result = True
    while body_literals:
        changed = False
        for literal in body_literals:
            if any (x in head_connected_vars for x in literal.arguments):
                head_connected_vars.update(literal.arguments)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            result = False
            break

    cached_head_connected[k] = result
    return result

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

def singleton_head(rule):
    head, body = rule
    head_vars = set(head.arguments)
    for b in body:
        head_vars = head_vars.difference({b.arguments})
    if head_vars:
        return True
    return False

def recursive_input_is_ok(rule):
    head, body = rule
    body_vars = set(x for literal in body for x in literal.arguments)
    for x in head.inputs:
        if x not in body_vars:
            return False
    return True

def is_headless(prog):
    return any(head == None for head, body in prog)

def seen_more_general_unsat(prog, unsat):
    return any(theory_subsumes(seen, prog) for seen in unsat)

def seen_more_specific_sat(prog, sat):
    return any(theory_subsumes(prog, seen) for seen in sat)