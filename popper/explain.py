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
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive
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

def headless_hash(subprog):
    rule = subprog[0]
    head, body = rename_variables(rule)
    return hash(frozenset(body))

class Explainer:

    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_prog = set()

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
        return self.explain_totally_incomplete_aux(prog, directions, 0, set(), set())

    def explain_totally_incomplete_aux(self, prog, directions, depth, sat=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        for subprog in find_subprogs(prog, has_recursion):
            headless = is_headless(subprog)

            if headless:
                k = headless_hash(subprog)
            else:
                k = prog_hash(subprog)

            if k in self.seen_prog:
                continue

            self.seen_prog.add(k)

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

            unsat.add(raw_prog)
            xs = list(self.explain_totally_incomplete_aux(subprog, directions, depth+1, sat, unsat))
            if len(xs):
                yield from xs
            else:
                yield subprog, headless

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

def find_subrules(rule, force_head, recursive):
    for rule in find_subrules_aux(rule, force_head, recursive):
        head, body = rule
        if head and not head_connected(rule):
            continue
        if not head and not connected(body):
            continue
        if not has_valid_directions(rule):
            continue
        if head and recursive and not rule_is_recursive(rule) and singleton_head(rule):
            continue
        # %% head input arg in a recursive rule must appear in the bod
        yield rule

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

# TODO: THIS CHECK IS NOT COMPLETE
# IT DOES NOT ACCOUNT FOR VARIABLE RENAMING
# R1 = (None, frozenset({('c3', ('A',)), ('c2', ('A',))}))
# R2 = (None, frozenset({('c3', ('B',)), ('c2', ('B',), true_value(A,B))}))
def rule_subsumes(r1, r2):
    # r1 subsumes r2 if r1 is a subset of r2
    h1, b1 = r1
    h2, b2 = r2
    if h1 != None and h2 == None:
        return False
    return b1.issubset(b2)

def theory_subsumes(prog1, prog2):
    # P1 subsumes P2 if for every rule R2 in P2 there is a rule R1 in P1 such that R1 subsumes R2
    return all(any(rule_subsumes(r1, r2) for r1 in prog1) for r2 in prog2)

def seen_more_general_unsat(prog, unsat):
    return any(theory_subsumes(seen, prog) for seen in unsat)

def seen_more_specific_sat(prog, sat):
    return any(theory_subsumes(prog, seen) for seen in sat)