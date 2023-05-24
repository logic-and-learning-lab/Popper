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

def get_raw_prog2(prog):
    xs = set()
    for head, body in prog:
        if head:
            new_head = (head.predicate, head.arguments)
        else:
            new_head = None
        new_body = frozenset((atom.predicate, atom.arguments) for atom in body)
        new_rule = (new_head, new_body)
        xs.add(new_rule)
    return frozenset(xs)

def prog_hash(prog):
    new_prog = get_raw_prog(prog)
    return hash(new_prog)

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

class Explainer:

    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.seen_prog = set()
        self.savings = 0
        self.unsat = set()

    def add_seen(self, prog):
        self.seen_prog.add(get_raw_prog(prog))
        self.seen_prog.add(get_raw_prog2(prog))

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
        return list(self.explain_totally_incomplete_aux2(prog, directions, set(), set()))

    def explain_totally_incomplete_aux2(self, prog, directions, sat=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        out = []
        for subprog in generalisations(prog, has_recursion):
            raw_prog2 = get_raw_prog2(subprog)
            if raw_prog2 in self.seen_prog:
                continue

            raw_prog = get_raw_prog(subprog)
            if raw_prog in self.seen_prog:
                continue

            self.seen_prog.add(raw_prog)
            self.seen_prog.add(raw_prog2)

            def should_skip():
                if len(subprog) > 0:
                    return False
                h_, b_ = list(subprog)[0]
                for x in non_empty_powerset(b_):
                    if get_raw_prog2([(None,x)]) in self.unsat:
                        return True
                    if get_raw_prog2([(h_,x)]) in self.unsat:
                        return True
                return False

            if should_skip():
                continue

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if seen_more_general_unsat(raw_prog2, unsat):
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

            headless = is_headless(subprog)


            if headless:
                # print('testing A')
                # print(format_prog(test_prog))
                body = test_prog[0][1]
                if self.tester.is_body_sat(order_body(body)):
                    sat.add(raw_prog)
                    continue
            else:
                # print('testing B')
                # print(format_prog(test_prog))
                if self.tester.is_sat(test_prog):
                    sat.add(raw_prog)
                    continue

            unsat.add(raw_prog)
            unsat.add(raw_prog2)
            self.unsat.add(raw_prog)
            self.unsat.add(raw_prog2)

            xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))
        return out

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

    if len(prog) == 1:
        return True

    # if more than two rules then there must be recursion
    has_recursion = False
    for rule in prog:
        h, b = rule

        if h == None:
            return False

        if rule_is_recursive(rule):
            has_recursion = True
            h, b = rule
            if len(b) == 1:
                return False

    if not has_recursion:
        return False


    if needs_datalog(prog) and not tmp(prog):
        return False

    return True

def needs_datalog(prog):
    for rule in prog:
        rec_outputs = set()
        non_rec_inputs = set()
        head, body = rule
        for literal in body:
            if literal.predicate == head.predicate:
                rec_outputs.update(literal.outputs)
            else:
                # if any(x in xr)
                non_rec_inputs.update(literal.inputs)
        if any(x in rec_outputs for x in non_rec_inputs):
            return True
    return False


def tmp(prog):
    for rule in prog:
        head, body = rule
        body_args = set(x for atom in body for x in atom.arguments)
        if any(x not in body_args for x in head.arguments):
            return False
    return True




# f(A,B):- empty(A).
# f(A,B):- tail(A,D),f(D,C),increment(C,B).

# if an output argument of a recursive literal is used as an input argument to another literal then all head arguments must appear in the body


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