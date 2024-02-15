import os
import copy
import time
import numbers
from functools import cache
from clingo import Function, Number, Tuple_
from itertools import chain, combinations
import pkg_resources
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog, format_literal, rule_is_recursive, theory_subsumes, format_prog2
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

def generalisations(prog, allow_headless=True, recursive=False):

    if len(prog) == 1:
        rule = list(prog)[0]
        head, body = rule

        if allow_headless:
            if head and len(body) > 0:
                new_rule = (None, body)
                new_prog = [new_rule]
                yield new_prog

        if (recursive and len(body) > 2 and head) or (not recursive and len(body) > 1):
            body = list(body)
            for i in range(len(body)):
                # do not remove recursive literals
                if recursive and body[i].predicate == head.predicate:
                    continue
                new_body = body[:i] + body[i+1:]
                new_rule = (head, frozenset(new_body))
                new_prog = [new_rule]
                yield new_prog

    else:
        prog = list(prog)
        for i in range(len(prog)):
            subrule = prog[i]
            recursive = rule_is_recursive(subrule)
            for new_subrule in generalisations([subrule], allow_headless=False, recursive=recursive):
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

@cache
def head_connected(rule):
    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    if not any(x in head_connected_vars for literal in body for x in literal.arguments):
        return False

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

def seen_more_general_unsat(prog, unsat):
    return any(theory_subsumes(seen, prog) for seen in unsat)

def seen_more_specific_sat(prog, sat):
    return any(theory_subsumes(prog, seen) for seen in sat)