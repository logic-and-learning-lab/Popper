import os
import copy
import time
import numbers
import numpy as np
from clingo import Function, Number, Tuple_
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog
from . core import Literal
import clingo
import clingo.script

def prog_hash(prog):
    rules = set()
    for rule in prog:
        head, body = rule
        body = frozenset((lit.predicate, lit.arguments) for lit in body)
        new_rule = ((head.predicate, head.arguments), body)
        rules.add(new_rule)
    return hash(frozenset(rules))

def literal_hash(literal):
    return hash((literal.predicate, literal.arguments))

def vars_to_ints(vars):
    return tuple(ord(v) - 65 for v in vars)

def prog_hash4(prog):
    rules = []
    for rule in prog:
        head, body = rule
        head_vars = set(head.arguments)
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
        new_rule = ((head.predicate, head.arguments), frozenset(new_body))
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

def build_explain_encoding(prog, with_directions=False):
    literal_index = {}
    encoding = set()

    head_count = 0
    body_count = 0
    size = 0

    for rule_id, rule in enumerate(prog):
        head, body = rule
        rule_vars = set()
        rule_vars.add(vars_to_ints(head.arguments))
        encoding.add(f'head_literal({rule_id},{head.predicate},{head.arity},{vars_to_ints(head.arguments)}).')
        encoding.add(f'arity({head.predicate},{head.arity}).')
        for literal in body:
            rule_vars.add(vars_to_ints(literal.arguments))
            literal_enc = f'body_literal({rule_id},{literal.predicate},{literal.arity},{vars_to_ints(literal.arguments)})'
            encoding.add(f'arity({literal.predicate},{literal.arity}).')
            encoding.add(f'{{{literal_enc}}}.')
            encoding.add(f'selected({body_count}):-{literal_enc}.')
            literal_index[body_count] = head, literal
            if literal.predicate == head.predicate:
                encoding.add(f':- not selected({body_count}).')
            body_count += 1

        size += len(body)
        if with_directions:
            for xs in rule_vars:
                encoding.add(f'vars({len(xs)},{xs}).')
    encoding.add(f':- size({size}).')
    return literal_index, encoding


class Explainer:

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

        self.explain_encoding = pkg_resources.resource_string(__name__, "lp/explain.pl").decode()
        self.explain_dir_encoding = pkg_resources.resource_string(__name__, "lp/explain-dirs.pl").decode()
        self.directions = self.load_directions()
        self.has_directions = len(self.directions) > 0

        self.tmp_count = 0

    # @profile
    def add_seen_prog(self, prog):
        k = prog_hash(prog)
        k2 = prog_hash4(prog)
        self.seen_prog.add(k)
        self.seen_prog.add(k2)

    # @profile
    # TODO: PRUNE PROGRAMS KNOWN TO BE PARTIALLY COMPLETE
    # TODO: PRUNE PROGRAMS KNOWN TO BE TOTALLY INCOMPLETE
    # TODO: ADD CONSTRAINTS THE SOLVER WHILST ENUMERATING UNSAT RULES
    # TODO: TRY RULES WITHOUT HEAD LITERALS
    # TODO: CHECK SUBSUMPTION
    #   r1 = f(A):-x(A) SAT
    #   r2 = f(A):-x(B)
    #   r3 = f(A):-x(C)
    #   if r1 is SAT, then r2 and r3 are SAT
    # ----------
    #   r1 = f(A):-x(B) UNSAT
    #   r2 = f(A):-x(A)
    #   if r1 is UNSAT, then r2 in UNSAT

    # @profile
    def explain_totally_incomplete2(self, prog, directions):
        encoding = set()
        encoding.add(self.explain_encoding)
        if self.has_directions:
            encoding.update(self.directions)
            encoding.add(self.explain_dir_encoding)
        literal_index, prog_encoding = build_explain_encoding(prog, self.has_directions)
        encoding.update(prog_encoding)

        # with open('DBG-explain.pl','w') as f:
        #     tmp = '\n'.join(sorted(list(encoding)))
        #     f.write(tmp)

        encoding = '\n'.join(encoding)

        # with self.settings.stats.duration('explain_clingo'):
        # subprogs =

        unsat_count = 0
        # print('TOTALLY INCOMPLETE')
        # for rule in prog:
            # print(format_rule(order_rule(rule)))

        for selected_literals, model in self.find_subprogs(literal_index, encoding):
            # print(selected_literals)
            subprog = parse_model4(literal_index, selected_literals)

            k1 = prog_hash(subprog)

            # Q. Is it better to push this check to the solver? I think not
            if k1 in self.seen_prog:
                # print('k1 in seen')
                continue

            if k1 in self.cached_unsat:
                # print('k1 in cached_unsat')
                continue

            if k1 in self.cached_sat:
                # print('k1 in cached_sat')
                continue

            k2 = prog_hash4(subprog)

            if k2 in self.seen_prog:
                # print('\tmoo1')
                continue

            if k2 in self.cached_unsat:
                # print('\tmoo2')
                yield subprog
                continue

            if k2 in self.cached_sat:
                # print('\tmoo3')
                continue

            print('\t', 'TESTING SUBPROG1')
            for rule in order_prog(subprog):
                print('\t', format_rule(order_rule(rule)))

            with self.settings.stats.duration('explain_prolog'):
                test_prog = []
                for head, body in subprog:
                    head_modes = tuple(directions[head.predicate][i] for i in range(head.arity))
                    # print(head_modes)
                    head_literal = Literal(head.predicate, head.arguments, head_modes)
                    body_literals = set()
                    for body_literal in body:
                        body_modes = tuple(directions[body_literal.predicate][i] for i in range(body_literal.arity))
                        # print(body_literal.predicate,body_modes)
                        body_literals.add(Literal(body_literal.predicate, body_literal.arguments, body_modes))
                    rule = head_literal, body_literals
                    test_prog.append(rule)

                # print('\t', 'TESTING SUBPROG2')
                # for rule in order_prog(test_prog):
                #     print('\t', format_rule(order_rule(rule)))

                if self.tester.is_sat(test_prog):
                    self.cached_sat.add(k1)
                    self.cached_sat.add(k2)
                else:
                    self.cached_unsat.add(k1)
                    self.cached_unsat.add(k2)
                    # unsat_count+=1


                    # ADD NOGOOD!!!!!!


                    def arg_to_symbol(arg):
                        if isinstance(arg, numbers.Number):
                            return Number(arg)
                        # if isinstance(arg, tuple):
                            # return Tuple_(tuple(arg_to_symbol(a) for a in arg))
                        if isinstance(arg, str):
                            return Function(arg)
                        assert False, f'Unhandled argtype({type(arg)}) in aspsolver.py arg_to_symbol()'

                    def atom_to_symbol(pred, args):
                        xs = tuple(arg_to_symbol(arg) for arg in args)
                        return Function(name = pred, arguments = xs)

                    nogood = []
                    for idx in selected_literals:
                        x = (atom_to_symbol('selected', (idx,)), True)
                        nogood.append(x)
                    # print('X', nogood)

                    model.context.add_nogood(nogood)
                    unsat_count +=1
                    # if unsat_count > 0:
                    print('PRUNE', unsat_count, selected_literals)
                        # exit()

                    yield subprog


                    # for rule_id, rule in enumerate(prog):
                    # head, body = rule
                    # rule_var = vo_clause(rule_id)
                    # rule_index[rule] = rule_var
                    # literals.extend(build_rule_literals(rule, rule_var))
                    # literals.append(lt(rule_var, len(prog)))
                    # literals.append(Literal('clause', (len(prog), ), positive = False))

                    # for nogood in nogoods:
                    # model.context.add_nogood(nogood)


    # @profile
    def find_subprogs(self, literal_index, encoding):
        self.tmp_count +=1
        with open(f'DBG/explain-{self.tmp_count}.pl', 'w') as f:
            f.write(encoding)
        # solver = clingo.Control(["--heuristic=Domain"])
        solver = clingo.Control([])
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])

        with solver.solve(yield_=True) as handle:
            for model in handle:
                atoms = model.symbols(shown = True)
                yield [atom.arguments[0].number for atom in atoms], model
                # x1 = parse_model3(literal_index, atoms)
                # selected_literals =
                # x1 = parse_model4(literal_index, atoms)
                # yield x1, model

# from itertools import chain, combinations
# def powerset(iterable):
#     "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
#     s = list(iterable)
#     return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def parse_model4(literal_index, selected_literals):
    rules = {}
    for idx in selected_literals:
        head_literal, body_literal = literal_index[idx]
        if head_literal not in rules:
            rules[head_literal] = set()
        rules[head_literal].add(body_literal)
    return tuple((head, frozenset(body)) for head, body in rules.items())