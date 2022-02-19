#!/usr/bin/env python3

import logging
import sys
from . util import Settings, Stats, timeout, parse_settings, format_program
from . tester import Tester
from . constrain import Constrain
from collections import defaultdict
from . core import Clause, Literal
import clingo
import clingo.script
clingo.script.enable_python()

def gen_prog(examples, covers, sizes):
    prog = []
    for x in examples:
        prog.append(f'example({x}).')
    for r, xs in covers.items():
        prog.append('{rule(' + str(r) + ')}.')
        for x in xs:
            prog.append(f'covers({r},{x}).')
    for r, size in sizes.items():
        prog.append(f'size({r},{size}).')

    prog = '\n'.join(prog)
    return prog

def gen_args(args):
    return tuple(chr(ord('A') + arg.number) for arg in args)

def parse_model(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    body_atoms = []
    for atom in model:
        pred = atom.arguments[1].name
        args = gen_args(atom.arguments[3].arguments)
        arity = len(args)
        modes = tuple(directions[pred][i] for i in range(arity))
        literal = Literal(pred, args, modes)
        if atom.name == 'body_literal':
            body_atoms.append(literal)
        elif atom.name == 'head_literal':
            head = literal
    return head, frozenset(body_atoms)

# def get_rules(settings, stats, size, cons):
#     with stats.duration('gen.ground.alan'):
#         solver = clingo.Control(["--single-shot"])
#         solver.configuration.solve.models = 0
#         with open('popper/lp/alan.pl') as f:
#             solver.add('alan', [], f.read())
#         with open(settings.bias_file) as f:
#             solver.add('bias', [], f.read())
#         solver.add('bias', [], f':- not body_size(0,{size}).\n')
#         # solver.add('bias', [], '\n'.join(cons))
#         solver.ground([('alan', []), ('bias', [])])

#     with stats.duration('gen.ground.cons'):
#         solver.add('cons', [], '\n'.join(cons))
#         solver.ground([('cons', [])])

#     with stats.duration('gen.solve'):
#         models = []
#         with solver.solve(yield_=True) as handle:
#             for m in handle:
#                 models.append(m.symbols(shown = True))

#     with stats.duration('gen.build'):
#         for model in models:
#             yield parse_model(model)


def get_rules(settings, stats, size, cons, solver):

    with stats.duration('gen.ground.cons'):
        k = f'cons_{size}'
        solver.add(k, [], '\n'.join(cons))
        solver.ground([(k, [])])

    with stats.duration('gen.solve'):
        models = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                models.append(m.symbols(shown = True))

    with stats.duration('gen.build'):
        for model in models:
            yield parse_model(model)

def test_prolog(tester, stats, rules):
    for rule in rules:
        inconsistent, pos_covered = tester.my_test(stats, rule)
        yield rule, inconsistent, pos_covered

def test_rules_clingo(tester, stats, bk, rules):
    if len(rules) == 0:
        return []

    with stats.duration('test.build'):
        prog = """
        #defined pos_covers/2.
        #defined inconsistent/1.
        #show pos_covers/2.
        #show inconsistent/1.

        pos_covers(R,E):-
            pos(E,Atom),
            holds(R,Atom).

        inconsistent(R):-
            neg(E,Atom),
            holds(R,Atom).
        """

        prog += '\n'
        prog += '\n'.join(f'pos({i},{atom}).' for i, atom in zip(tester.pos, tester.pos_atoms))
        prog += '\n'
        prog += '\n'.join(f'neg({i},{atom}).' for i, atom in zip(tester.neg, tester.neg_atoms))
        prog += '\n'

        for i, rule in enumerate(rules):
            rule = format_program(rule)
            # print(rule)
            rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
            rule = rule.replace('f(A)', f'holds({i},f(A))')
            rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
            rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
            rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
            prog += rule + '\n'

    solver = clingo.Control(["--single-shot"])
    solver.add('bk', [], bk)
    solver.add('rules', [], prog)

    # # with open('v1.pl', 'w') as f:
    # #     f.write(bk)
    # #     f.write(prog)

    # print(prog)

    with stats.duration('test.ground.bk'):
        solver.ground([('bk', [])])
    with stats.duration('test.ground.rules'):
        solver.ground([('rules', [])])

    atoms = []
    with stats.duration('test.solve'):
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms.extend(m.symbols(shown = True))

    with stats.duration('test.parse'):
        inconsistent = {i:False for i in range(len(rules))}
        covers = {i:set() for i in range(len(rules))}

        for atom in atoms:
            if atom.name == 'pos_covers':
                i = atom.arguments[0].number
                example = atom.arguments[1].number
                covers[i].add(example)
            elif atom.name == 'inconsistent':
                i = atom.arguments[0].number
                inconsistent[i] = True

    for i, rule in enumerate(rules):
        yield rule, inconsistent[i], covers[i]

def find_subset(prog):
    prog += """
    covered(E):- covers(R,E), rule(R).
    :- example(E), not covered(E).
    #show rule/1.
    size(N):- #sum{K,R : rule(R), size(R,K)} == N.
    #minimize{X : size(X)}.
    """
    with open('sat-prob.pl', 'w') as f:
        f.write(prog)

    solver = clingo.Control(["--single-shot"])
    solver.add('base', [], prog)
    solver.ground([('base', [])])

    out = []
    with solver.solve(yield_=True) as handle:
        for m in handle:
            xs = m.symbols(shown = True)
            out = [atom.arguments[0].number for atom in xs]
    return out

def format_program(rule):
    return Clause.to_code(rule) + '.\n'

def get_solver(stats, settings):
    with stats.duration('gen.ground.alan'):
        # solver = clingo.Control(["--single-shot"])
        solver = clingo.Control()
        solver.configuration.solve.models = 0
        with open('popper/lp/alan.pl') as f:
            solver.add('alan', [], f.read())
        with open(settings.bias_file) as f:
            solver.add('bias', [], f.read())
        # solver.add('bias', [], f':- not body_size(0,{size}).\n')
        # solver.add('bias', [], '\n'.join(cons))
        solver.ground([('alan', []), ('bias', [])])

        NUM_OF_LITERALS = (
        """
        %%% External atom for number of literals in the program %%%%%
        #external size_in_literals(n).
        :-
            size_in_literals(n),
            not body_size(0,n).
        """)

        solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)

    return solver

def update_number_of_literals(solver, tracker, size):
    # 1. Release those that have already been assigned
    for atom, truth_value in tracker.items():
        if atom[0] == 'size_in_literals' and truth_value:
            tracker[atom] = False
            symbol = clingo.Function('size_in_literals', [clingo.Number(atom[1])])
            solver.release_external(symbol)
    solver.ground([('number_of_literals', [clingo.Number(size)])])
    tracker[('size_in_literals', size)] = True
    symbol = clingo.Function('size_in_literals', [clingo.Number(size)])
    solver.assign_external(symbol, True)

def popper(settings, stats):
    tester = Tester(settings)
    with open(settings.bk_file, 'r') as f:
        bk = f.read()
    pos = tester.pos
    neg = tester.neg
    constrainer = Constrain()

    cons = set()
    covers = {}
    sizes = {}
    index = {}

    count = 0
    crap_count = 0
    size = 0
    max_size = 100
    tracker = {}
    best_prog = []

    solver = get_solver(stats, settings)

    while size < max_size:
        size += 1
        body_size = size-1

        update_number_of_literals(solver, tracker, body_size)

        print('--')
        print(f'generating size:{size} with cons:{len(cons)}')
        rules = list(get_rules(settings, stats, body_size, cons, solver))
        # reset cons
        cons = set()
        # rules = list(get_rules(settings, stats, size, []))

        print(f'testing {len(rules)} rules')
        # rules = list(test_prolog(tester, stats, rules))
        # list(test_rules_clingo1(tester, stats, bk, rules))
        rules = list(test_rules_clingo(tester, stats, bk, rules))

        new_rules = False
        for rule, inconsistent, coverage in rules:

            if len(coverage) == 0:
                for con in constrainer.specialisation_constraint([rule], {}, {}):
                    cons.add(constrainer.format_constraint(con))
                continue

            with stats.duration('check_crap'):
                is_crap = False
                for i, xs in covers.items():
                    if coverage.issubset(xs):
                        is_crap = True
                        for con in constrainer.specialisation_constraint([rule], {}, {}):
                            cons.add(constrainer.format_constraint(con))
                if is_crap:
                    continue

            if inconsistent:
                continue

            # if here, then the rule is consistent and covers at least one example
            for con in constrainer.specialisation_constraint([rule], {}, {}):
                cons.add(constrainer.format_constraint(con))

            if tester.rule_has_redundant_literal(rule):
                print('MOOOO3')
                continue

            covers[count] = coverage
            sizes[count] = size + 1
            index[count] = format_program(rule)
            count += 1
            new_rules = True

        if not new_rules:
            continue

        with stats.duration('gen-prob'):
            prog = gen_prog(tester.pos, covers, sizes)

        with stats.duration('subset'):
            print(f'subset problem:{len(index)}')
            xs = list(find_subset(prog))
            if xs != []:
                new_bound = 0
                print('new_prog')
                best_prog = xs
                for i in xs:
                    new_bound += sizes[i]
                    print(index[i].strip())
                max_size = new_bound -1
                print('NEW BOUND', max_size)
                return

def learn_solution(settings):
    stats = Stats(log_best_programs=settings.info)
    log_level = logging.DEBUG if settings.debug else logging.INFO
    logging.basicConfig(level=log_level, stream=sys.stderr, format='%(message)s')
    timeout(popper, (settings, stats), timeout_duration=int(settings.timeout))

    if stats.solution:
        prog_stats = stats.solution
    elif stats.best_programs:
        prog_stats = stats.best_programs[-1]
    else:
        return None, stats

    return prog_stats.code, stats
