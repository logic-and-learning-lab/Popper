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

def get_rules(settings, stats, size, cons):
    with stats.duration('gen.ground'):
        solver = clingo.Control(["--single-shot"])
        solver.configuration.solve.models = 0
        with open('popper/lp/alan.pl') as f:
            solver.add('alan', [], f.read())
        with open(settings.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], f':- not body_size(0,{size}).\n')
        solver.add('bias', [], '\n'.join(cons))
        solver.ground([('alan', []), ('bias', [])])

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
            rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
            rule = rule.replace('f(A)', f'holds({i},f(A))')
            rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
            rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
            prog += rule + '\n'

    # print(prog)
    solver = clingo.Control(["--single-shot"])
    solver.add('bk', [], bk)
    solver.add('rules', [], prog)

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

# def test_rules_clingo1(tester, stats, bk, rules):
#     if len(rules) == 0:
#         return []

#     with stats.duration('test.build'):
#         prog = """
#         #show pos_covers/2.
#         #show inconsistent/1.

#         pos_covers(R,E):-
#             pos(E,Atom),
#             holds(R,Atom).

#         inconsistent(R):-
#             neg(E,Atom),
#             holds(R,Atom).
#         """

#         prog += '\n'
#         prog += '\n'.join(f'pos({i},{atom}).' for i, atom in zip(tester.pos, tester.pos_atoms))
#         prog += '\n'
#         prog += '\n'.join(f'neg({i},{atom}).' for i, atom in zip(tester.neg, tester.neg_atoms))
#         prog += '\n'

#         for i, rule in enumerate(rules):
#             print(rule)
#             rule = format_program(rule)
#             print(rule)
#             exit()
#             # rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
#             # rule = rule.replace('f(A)', f'holds({i},f(A))')
#             # rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
#             # :- neg(E,Atom), holds(R,Atom).
#             h = f'inconsistent({i})'
#             Literal.to_code(head)
#             b = 'neg(E,next_score(A,B,C))'
#             # )

#             # rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
#             prog += rule + '\n'

#     solver = clingo.Control(["--single-shot"])
#     solver.add('bk', [], bk)
#     solver.add('rules', [], prog)

    # with stats.duration('test.ground.bk'):
    #     solver.ground([('bk', [])])
    # with stats.duration('test.ground.rules'):
    #     solver.ground([('rules', [])])

    # atoms = []
    # with stats.duration('test.solve'):
    #     with solver.solve(yield_=True) as handle:
    #         for m in handle:
    #             atoms.extend(m.symbols(shown = True))

    # with stats.duration('test.parse'):
    #     inconsistent = {i:False for i in range(len(rules))}
    #     covers = {i:set() for i in range(len(rules))}

    #     for atom in atoms:
    #         if atom.name == 'pos_covers':
    #             i = atom.arguments[0].number
    #             example = atom.arguments[1].number
    #             covers[i].add(example)
    #         elif atom.name == 'inconsistent':
    #             i = atom.arguments[0].number
    #             inconsistent[i] = True


    # for i, rule in enumerate(rules):
    #     yield rule, inconsistent[i], covers[i]

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

    def on_model(m):
        xs = m.symbols(shown = True)
        print(xs)

    solver.solve(on_model=on_model)

# def format_program(program):
#     return "\n".join(Clause.to_code(Clause.to_ordered(clause)) + '.' for clause in program)
def format_program(rule):
    return Clause.to_code(rule) + '.\n'

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
    for size in [2,3,4,5,6]:
        print('--')
        print(f'size:{size}')

        print('generating')
        rules = list(get_rules(settings, stats, size, cons))
        # rules = list(get_rules(settings, stats, size, []))

        print(f'testing {len(rules)} rules')
        # rules = list(test_prolog(tester, stats, rules))
        rules = list(test_rules_clingo(tester, stats, bk, rules))

        new_rules = False
        for rule, inconsistent, coverage in rules:

            if len(coverage) == 0:
                for con in constrainer.specialisation_constraint([rule], {}, {}):
                    cons.add(constrainer.format_constraint(con))
                continue

            if inconsistent:
                continue

            # if here, then the rule is consistent and covers at least one example
            for con in constrainer.specialisation_constraint([rule], {}, {}):
                cons.add(constrainer.format_constraint(con))

            with stats.duration('check_crap'):
                is_crap = False
                for i, xs in covers.items():
                    if coverage.issubset(xs):
                        is_crap = True
                        crap_count +=1
                        break
                if is_crap:
                    continue

            if tester.rule_has_redundant_literal(rule):
                print('MOOOO3')
                continue

            covers[count] = coverage
            sizes[count] = size
            index[count] = format_program(rule)
            count += 1
            new_rules = True

        if not new_rules:
            continue

        with stats.duration('gen-prob'):
            prog = gen_prog(tester.pos, covers, sizes)

        with stats.duration('subset'):
            print(f'subset problem:{len(index)}')
            print(f'crap_count', crap_count)
            find_subset(prog)

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
