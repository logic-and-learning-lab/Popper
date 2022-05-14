from collections import defaultdict
from . util import format_rule
import clingo
import clingo.script
from pyswip import Prolog
import time
from . core import Literal

MAX_RULE_SIZE = 8
OPTIMAL = False

clingo.script.enable_python()

prolog = Prolog()
prolog.consult("popper/lp/test.pl")

def rule_has_redundant_literal(rule):
    head, body = rule
    C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
    # print(C)
    has_redundant_literal = len(list(prolog.query(f'redundant_literal({C})'))) > 0
    # self.cached_redundant_literals[k] = has_redundant_literal
    return has_redundant_literal

class Tester:
    def __init__(self, settings):
        self.settings = settings

    def test_rule(self, rule):
        xs = list(test_rules_clingo(self.settings, self.settings.bk, self.settings.pos, self.settings.neg, [rule]))
        _, inconsistent, pos_covered = xs[0]
        # print(pos_covered)
        return inconsistent, pos_covered


def test_rules_clingo(settings, bk, pos, neg, rules):
    result_inconsistent = defaultdict(lambda: False)
    result_covers = defaultdict(set)

    grouped_pos = defaultdict(set)
    grouped_neg = defaultdict(set)

    # with stats.duration('test.a'):
    for ex_task, ex in pos:
        # print(ex_task, ex, 'pos')
        grouped_pos[ex_task].add(ex)
    for ex_task, ex in neg:
        # print(ex_task, ex, 'neg')
        grouped_neg[ex_task].add(ex)

    # with stats.duration('test.b'):
    for task, task_bk in bk.items():
        if task not in grouped_pos:
            continue

        task_pos = grouped_pos[task]
        task_neg = grouped_neg[task]
        if len(task_pos) == 0:
            continue
        # with stats.duration('test.x'):
        res = list(test_rules_clingo_aux(settings, task_bk, task_pos, task_neg, rules))
        for rule, inconsistent, covers in res:

                covers = set((task, ex) for ex in covers)
                result_covers[rule].update(covers)
                result_inconsistent[rule] = inconsistent or result_inconsistent[rule]


    # with stats.duration('test.c'):
    to_check_rules = set()
    for rule in rules:
        if len(result_covers[rule]) != len(pos):
            continue
        if result_inconsistent[rule]:
            continue
        to_check_rules.add(rule)

    for task, task_bk in bk.items():
        if len(to_check_rules) == 0:
            break
        task_neg = grouped_neg[task]
        for rule, inconsistent, covers in test_rules_clingo_aux(settings, task_bk, [], task_neg, to_check_rules):
            result_inconsistent[rule] = inconsistent
            if inconsistent:
                to_check_rules.remove(rule)

    for rule in rules:
        x = 'unknown'
        if rule in result_inconsistent:
            x  = result_inconsistent[rule]
        yield rule, x, result_covers[rule]

TEST_PROG = """
#defined pos_covers/2.
#defined inconsistent/1.
#defined pos/2.
#defined neg/2.
#show pos_covers/2.
#show inconsistent/1.
pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
inconsistent(R):- neg(E,Atom), holds(R,Atom).
"""

# TEST_PROG = """
# #defined pos_covers/2.
# #defined neg_covers/2.
# #defined pos/2.
# #defined neg/2.
# #show ex/1.
# pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
# neg_covers(R,E):- neg(E,Atom), holds(R,Atom).
# """

# returns [(rule, inconsistent[rule], covers[rule]) for rule in rules]
def test_rules_clingo_aux(settings, bk, pos, neg, rules):
    if len(rules) == 0:
        return []

    hash_to_rule = {}
    hash_to_ex = {}

    # with stats.duration('test.build'):
    prog = []
    prog.append(TEST_PROG)

    for ex in pos:
        k = f'"{hash(ex)}"'
        hash_to_ex[k] = ex
        x = f'pos({k},{ex}).'
        # print(x)
        prog.append(x)

    for ex in neg:
        k = f'"{hash(ex)}"'
        hash_to_ex[k] = ex
        x = f'neg({k},{ex}).'
        prog.append(x)

    for rule in rules:
        i = f'"{hash(rule)}"'
        hash_to_rule[i] = rule
        rule = format_rule(rule)
        rule = rule.replace(settings.head_str, f'holds({i},{settings.head_str})')
        prog.append(rule)

    prog = '\n'.join(prog)
    # print('---')
    # print(prog + bk)

    # solver = clingo.Control(["--single-shot"])
    # solver = clingo.Control(["--single-shot", "-t16"])
    # with stats.duration('test.init.a'):
    solver = clingo.Control(["--single-shot"])
    # with stats.duration('test.init.b'):
    solver.add('base1', [], bk)
    # with stats.duration('test.init.c'):
    solver.add('base2', [], prog)


    # VERY SLOW!!!

    # with stats.duration('test.ground.base1'):
    solver.ground([('base1', [])])
    # with stats.duration('test.ground.base2'):
    solver.ground([('base2', [])])

    atoms = []
    # with stats.duration('test.solve'):
    with solver.solve(yield_=True) as handle:
        for m in handle:
            atoms.extend(m.symbols(shown = True))

    # with stats.duration('test.parse'):
        inconsistent = {rule:False for rule in rules}
        covers = {rule:set() for rule in rules}

        for atom in atoms:
            if atom.name == 'pos_covers':
                rule_hash = str(atom.arguments[0])
                example_hash = str(atom.arguments[1])
                rule = hash_to_rule[rule_hash]
                example = hash_to_ex[example_hash]
                covers[rule].add(example)
            elif atom.name == 'inconsistent':
                rule_hash = str(atom.arguments[0])
                rule = hash_to_rule[rule_hash]
                inconsistent[rule] = True

    return [(rule, inconsistent[rule], covers[rule]) for rule in rules]

# def test_coverage(settings, stats, bk, pos, rules):
#     grouped_pos = defaultdict(set)

#     for task, ex in pos:
#         grouped_pos[task].add(ex)

#     out = defaultdict(set)
#     for task, task_bk in bk.items():
#         if task not in grouped_pos:
#             continue
#         task_pos = grouped_pos[task]
#         if len(task_pos) == 0:
#             continue
#         # with stats.duration('test_coverage_aux'):
#         res = test_coverage_aux(settings, stats, task_bk, task_pos, rules)
#         for rule, covers in res.items():
#             covers = set((task, ex) for ex in covers)
#             out[rule].update(covers)
#     return out

# def test_coverage_aux(settings, stats, bk, pos, rules):
#     assert(len(rules) > 0)

#     # with stats.duration('test.build'):
#     prog = []
#     prog.append("#show covers/2.")
#     prog.append("covers(R,E):- example(E,Atom), holds(R,Atom).")
#     hash_to_rule = {}
#     hash_to_ex = {}

#     for ex in pos:
#         k = f'"{hash(ex)}"'
#         hash_to_ex[k] = ex
#         prog.append(f'example({k},{ex}).')

#     for rule in rules:
#         i = f'"{hash(rule)}"'
#         hash_to_rule[i] = rule
#         rule = format_rule(rule)
#         rule = rule.replace(settings.head_str, f'holds({i},{settings.head_str})')
#         prog.append(rule)

#     prog = '\n'.join(prog)
#     # print(prog)

#     # solver = clingo.Control(["--single-shot", "-t16"])
#     solver = clingo.Control(["--single-shot"])
#     solver.add('base', [], bk)
#     solver.add('base', [], prog)

#     # with stats.duration('test.ground'):
#     solver.ground([('base', [])])

#     atoms = []
#     # with stats.duration('test.solve'):
#     with solver.solve(yield_=True) as handle:
#         for m in handle:
#             atoms.extend(m.symbols(shown = True))

#     out = defaultdict(set)

#     # with stats.duration('test.parse'):
#     for atom in atoms:
#         # print(atom)
#         rule_hash = str(atom.arguments[0])
#         rule = hash_to_rule[rule_hash]

#         example_hash = str(atom.arguments[1])
#         example = hash_to_ex[example_hash]

#         out[rule].add(example)
#     return out


# from pyswip import Prolog

# import os
# import sys
# import time
# import pkg_resources
# from contextlib import contextmanager
# from . core import Clause, Literal
# from . util import format_program
# from datetime import datetime

# class Tester():
#     def __init__(self, settings):
#         self.settings = settings
#         self.prolog = Prolog()
#         self.prolog.retractall(f'pos_index(_,_)')
#         self.prolog.retractall(f'neg_index(_,_)')

#         self.eval_timeout = settings.eval_timeout
#         self.cached_redundant_literals = {}
#         self.seen_tests = {}
#         self.seen_prog = {}

#         bk_pl_path = self.settings.bk_file
#         exs_pl_path = self.settings.ex_file
#         test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")


#         for x in [exs_pl_path, test_pl_path]:
#             if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
#                 x = x.replace('\\', '\\\\')
#             self.prolog.consult(x)

#         # load examples
#         list(self.prolog.query('load_examples'))

#         # self.pos = [x['I'] for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,_)')]
#         # self.neg = [x['I'] for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,_)')]

#         self.pos = []
#         self.neg = []
#         self.pos_atoms = []
#         self.neg_atoms = []

#         for x in self.prolog.query('current_predicate(pos_index/2),pos_index(I,Atom)'):
#             self.pos.append(x['I'])
#             self.pos_atoms.append(x['Atom'])

#         for x in self.prolog.query('current_predicate(neg_index/2),neg_index(I,Atom)'):
#             self.neg.append(x['I'])
#             self.neg_atoms.append(x['Atom'])

#         self.prolog.assertz(f'timeout({self.eval_timeout})')

#     def first_result(self, q):
#         return list(self.prolog.query(q))[0]

#     @contextmanager
#     def using(self, rules):
#         current_clauses = set()
#         try:
#             for rule in rules:
#                 (head, body) = rule
#                 self.prolog.assertz(Clause.to_code(Clause.to_ordered(rule)))
#                 current_clauses.add((head.predicate, head.arity))
#             yield
#         finally:
#             for predicate, arity in current_clauses:
#                 args = ','.join(['_'] * arity)
#                 self.prolog.retractall(f'{predicate}({args})')

#     # def check_redundant_literal(self, program):
#     #     for clause in program:
#     #         k = Clause.clause_hash(clause)
#     #         if k in self.cached_redundant_literals:
#     #             continue
#     #         self.cached_redundant_literals.add(k)
#     #         (head, body) = clause
#     #         C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
#     #         res = list(self.prolog.query(f'redundant_literal({C})'))
#     #         if res:
#     #             yield clause


#     # def check_redundant_clause(self, program):
#     #     # AC: if the overhead of this call becomes too high, such as when learning programs with lots of clauses, we can improve it by not comparing already compared clauses
#     #     prog = []
#     #     for (head, body) in program:
#     #         C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
#     #         prog.append(C)
#     #     prog = f"[{','.join(prog)}]"
#     #     return list(self.prolog.query(f'redundant_clause({prog})'))

#     def is_non_functional(self, program):
#         try:
#             with self.using(program):
#                 return len(list(self.prolog.query(f'non_functional.'))) > 0
#         except:
#             print('ERROR!!!!!!!!!')
#             print(format_program(program))
#             return True

#     def is_functional(self, program):
#         with self.using(program):
#             return len(list(self.prolog.query(f'functional.'))) > 0

#     def success_set(self, rules):
#         k = hash(frozenset(rules))

#         if k in self.seen_prog:
#             return self.seen_prog[k]

#         if len(rules) == 1 or not all(Clause.is_separable(rule) for rule in rules):
#             with self.using(rules):
#                 xs = set(next(self.prolog.query('success_set(Xs)'))['Xs'])
#                 self.seen_prog[k] = xs
#                 return xs

#         xs = set()
#         for rule in rules:
#             xs.update(self.success_set([rule]))
#         self.seen_prog[k] = xs
#         return xs

#     def pos_covered(self, rules):
#         k = hash(frozenset(rules))

#         if k in self.seen_prog:
#             return self.seen_prog[k]

#         if len(rules) == 1 or not all(Clause.is_separable(rule) for rule in rules):
#             with self.using(rules):
#                 xs = set(next(self.prolog.query('pos_covered(Xs)'))['Xs'])
#                 self.seen_prog[k] = xs
#                 return xs

#         xs = set()
#         for rule in rules:
#             xs.update(self.success_set([rule]))
#         self.seen_prog[k] = xs
#         return xs

#     def is_inconsistent(self, rules):
#         with self.using(rules):
#             return len(list(self.prolog.query('inconsistent'))) > 0

#     def my_test(self, stats, rules):
#         with self.using(rules):
#             with stats.duration('pos_covered'):
#                 coverage = set(next(self.prolog.query('pos_covered(Xs)'))['Xs'])
#             if len(coverage) == 0:
#                 return None, coverage
#             with stats.duration('inconsistent'):
#                 inconsistent = len(list(self.prolog.query('inconsistent'))) > 0
#             return inconsistent, coverage

#     def find_redundant_clauses(self, rules):
#         prog = []
#         for i, (head, body) in enumerate(rules):
#             C = f"[{','.join(('not_'+ Literal.to_code(head),) + tuple(Literal.to_code(lit) for lit in body))}]"
#             C = f'{i}-{C}'
#             prog.append(C)
#         prog = f"[{','.join(prog)}]"
#         res = self.prolog.query(f'find_redundant_clauses({prog},R0,R1)')

#         for dic in res:
#             r0 = dic['R0']
#             r1 = dic['R1']
#             yield rules[r0], rules[r1]

#     def test(self, rules):
#         covered = self.success_set(rules)

#         tp, fn, tn, fp = 0, 0, 0, 0

#         for p in self.pos:
#             if p in covered:
#                 tp +=1
#             else:
#                 fn +=1
#         for n in self.neg:
#             if n in covered:
#                 fp +=1
#             else:
#                 tn +=1

#         return tp, fn, tn, fp

#     def is_complete(self, rules):
#         return all(x in self.success_set(rules) for x in self.pos)

#     def is_consistent(self, rules):
#         return all(x not in self.success_set(rules) for x in self.neg)

#     def is_incomplete(self, rules):
#         return any(x not in self.success_set(rules) for x in self.pos)

#     def is_totally_incomplete(self, rules):
#         return all(x not in self.success_set(rules) for x in self.pos)











# class Tester:

#     def __init__(self, settings, stats, bk, pos, neg):
#         grouped_pos = defaultdict(set)
#         grouped_neg = defaultdict(set)
#         solvers = {}

#         with stats.duration('test.a'):
#             for ex_task, ex in pos:
#                 grouped_pos[ex_task].add(ex)
#             for ex_task, ex in neg:
#                 grouped_neg[ex_task].add(ex)

#         # load solver
#         for task, task_bk in bk.items():
#             prog = [TEST_PROG]
#             prog.append(bk)

#             for ex in grouped_pos[task]:
#                 k = f'"{hash(ex)}"'
#                 hash_to_ex[k] = ex
#                 x = f'pos({k},{ex}).'
#                 prog.append(x)

#             for ex in grouped_neg[task]:
#                 k = f'"{hash(ex)}"'
#                 hash_to_ex[k] = ex
#                 x = f'neg({k},{ex}).'
#                 prog.append(x)

#             prog = '\n'.join(prog)
#             task_solver = clingo.Control()
#             task_solver.add('base', [], prog)
#             task_solver.ground([('base', [])])
#             solvers[task] = task_solver

#         self.solvers = solvers


#     def test_rule(rule):
#         covers = set()
#         for task in self.solvers:
#             solver = self.solvers[task]

#             i = f'"{hash(rule)}"'
#             k = f'prog{i}'
#             rule = format_rule(rule)
#             rule = rule.replace(self.settings.head_str, f'holds({i},{settings.head_str})')

#             solver.add(k, [], rule)
#             solver.ground([(k, [])])


#             with solver.solve(yield_=True) as handle:
#                 for m in handle:
#                     atoms = m.symbols(shown = True)

#                 for atom in atoms:
#                         rule_hash = str(atom.arguments[0])
#                         example_hash = str(atom.arguments[1])
#                         rule = hash_to_rule[rule_hash]
#                         example = hash_to_ex[example_hash]
#                     if atom.name == 'pos_covers':
#                         covers[rule].add(example)
#                     elif atom.name == 'neg_covers':
#                         rule_hash = str(atom.arguments[0])
#                         rule = hash_to_rule[rule_hash]
#                         inconsistent[rule] = True


# # returns [(rule, inconsistent[rule], covers[rule]) for rule in rules]
# def test_rules_clingo_aux(settings, stats, bk, pos, neg, rules):
#     if len(rules) == 0:
#         return []

#     hash_to_rule = {}
#     hash_to_ex = {}

#     with stats.duration('test.build'):
#         prog = []
#         prog.append(TEST_PROG)

#         for ex in pos:
#             k = f'"{hash(ex)}"'
#             hash_to_ex[k] = ex
#             x = f'pos({k},{ex}).'
#             # print(x)
#             prog.append(x)

#         for ex in neg:
#             k = f'"{hash(ex)}"'
#             hash_to_ex[k] = ex
#             x = f'neg({k},{ex}).'
#             prog.append(x)

#         for rule in rules:
#             i = f'"{hash(rule)}"'
#             hash_to_rule[i] = rule
#             rule = format_rule(rule)
#             rule = rule.replace(settings.head_str, f'holds({i},{settings.head_str})')
#             prog.append(rule)

#         prog = '\n'.join(prog)
#         # print('---')
#         # print(prog + bk)

#     # solver = clingo.Control(["--single-shot"])
#     # solver = clingo.Control(["--single-shot", "-t16"])
#     with stats.duration('test.init.a'):
#         solver = clingo.Control(["--single-shot"])
#     with stats.duration('test.init.b'):
#         solver.add('base1', [], bk)
#     with stats.duration('test.init.c'):
#         solver.add('base2', [], prog)


#     # VERY SLOW!!!

#     with stats.duration('test.ground.base1'):
#         solver.ground([('base1', [])])
#     with stats.duration('test.ground.base2'):
#         solver.ground([('base2', [])])

