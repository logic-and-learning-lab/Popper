#!/usr/bin/env python3

import logging
import sys
from . util import Settings, Stats, timeout, parse_settings, format_program
from . asp import ClingoGrounder, ClingoSolver
from . tester import Tester
from . constrain import Constrain
from . generate import generate_program
from . core import Grounding, Clause

def bind_vars_in_cons(stats, grounder, max_clauses, max_vars, clauses):
    ground_cons = set()
    for clause in clauses:
        head, body = clause

        # find bindings for variables in the constraint
        assignments = grounder.find_bindings(clause, max_clauses, max_vars)

        # keep only standard literals
        body = tuple(literal for literal in body if not literal.meta)

        # ground the clause for each variable assignment
        for assignment in assignments:
            ground_cons.add(Grounding.ground_clause((head, body), assignment))
    
    stats.register_ground_rules(ground_cons)

    return ground_cons

def build_constraints(settings, stats, constrainer, tester, program, before, min_clause):
    cons = set()

    if tester.is_inconsistent(program):
        cons.update(constrainer.generalisation_constraint(program, before, min_clause))

    if tester.is_totally_incomplete(program):
        cons.update(constrainer.redundancy_constraint(program, before, min_clause))

    if tester.is_incomplete(program):
        cons.update(constrainer.specialisation_constraint(program, before, min_clause))

    # eliminate building rules subsumed by this one
    for rule in program:
        cons.update(constrainer.subsumption_constraint(rule, min_clause))

    if settings.functional_test and tester.is_non_functional(program):
        cons.update(constrainer.generalisation_constraint(program, before, min_clause))

    # eliminate generalisations of clauses that contain redundant literals
        for rule in program:
            if tester.rule_has_redundant_literal(rule):
                cons.update(constrainer.redundant_literal_constraint(rule, before, min_clause))

    if len(program) > 1:

        # detect subsumption redundant rules
        for r1, r2 in tester.find_redundant_clauses(program):
            cons.update(constrainer.subsumption_constraint_pairs(r1, r2, min_clause))

        # eliminate inconsistent rules
        if tester.is_inconsistent(program):
            for rule in program:
                if tester.is_inconsistent([rule]):
                    cons.update(constrainer.generalisation_constraint([rule], before, min_clause))

        # eliminate totally incomplete rules
        if all(Clause.is_separable(rule) for rule in program):
            for rule in program:
                if tester.is_totally_incomplete([rule]):
                    cons.update(constrainer.redundancy_constraint([rule], before, min_clause))

    stats.register_rules(cons)

    return cons

PROG_KEY = 'prog'

def calc_score(conf_matrix):
    tp, fn, tn, fp = conf_matrix
    return tp + tn

def popper(settings, stats):
    solver = ClingoSolver(settings)
    tester = Tester(settings)
    settings.num_pos, settings.num_neg = len(tester.pos), len(tester.neg)
    grounder = ClingoGrounder()
    constrainer = Constrain()
    best_score = None

    all_ground_cons = set()
    all_fo_cons = set()

    for size in range(1, settings.max_literals + 1):
        stats.update_num_literals(size)
        solver.update_number_of_literals(size)

        while True:

            # GENERATE HYPOTHESIS
            with stats.duration('generate'):
                model = solver.get_model()
                if not model:
                    break
                program, before, min_clause = generate_program(model)

            # TEST HYPOTHESIS AND UPDATE BEST PROGRAM
            with stats.duration('test'):
                conf_matrix = tester.test(program)
                score = calc_score(conf_matrix)
                stats.register_program(program, conf_matrix)

                if best_score == None or score > best_score:
                    best_score = score

                    if tester.is_complete(program) and tester.is_consistent(program):
                        stats.register_solution(program, conf_matrix)
                        return stats.solution.code

                    stats.register_best_program(program, conf_matrix)

            # BUILD CONSTRAINTS
            with stats.duration('build'):
                cons = build_constraints(settings, stats, constrainer, tester, program, before, min_clause)
                cons = cons - all_fo_cons
                all_fo_cons.update(cons)

            # GROUND CONSTRAINTS
            with stats.duration('ground'):
                ground_cons = bind_vars_in_cons(stats, grounder, solver.max_clauses, solver.max_vars, cons)
                ground_cons = ground_cons - all_ground_cons
                all_ground_cons.update(ground_cons)

            # ADD CONSTRAINTS TO SOLVER
            with stats.duration('add'):
                solver.add_ground_clauses(ground_cons)

    stats.register_completion()
    return stats.best_program.code if stats.best_program else None

def show_hspace(settings):
    f = lambda i, m: print(f'% program {i}\n{format_program(generate_program(m)[0])}')
    ClingoSolver.get_hspace(settings, f)

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
