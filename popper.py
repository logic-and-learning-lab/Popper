#!/usr/bin/env python3

from popper.util import Settings, Stats, timeout
from popper.asp import ClingoGrounder, ClingoSolver
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program
from popper.core import Literal, Grounding, Clause
import multiprocessing
import threading

class Outcome:
    ALL = 'all'
    SOME = 'some'
    NONE = 'none'

class Con:
    GENERALISATION = 'generalisation'
    SPECIALISATION = 'specialisation'
    REDUNDANCY = 'redundancy'
    BANISH = 'banish'

OUTCOME_TO_CONSTRAINTS = {
        (Outcome.ALL, Outcome.NONE)  : (Con.BANISH,),
        (Outcome.ALL, Outcome.SOME)  : (Con.GENERALISATION,),
        (Outcome.SOME, Outcome.NONE) : (Con.SPECIALISATION,),
        (Outcome.SOME, Outcome.SOME) : (Con.SPECIALISATION, Con.GENERALISATION),
        (Outcome.NONE, Outcome.NONE) : (Con.SPECIALISATION, Con.REDUNDANCY),
        (Outcome.NONE, Outcome.SOME) : (Con.SPECIALISATION, Con.REDUNDANCY, Con.GENERALISATION)
    }

def ground_rules(grounder, max_clauses, max_vars, clauses):
    out = set()
    for clause in clauses:
        (head, body) = clause
        # find bindings for variables in the constraint
        assignments = grounder.find_bindings(clause, max_clauses, max_vars)

        # keep only standard literals
        body = tuple(literal for literal in body if not literal.meta)

        # ground the clause for each variable assignment
        for assignment in assignments:
            out.add(Grounding.ground_clause((head, body), assignment))
    return out

def pprint(program):
    for clause in program:
        print(Clause.to_code(clause))

def decide_outcome(conf_matrix):
    tp, fn, tn, fp = conf_matrix
    if fn == 0:
        positive_outcome = Outcome.ALL # complete
    elif tp == 0 and fn > 0:
        positive_outcome = Outcome.NONE # totally incomplete
    else:
        positive_outcome = Outcome.SOME # incomplete

    if fp == 0:
        negative_outcome = Outcome.NONE  # consistent
    # elif FP == self.num_neg:     # AC: this line may not work with minimal testing
        # negative_outcome = Outcome.ALL # totally inconsistent
    else:
        negative_outcome = Outcome.SOME # inconsistent

    return (positive_outcome, negative_outcome)

def build_rules(settings, constrainer, tester, program, before, min_clause, outcome):
    (positive_outcome, negative_outcome) = outcome
    # RM: If you don't use these two lines you need another three entries in the OUTCOME_TO_CONSTRAINTS table (one for every positive outcome combined with negative outcome ALL).
    if negative_outcome == Outcome.ALL:
         negative_outcome = Outcome.SOME

    rules = set()
    for constraint_type in OUTCOME_TO_CONSTRAINTS[(positive_outcome, negative_outcome)]:
        if constraint_type == Con.GENERALISATION:
            rules.update(constrainer.generalisation_constraint(program, before, min_clause))
        elif constraint_type == Con.SPECIALISATION:
            rules.update(constrainer.specialisation_constraint(program, before, min_clause))
        elif constraint_type == Con.REDUNDANCY:
            rules.update(constrainer.redundancy_constraint(program, before, min_clause))
        elif constraint_type == Con.BANISH:
            rules.update(constrainer.banish_constraint(program, before, min_clause))

    if settings.functional_test and tester.is_non_functional(program):
        rules.update(constrainer.generalisation_constraint(program, before, min_clause))

    # eliminate generalisations of clauses that contain redundant literals
    for rule in tester.check_redundant_literal(program):
        rules.update(constrainer.redundant_literal_constraint(rule, before, min_clause))

    # eliminate generalisations of programs that contain redundant clauses
    if tester.check_redundant_clause(program):
        rules.update(constrainer.generalisation_constraint(program, before, min_clause))

    if len(program) > 1:
        # evaluate inconsistent sub-clauses
        for rule in program:
            if Clause.is_separable(rule) and tester.is_inconsistent(rule):
                for x in constrainer.generalisation_constraint([rule], before, min_clause):
                    rules.add(x)

        # eliminate totally incomplete rules
        with stats.duration('test_individual_rules.is_totally_incomplete'):
            if all(Clause.is_separable(rule) for rule in program):
                for rule in program:
                    if tester.is_totally_incomplete(rule):
                        for x in constrainer.redundancy_constraint([rule], before, min_clause):
                            rules.add(x)

    if settings.debug:
        print('Rules:')
        for rule in rules:
            Constrain.print_constraint(rule)
        print()

    return rules

PROG_KEY = 'prog'

def calc_score(conf_matrix):
    (tp, fn, tn, fp) = conf_matrix
    return tp + tn

def print_conf_matrix(conf_matrix):
    tp, fn, tn, fp = conf_matrix
    precision = 'n/a'
    if (tp+fp) > 0:
        precision = f'{tp / (tp+fp):0.2f}'
    recall = 'n/a'
    if (tp+fn) > 0:
        recall = f'{tp / (tp+fn):0.2f}'
    print(f'Precision:{precision}, Recall:{recall}, TP:{tp}, FN:{fn}, TN:{tn}, FP:{fp}')

def popper(settings, stats, args):
    solver = ClingoSolver(settings)
    tester = Tester(settings)
    settings.num_pos, settings.num_neg = len(tester.pos), len(tester.neg)
    grounder = ClingoGrounder()
    constrainer = Constrain()
    best_score = None

    for size in range(1, settings.max_literals + 1):
        if settings.debug:
            print(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')
        solver.update_number_of_literals(size)
        while True:
            # GENERATE HYPOTHESIS
            with stats.duration('generate'):
                model = solver.get_model()
                if not model:
                    break
                (program, before, min_clause) = generate_program(model)

            if settings.debug:
                print(f'Program {stats.total_programs}:')
                pprint(program)

            stats.total_programs +=1

            # TEST HYPOTHESIS
            with stats.duration('test'):
                conf_matrix = tester.test(program)
                outcome = decide_outcome(conf_matrix)
                score = calc_score(conf_matrix)

            # UPDATE BEST PROGRAM
            if best_score == None or score > best_score:
                best_score = score
                args[PROG_KEY] = (program, conf_matrix)

                if outcome == (Outcome.ALL, Outcome.NONE):
                    return

                if settings.info:
                    print(f'NEW BEST PROG {stats.total_programs}:')
                    pprint(program)
                    print_conf_matrix(conf_matrix)
                    print('')

            # BUILD RULES
            with stats.duration('build_rules'):
                rules = build_rules(settings, constrainer, tester, program, before, min_clause, outcome)

            # GROUND RULES
            with stats.duration('ground'):
                rules = ground_rules(grounder, solver.max_clauses, solver.max_vars, rules)

            # UPDATE SOLVER
            with stats.duration('add'):
                solver.add_ground_clauses(rules)

    print('NO MORE SOLUTIONS')
    return

if __name__ == '__main__':
    settings = Settings()
    stats = Stats()
    args = {}
    timeout(popper, (settings, stats, args), timeout_duration=int(settings.timeout))

    if PROG_KEY in args:
        print(f'BEST PROG {stats.total_programs}:')
        (program, conf_matrix) = args[PROG_KEY]
        pprint(program)
        print_conf_matrix(conf_matrix)
        print('')
    if settings.stats:
        stats.show()
