#!/usr/bin/env python3

from popper.util import Settings, Stats, timeout
from popper.asp import ClingoGrounder, ClingoSolver
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program
from popper.core import Literal, Grounding, Clause
import multiprocessing

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
    (tp, fn, tn, fp) = conf_matrix
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

    if settings.debug:
        print('Rules:')
        for rule in rules:
            Constrain.print_constraint(rule)
        print()

    return rules

def print_conf_matrix(tester, conf_matrix):
    (tp, fn, tn, fp) = conf_matrix
    approx_pos = '+' if tp + fn < tester.num_pos else ''
    approx_neg = '+' if tn + fp < tester.num_neg else ''
    print(f'TP: {tp}{approx_pos}, FN: {fn}{approx_pos}, TN: {tn}{approx_neg}, FP: {fp}{approx_neg}')

def popper(settings, stats):
    solver = ClingoSolver(settings)
    tester = Tester(settings)
    grounder = ClingoGrounder()
    constrainer = Constrain()

    num_solutions = 0

    for size in range(1, settings.max_literals + 1):
        if settings.debug:
            print(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')
        solver.update_number_of_literals(size)
        while True:
            # 1. Generate
            with stats.duration('generate'):
                model = solver.get_model()
                if not model:
                    break
                (program, before, min_clause) = generate_program(model)

            stats.total_programs += 1

            # 2. Test
            with stats.duration('test'):
                conf_matrix = tester.test(program)

            if settings.debug:
                print(f'Program {stats.total_programs}:')
                pprint(program)
                print_conf_matrix(tester, conf_matrix)

            outcome = decide_outcome(conf_matrix)
            if outcome == (Outcome.ALL, Outcome.NONE):
                print('SOLUTION:')
                pprint(program)
                num_solutions += 1
                if num_solutions == settings.max_solutions:
                    return

            # 3. Build rules
            with stats.duration('build_rules'):
                rules = build_rules(settings, constrainer, tester, program, before, min_clause, outcome)

            # 4. Ground rules
            with stats.duration('ground'):
                rules = ground_rules(grounder, solver.max_clauses, solver.max_vars, rules)

            # 5. Add rules to solver
            with stats.duration('add'):
                solver.add_ground_clauses(rules)

    print('NO MORE SOLUTIONS')
    return

if __name__ == '__main__':
    settings = Settings()
    stats = Stats()
    timeout(popper, (settings, stats), timeout_duration=int(settings.timeout))
    if settings.stats:
        stats.show()