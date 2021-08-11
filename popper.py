#!/usr/bin/env python3

from popper.log import Experiment
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

def ground_rules(grounder, max_clauses, max_vars, clauses):
    for clause in clauses:
        (head, body) = clause
        # find bindings for variables in the constraint
        assignments = grounder.find_bindings(clause, max_clauses, max_vars)

        # keep only standard literals
        body = tuple(literal for literal in body if isinstance(literal, Literal))

        # ground the clause for each variable assignment
        for assignment in assignments:
            yield Grounding.ground_clause((head, body), assignment)

def pprint(program):
    for clause in program:
        print(Clause.to_code(clause))

def decide_outcome(tp, fn, tn, fp):
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

def build_rules(experiment, constrainer, tester, program, before, min_clause, outcome):
    OUTCOME_TO_CONSTRAINTS = {
        (Outcome.ALL, Outcome.NONE)  : (Con.BANISH,),
        (Outcome.ALL, Outcome.SOME)  : (Con.GENERALISATION,),
        (Outcome.SOME, Outcome.NONE) : (Con.SPECIALISATION,),
        (Outcome.SOME, Outcome.SOME) : (Con.SPECIALISATION, Con.GENERALISATION),
        (Outcome.NONE, Outcome.NONE) : (Con.SPECIALISATION, Con.REDUNDANCY),
        (Outcome.NONE, Outcome.SOME) : (Con.SPECIALISATION, Con.REDUNDANCY, Con.GENERALISATION)
    }

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

    if experiment.functional_test and tester.is_non_functional(program):
        rules.update(constrainer.generalisation_constraint(program, before, min_clause))

    # eliminate generalisations of clauses that contain redundant literals
    for rule in tester.check_redundant_literal(program):
        rules.update(constrainer.redundant_literal_constraint(rule, before, min_clause))

    # eliminate generalisations of programs that contain redundant clauses
    if tester.check_redundant_clause(program):
        rules.update(constrainer.generalisation_constraint(program, before, min_clause))

    if experiment.debug:
        print('Rules:')
        for rule in rules:
            Constrain.print_constraint(rule)
        print()

    return rules

def show_conf_matrix(tester, tp, fn, tn, fp):
    approx_pos = '+' if tp + fn < tester.num_pos else ''
    approx_neg = '+' if tn + fp < tester.num_neg else ''
    print(f'TP: {tp}{approx_pos}, FN: {fn}{approx_pos}, TN: {tn}{approx_neg}, FP: {fp}{approx_neg}')

def popper(experiment):
    if experiment.kbpath[-1] != '/':
        experiment.kbpath += '/'
    solver = ClingoSolver(experiment.kbpath, experiment.clingo_args)
    tester = Tester(experiment)
    grounder = ClingoGrounder()
    constrainer = Constrain(experiment)
    num_solutions = 0

    for size in range(1, experiment.args.max_literals + 1):
        if experiment.debug:
            print(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')
        solver.update_number_of_literals(size)
        while True:
            # 1. Generate
            with experiment.duration('generate'):
                model = solver.get_model()
                if not model:
                    break
                (program, before, min_clause) = generate_program(model)

            experiment.total_programs += 1

            # 2. Test
            with experiment.duration('test'):
                (tp, fn, tn, fp) = tester.test(program)

            if experiment.debug:
                print(f'Program {experiment.total_programs}:')
                pprint(program)
                show_conf_matrix(tester, tp, fn, tn, fp)

            outcome = decide_outcome(tp, fn, tn, fp)
            if outcome == (Outcome.ALL, Outcome.NONE):
                print('SOLUTION:')
                pprint(program)
                num_solutions += 1
                if num_solutions == experiment.max_solutions:
                    if experiment.stats:
                        experiment.show_stats()
                    return

            # 3. Build rules
            rules = build_rules(experiment, constrainer, tester, program, before, min_clause, outcome)

            # 4. Ground rules
            with experiment.duration('ground'):
                rules = set(ground_rules(grounder, solver.max_clauses, solver.max_vars, rules))

            # 5. Add rules to solver
            with experiment.duration('add'):
                solver.add_ground_clauses(rules)

    print('NO MORE SOLUTIONS')
    if experiment.stats:
        experiment.show_stats()
    return

if __name__ == '__main__':
    experiment = Experiment()
    p = multiprocessing.Process(target = popper, args = (experiment,))
    p.start()
    p.join(experiment.args.timeout)

    if p.is_alive():
        p.terminate()
        print('Timed out.')

