#!/usr/bin/env python3

from popper.log import Experiment
from popper.aspsolver import Clingo
from popper.tester import Tester
from popper.constrain import Constrain, Outcome
from popper.generate import generate_program
from popper.core import Clause, Literal, Grounding
import multiprocessing

def ground_rules(grounder, max_clauses, max_vars, rules):
    for (head, body) in rules:
        # find bindings for variables in the constraint
        assignments = grounder.find_bindings(head, body, max_clauses, max_vars)

        # keep only standard literals
        body = [literal for literal in body if isinstance(literal, Literal)]

        # ground the clause for each variable assignment
        for assignment in assignments:
            yield Grounding.ground_rule(head, body, assignment)

def pprint(program):
    for clause in program.to_code():
        print(clause)

def popper(experiment):
    if experiment.kbpath[-1] != '/':
        experiment.kbpath += '/'
    solver = Clingo(experiment.kbpath, experiment.clingo_args)
    tester = Tester(experiment)
    grounder = solver
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
                program = generate_program(model)

            experiment.total_programs += 1

            # 2. Test
            with experiment.duration('test'):
                (tp, fn, tn, fp) = tester.test(program)

            if experiment.debug:
                print(f'Program {experiment.total_programs}:')
                pprint(program)
                approx_pos = '+' if tp + fn < tester.num_pos else ''
                approx_neg = '+' if tn + fp < tester.num_neg else ''
                print(f'TP: {tp}{approx_pos}, FN: {fn}{approx_pos}, TN: {tn}{approx_neg}, FP: {fp}{approx_neg}')

            outcome = Constrain.decide_outcome(tp, fn, tn, fp)
            if outcome == (Outcome.ALL, Outcome.NONE):
                print('SOLUTION:')
                pprint(program)
                num_solutions += 1
                if num_solutions == experiment.max_solutions:
                    if experiment.stats:
                        experiment.show_stats(True)
                    return

            # 3. Build constraints
            rules = set()

            # add standard constraints
            rules.update(constrainer.build_constraints(program, outcome))

            if experiment.functional_test and tester.is_non_functional(program):
                rules.update(constrainer.generalisation_constraint(program))

            # eliminate generalisations of clauses that contain redundant literals
            for rule in tester.check_redundant_literal(program):
                rules.update(constrainer.redundant_literal_constraint(rule))

            # eliminate generalisations of programs that contain redundant clauses
            if tester.check_redundant_clause(program):
                rules.update(constrainer.generalisation_constraint(program))

            if experiment.debug:
                print('Rules:')
                for rule in rules:
                    Constrain.print_constraint(rule)
                print()

            # 4. Ground constraints
            with experiment.duration('ground'):
                rules = set(ground_rules(grounder, solver.max_clauses, solver.max_vars, rules))

            # 5. Add to the solver
            with experiment.duration('add'):
                solver.add_ground_clauses(rules)

    print('NO MORE SOLUTIONS')
    if experiment.stats:
        experiment.show_stats(True)
    return

if __name__ == '__main__':
    experiment = Experiment()
    p = multiprocessing.Process(target = popper, args = (experiment,))
    p.start()
    p.join(experiment.args.timeout)

    if p.is_alive():
        p.terminate()
        print('Timed out.')

