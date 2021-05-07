#!/usr/bin/env python3

from popper.log import Experiment
from popper.aspsolver import Clingo
from popper.tester import Tester
from popper.constrain import Constrain, Outcome
from popper.generate import generate_program
from popper.core import Clause, Literal
import multiprocessing

def ground_constraints(grounder, max_clauses, max_vars, constraints):
    for constraint in constraints:
        # find bindings for variables in the constraint
        assignments = grounder.ground_program(constraint, max_clauses, max_vars)

        # build the clause
        clause = Clause(constraint.head, tuple(lit for lit in constraint.body if isinstance(lit, Literal)))

        # ground the clause for each variable assignment
        for assignment in assignments:
            yield clause.ground(assignment)

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
                (outcome, (TP,FN,TN,FP)) = tester.test(program)

            if experiment.debug:
                print(f'Program {experiment.total_programs}:')
                pprint(program)
                approx_pos = '+' if TP + FN < tester.num_pos else ''
                approx_neg = '+' if TN + FP < tester.num_neg else ''
                print(f'TP: {TP}{approx_pos}, FN: {FN}{approx_pos}, TN: {TN}{approx_neg}, FP: {FP}{approx_neg}')

            if outcome == (Outcome.ALL, Outcome.NONE):
                if experiment.debug:
                    print()
                if experiment.stats:
                    experiment.show_stats(True)
                print('SOLUTION:')
                pprint(program)
                return

            # 3. Build constraints
            cons = set()

            if experiment.functional_test and tester.is_non_functional(program):
                cons.update(constrainer.generalisation_constraint(program))

            # eliminate generalisations of clauses that contain redundant literals
            for clause in tester.check_redundant_literal(program):
                cons.update(constrainer.redundant_literal_constraint(clause))

            # eliminate generalisations of programs that contain redundant clauses
            if tester.check_redundant_clause(program):
                cons.update(constrainer.generalisation_constraint(program))

            # add other constraints
            cons.update(constrainer.build_constraints(program, outcome))

            if experiment.debug:
                print('Constraints:')
                for constraint in cons:
                    print(constraint.ctype, constraint)
                print()

            # 4. Ground constraints
            with experiment.duration('ground'):
                cons = set(ground_constraints(grounder, solver.max_clauses, solver.max_vars, cons))

            # 5. Add to the solver
            with experiment.duration('add'):
                solver.add_ground_clauses(cons)

    if experiment.stats:
        experiment.show_stats(False)
    print('NO SOLUTION')
    return

if __name__ == '__main__':
    experiment = Experiment()
    p = multiprocessing.Process(target = popper, args = (experiment,))
    p.start()
    p.join(experiment.args.timeout)

    if p.is_alive():
        p.terminate()
        print('Timedout.')