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
    if program:
        for clause in program.to_code():
            print(clause)

def popper(experiment):
    if experiment.args.kbpath[-1] != '/':
        experiment.args.kbpath += '/'
    solver = Clingo(experiment)
    tester = Tester(experiment)
    grounder = solver
    constrainer = Constrain(experiment)

    for size in range(1, experiment.args.max_literals + 1):
        if experiment.args.debug:
            print(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')
        solver.update_number_of_literals(size)
        while True:
            experiment.total_programs += 1

            # 1. Generate
            with experiment.duration('generate'):
                program = generate_program(solver)
            if program == None:
                break

            # 2. Test
            with experiment.duration('test'):
                (outcome, conf_matrix) = tester.test(program)

            if experiment.args.debug:
                print(f'Program {experiment.total_programs}:')
                for clause in program.to_code():
                    print(clause)
                (TP,FN,TN,FP) = conf_matrix
                approx_pos = '+' if TP + FN < tester.num_pos else ''
                approx_neg = '+' if TN + FP < tester.num_neg else ''
                print(f'TP: {TP}{approx_pos}, FN: {FN}{approx_pos}, TN: {TN}{approx_neg}, FP: {FP}{approx_neg}')

            if outcome == (Outcome.ALL, Outcome.NONE):
                if experiment.args.debug:
                    print()
                if experiment.args.stats:
                    experiment.stats(True)
                pprint(program)
                return True

            # 3. Build constraints
            constraints = list(constrainer.build_constraints([(program, outcome)]))

            if experiment.args.debug:
                print('Constraints:')
                for index, constraint in enumerate(constraints, 1):
                    print(f'{index}. {constraint}')
                print()

            # 4. Ground constraints
            with experiment.duration('grounding'):
                constraints = list(ground_constraints(grounder, solver.max_clauses, solver.max_vars, constraints))

            # 5. Add to the solver
            with experiment.duration('adding'):
                solver.add_ground_clauses(constraints)

    if experiment.args.stats:
        experiment.stats(False)
        return True
    else:
        print('No program returned.')
        return True

if __name__ == '__main__':
    experiment = Experiment()
    p = multiprocessing.Process(target = popper, args = (experiment,))
    p.start()
    p.join(experiment.args.timeout)
    
    if p.is_alive():
        p.terminate()
        print('Timedout.')