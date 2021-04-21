from popper.log import Experiment
from popper.aspsolver import Clingo
# from popper.cpsolver import CPSolver
from popper.tester import Tester
from popper.constrain import Constrain, Outcome
from popper.generate import generate_program
from popper.core import Clause, Literal

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

# @profile
def popper(experiment):
    solver = Clingo(experiment)
    tester = Tester(experiment)
    grounder = solver
    constrainer = Constrain(experiment)

    iteration = 1
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
            # AC: TODO ORDER BY DEFAULT
            program.to_ordered()

            # 2. Test
            with experiment.duration('test'):
                program_outcomes = tester.test(program)
            if experiment.args.debug:
                print(f'Program {iteration}:')
                for clause in program.to_code():
                    print(clause)
                # AC: TODO - replace with confusion matrix
                print('Outcome: ', program_outcomes[program])
            if program_outcomes[program] == (Outcome.ALL, Outcome.NONE):
                if experiment.args.debug:
                    print()
                if experiment.args.stats:
                    experiment.stats(True)
                pprint(program)
                return

            # 3. Build constraints
            constraints = list(constrainer.build_constraints(program_outcomes))

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

            iteration += 1

    if experiment.args.stats:
        experiment.stats(False)
    else:
        print('No program returned.')

if __name__ == '__main__':
    experiment = Experiment()
    popper(experiment)