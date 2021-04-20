from popper.log import Experiment
from popper.aspsolver import Clingo
from popper.cpsolver import CPSolver
from popper.tester import Tester
from popper.asptester import ASPTester
from popper.constrain import Constrain, Outcome
from popper.generate import generate_program
from popper.core import Clause, Literal

def ground_constraints(max_clauses, max_vars, constraints):
    for constraint in constraints:
        # find all bindings for the variables in the constraint
        assignments = CPSolver.ground_program(constraint, max_clauses, max_vars)
        # assignments = Clingo.ground_program(constraint, max_clauses, max_vars)

        # build the clause object
        clause = Clause(constraint.head, tuple(lit for lit in constraint.body if isinstance(lit, Literal)))

        # For each variable assignment, ground the clause
        for assignment in assignments:
            yield clause.ground(assignment)

def pprint(program):
    if program:
        for clause in program.to_code():
            print(clause)

# @profile
def popper(experiment):
    with experiment.duration('basic setup'):
        solver = Clingo(experiment)
        tester = Tester(experiment)
        #tester = ASPTester(experiment)
        constrain = Constrain(experiment)

    for size in range(1, experiment.args.max_literals + 1):
        if experiment.args.debug:
            print(f'MAX LITERALS: {size}')

        solver.update_number_of_literals(size)
        iteration = 1
        
        while True:
            if experiment.args.debug:
                print(f'Iteration: {iteration}')
            experiment.total_programs += 1
            
            # 1. Generate
            with experiment.duration('generate'):
                program = generate_program(solver)
            if program == None:
                if experiment.args.debug: 
                    print(f'EXITS iteration {"*" * 50}\n\n')
                break
            program.to_ordered()

            # 2. Test
            with experiment.duration('test'):
                program_outcomes = tester.test(program)
            if experiment.args.debug:
                print('Outcomes ->')
                print('  Program:')
                for clause in program.to_code():
                    print('  ', clause)
                print('  Outcome: ', program_outcomes[program])
            if program_outcomes[program] == (Outcome.ALL, Outcome.NONE):
                if experiment.args.debug:
                    print()
                if experiment.args.stats:
                    experiment.stats(True)
                pprint(program)
                return

            # 3. Build constraints
            with experiment.duration('build contraints'):
                constraints = list(constrain.build_constraints(program_outcomes))
            
            if experiment.args.debug:
                print('Constraints ->')
                for index, constraint in enumerate(constraints, 1):
                    print(f'  {index}. {constraint}')
                print()

            # 4. Ground constraints
            with experiment.duration('ground constraints'):
                constraints = list(ground_constraints(solver.max_clauses, solver.max_vars, constraints))

            # 5. Add to the solver
            with experiment.duration('ground clauses'):
                solver.add_ground_clauses(constraints)
            
            iteration += 1

    if experiment.args.stats:
        experiment.stats(False)
    else:
        print('No program returned.')

if __name__ == '__main__':
    experiment = Experiment()
    popper(experiment)