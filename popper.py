import sys
from popper.aspsolver import Clingo
from popper.cpsolver import CPSolver
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program
from popper import core
from datetime import datetime

# max=11 for filter
# max=6 for finddup
def popper(solver, tester, constrain, max_literals = 100):
    # cnt = 0
    for size in range(1, max_literals + 1):
        print(size)
        solver.update_number_of_literals(size)
        while True:
            # 1. Generate
            program = generate_program(solver)
            if program == None:
                break
            else:
                program.to_ordered()

            # cnt += 1
            # print(cnt)

            # 2. Test
            program_outcomes = tester.test(program)
            if program_outcomes[program] == ('all', 'none'):
                return program

            # 3. Build constraints
            constraints = list(constrain.build_constraints(program_outcomes))

            # 4. Ground constraints and add to the solver
            for ast in constraints:
                # find all bindings for the variables in the constraint
                # assignments = CPSolver.ground_program(ast, solver.max_clauses, solver.max_vars)
                assignments = Clingo.ground_program(ast, solver.max_clauses, solver.max_vars)

                # build the clause object
                clbody = tuple(lit for lit in ast.body if isinstance(lit, core.Literal))
                clause = core.Clause(ast.head, clbody)

                # For each variable assignment, ground the clause
                for assignment in assignments:
                    x = clause.ground(assignment)
                    solver.add_ground_clause(x)

def output_program(program):
    if program:
        for clause in program.to_code():
            print(clause)
    else:
        print(program)

def main(kbpath):
    solver = Clingo(kbpath)
    tester = Tester(kbpath)
    constrain = Constrain()

    program = popper(solver, tester, constrain)
    output_program(program)

if __name__ == '__main__':
    main(sys.argv[1])