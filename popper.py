import sys
from popper.aspsolver import Clingo
from popper.cpsolver import CPSolver
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program
from popper import core
from datetime import datetime

def popper(solver, tester, constrain, max_literals = 100):
    for size in range(1, max_literals + 1):
        print(size)
        solver.update_number_of_literals(size)
        while True:

            # 1. Generate
            unordered_program = generate_program(solver)
            if unordered_program == None:
                break
            ordered_program = unordered_program.to_ordered()

            # 2. Test
            program_outcomes = tester.test(ordered_program)
            if program_outcomes[ordered_program] == ('all', 'none'):
                return ordered_program

            # 3. Build the constraints
            named_constraints = constrain.build_constraints(program_outcomes)

            # 4. Ground constraints and add to the solver
            for name, ast in named_constraints:
                # find all bindings for the variables in the constraint
                assignments = CPSolver.ground_program(ast, solver.max_clauses, solver.max_vars)
                # assignments = Clingo.ground_program(ast, solver.max_clauses, solver.max_vars)

                # build the clause object
                clbody = tuple(lit for lit in ast.body if not isinstance(lit, core.ConstraintLiteral))
                clause = core.Clause(head = ast.head, body = clbody)

                # For each variable assignment, ground the clause
                for assignment in assignments:
                    # AC: clause.ground is called often. Can we optimise it?
                    x = clause.ground(assignment)
                    solver.add_ground_clause(x)

                # AC: many methods above all loop of the clause :
                # AC: 'core.Clause(head = ast.head, body = clbody)'
                # AC: 'clause.ground'
                # AC: 'solver.add_ground_clause(x)' does
                # AC: can we refactor to only need one pass

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
    # program = direct_popper(solver, tester, constrain, 7)
    output_program(program)

if __name__ == '__main__':
    main(sys.argv[1])