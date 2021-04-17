import sys
import time
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

# @profile
def popper(solver, tester, constrain, max_literals = 100):
    prog_cnt = 0
    for size in range(1, max_literals + 1):
        print(size)
        solver.update_number_of_literals(size)
        while True:
            prog_cnt += 1
            # 1. Generate
            program = generate_program(solver)
            if program == None:
                break
            program.to_ordered()
            # pprint(program)

            # 2. Test
            program_outcomes = tester.test(program)
            if program_outcomes[program] == (Outcome.ALL, Outcome.NONE):
                print(prog_cnt)
                pprint(program)
                return

            # 3. Build constraints
            constraints = list(constrain.build_constraints(program_outcomes))

            # 4. Ground constraints
            constraints = list(ground_constraints(solver.max_clauses, solver.max_vars, constraints))

            # 5. Add to the solver
            solver.add_ground_clauses(constraints)
    print(prog_cnt)

def pprint(program):
    if program:
        for clause in program.to_code():
            print(clause)

def main(kbpath):
    solver = Clingo(kbpath)
    tester = Tester(kbpath)
    # tester = ASPTester(kbpath)
    constrain = Constrain()
    popper(solver, tester, constrain)

if __name__ == '__main__':
    main(sys.argv[1])