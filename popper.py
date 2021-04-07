import sys
from popper.solver import Clingo
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program

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

            # 3. Constrain
            constrain.constrain_solver(solver, program_outcomes)

def direct_popper(solver, tester, constrain, size):
    solver.update_number_of_literals(size)
    c = 1
    while True:
        print(c)
        print('Generate')
        unordered_program = generate_program(solver)
        if unordered_program == None:
            print('No Program Returned')
            break
        ordered_program = unordered_program.to_ordered()    
        
        print('Test')
        # 2. Test
        program_outcomes = tester.test(ordered_program)
        #print(program_outcomes)     
        if program_outcomes[ordered_program] == ('all', 'none'):
            return ordered_program
        
        print('Const')
        constrain.constrain_solver(solver, program_outcomes)

        c += 1
        print()

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

    #program = popper(solver, tester, constrain)
    program = direct_popper(solver, tester, constrain, 7)
    #output_program(program)
    
if __name__ == '__main__':
    main(sys.argv[1])