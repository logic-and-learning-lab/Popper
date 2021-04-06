import sys
from popper.solver import Clingo
from popper.tester import Tester
from popper.constrain import Constrain
from popper.generate import generate_program

def popper(solver, tester, max_literals = 3):
    for size in range(1, max_literals + 1):
        print('Size: ', size)
        while True:
            # 1. Generate
            unordered_program = generate_program(solver, size)
            if unordered_program == None:
                break
            ordered_program = unordered_program.to_ordered()    
            
            # 2. Test
            program_outcomes = tester.test(ordered_program)     
            if program_outcomes[ordered_program] == ('all', 'none'):
                return ordered_program

            # 3. Constrain

def direct_popper(solver, tester, constrain, size):
    unordered_program = generate_program(solver, size)
    if unordered_program == None:
        print('NO Program Returned')
    ordered_program = unordered_program.to_ordered()    
    
    # 2. Test
    program_outcomes = tester.test(ordered_program)     
    if program_outcomes[ordered_program] == ('all', 'none'):
        return ordered_program
    
    constrain.constrain_solver(solver, program_outcomes)

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
    #program = popper(solver, tester)
    program = direct_popper(solver, tester, constrain, 7)


    #output_program(program)
    
if __name__ == '__main__':
    main(sys.argv[1])