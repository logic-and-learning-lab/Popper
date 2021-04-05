import os
import re
import clingo
from . import core
from collections import OrderedDict

# -----------------------------------------------------------------------------
NUM_OF_LITERALS = (
"""
%%% External atom for number of literals in the program %%%%%
#external size_in_literals(n).
:-
    size_in_literals(n),
    #sum{K+1,Clause : clause_size(Clause,K)} != n.
""")

NUM_OF_CLAUSES = (
"""
%%% External atom for number of clauses in the program %%%%%
#external size_in_clauses(n).
:-
    size_in_clauses(n),
    #sum{K : clause(K)} != n.
""")

class Clingo():
    def __init__(self, kbpath):
        self.solver = clingo.Control([])
        self.load_basic(kbpath)

        # Runtime attributes
        self.grounded = []
        self.assigned = OrderedDict()

    def load_basic(self, kbpath):
        # Load Alan.
        alan_path = os.path.abspath('popper/alan/')
        prevwd = os.getcwd()
        with open(alan_path + '/alan.pl') as alan:
            os.chdir(alan_path)
            self.solver.add('alan', [], alan.read())
            os.chdir(prevwd)

        # Load Mode file
        with open(kbpath + 'modes.pl') as modefile:
            self.solver.add('modes_file', [], modefile.read())

        # Reset number of literals and clauses because size_in_literals literal
        # within Clingo is reset by loading Alan? (bottom two).
        self.solver.add('invented', ['predicate', 'arity'], 
                        '#external invented(pred,arity).')
        self.solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)
        self.solver.add('number_of_clauses', ['n'], NUM_OF_CLAUSES)

        # Ground 'alan' and 'modes_file'
        self.solver.ground([('alan', []), ('modes_file', [])])
    
    def get_model(self):
        with self.solver.solve(yield_ = True) as handle:
            m = handle.model()
            if m:
                return m.symbols(atoms = True)
            return m
    
    def update_number_of_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truthiness in self.assigned.items():
            if atom.predicate.name == 'size_in_literals' and truthiness:
                self.assigned[atom] = False
                atom_args = [clingo.Number(arg) for arg in atom.arguments]
                symbol = clingo.Function('size_in_literals', atom_args)
                self.solver.release_external(symbol)
        
        # 2. Ground the new size
        self.solver.ground([('number_of_literals', [clingo.Number(size)])])
        
        # 3. Assign the new size
        atom = core.Atom('size_in_literals', (size,))
        self.assigned[atom] = True

        # @NOTE: Everything passed to Clingo must be Symbol. Refactor after 
        # Clingo updates their cffi API
        symbol = clingo.Function('size_in_literals', [clingo.Number(size)])
        self.solver.assign_external(symbol, True)