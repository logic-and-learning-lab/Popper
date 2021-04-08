import os
import re
import clingo
import numbers
from . import core
from collections import OrderedDict

# AC: rename file to ClingoSolver.

# -----------------------------------------------------------------------------
NUM_OF_LITERALS = (
"""
%%% External atom for number of literals in the program %%%%%
#external size_in_literals(n).
:-
    size_in_literals(n),
    #sum{K+1,Clause : clause_size(Clause,K)} != n.
""")

# NUM_OF_CLAUSES = (
# """
# %%% External atom for number of clauses in the program %%%%%
# #external size_in_clauses(n).
# :-
#     size_in_clauses(n),
#     #sum{K : clause(K)} != n.
# """)

def arg_to_symbol(arg):
    if isinstance(arg, core.Variable):
        assert False, 'Grounding only symbols for now'
    if isinstance(arg, tuple):
        return clingo.Tuple_(tuple(arg_to_symbol(a) for a in arg))
    if isinstance(arg, numbers.Number):
        return clingo.Number(arg)
    if isinstance(arg, str):
        return clingo.Function(arg)
    assert False, 'Unhandled type'

def atom_to_symbol(lit):
    args = tuple(arg_to_symbol(arg) for arg in lit.arguments)
    return clingo.Function(name = lit.predicate.name, arguments = args)

class Clingo():
    def __init__(self, kbpath):
        self.solver = clingo.Control([])
        self.max_vars = 0
        self.max_clauses = 0

        # Runtime attributes
        # AC: what is this used for?
        # self.grounded = []
        # AC: why an OrderedDict? We never remove from it
        self.assigned = OrderedDict()
        # self.added = set()

        self.load_basic(kbpath)

    def load_basic(self, kbpath):
        # Load Alan.
        alan_path = os.path.abspath('popper/alan/')
        prevwd = os.getcwd()
        with open(alan_path + '/alan.pl') as alan:
            os.chdir(alan_path)
            self.solver.add('alan', [], alan.read())
            os.chdir(prevwd)

        # Load Mode file
        # AC: refactor this function out.
        with open(kbpath + 'modes.pl') as modefile:
            contents = modefile.read()
            self.max_vars = int(re.search("max_vars\((\d+)\)\.", contents).group(1))
            self.max_clauses = int(re.search("max_clauses\((\d+)\)\.", contents).group(1))
            self.solver.add('modes_file', [], contents)

        # Reset number of literals and clauses because size_in_literals literal
        # within Clingo is reset by loading Alan? (bottom two).
        self.solver.add('invented', ['predicate', 'arity'], '#external invented(pred,arity).')
        self.solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)
        # self.solver.add('number_of_clauses', ['n'], NUM_OF_CLAUSES)

        # Ground 'alan' and 'modes_file'
        parts = [('alan', []), ('modes_file', [])]
        self.solver.ground(parts)
        # self.grounded.extend(parts)

    def get_model(self):
        with self.solver.solve(yield_ = True) as handle:
            m = handle.model()
            if m:
                return m.symbols(atoms = True)
            return m

    def update_number_of_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom.predicate.name == 'size_in_literals' and truth_value:
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

    def add_ground_clause(self, ground_clause):
        with self.solver.backend() as backend:
            head_symbs = []
            head_atoms = []
            body_symbs = []
            body_lits = []
            for lit in ground_clause.head:
                symbol = atom_to_symbol(lit)
                head_symbs.append(symbol)
                head_atoms.append(backend.add_atom(symbol))
            for lit in ground_clause.body:
                symbol = atom_to_symbol(lit)
                body_symbs.append(symbol)
                body_atom = backend.add_atom(symbol)
                body_lits.append(body_atom if lit.polarity else -body_atom)

            backend.add_rule(head_atoms, body_lits, choice = False)



    def ground_program(ast, max_clauses, max_vars):

        # map each clause var in the program to an integer
        c_vars = {v:i for i,v in enumerate(var for var in ast.all_vars() if isinstance(var, core.ClauseVariable))}
        # map each var var in the program to an integer
        v_vars = {v:i for i,v in enumerate(var for var in ast.all_vars() if isinstance(var, core.VarVariable))}

        c_var_count = len(c_vars)
        v_var_count = len(v_vars)
        if c_var_count == 0 and v_var_count == 0:
            return [{}]

        solver = clingo.Control(['--seed=1','--rand-freq=0'])
        # ask for all models
        solver.configuration.solve.models = 0

        # add the base reasoning
        solver.add('base', [], """\
            #show v_var/2.
            #show c_var/2.
            c_val(0..num_c_vals-1).
            v_val(0..num_v_vals-1).
            1 {c_var(V,X): c_val(X)} 1:- V=0..num_c_vars-1.
            1 {v_var(V,X): v_val(X)} 1:- V=0..num_v_vars-1.
            :- c_val(X), #count{I : c_var(I,X)} > 1.
            :- v_val(X), #count{I : v_var(I,X)} > 1."""
            +
            f"""\
            #const num_c_vars={c_var_count}.
            #const num_c_vals={max_clauses}.
            #const num_v_vars={v_var_count}.
            #const num_v_vals={max_vars}.
        """)

        for lit in ast.body:
            if not isinstance(lit, core.ConstraintLiteral):
                continue
            if isinstance(lit, core.EQ):
                # lit A==0 <class 'popper.core.EQ'>
                # arg A <class 'popper.core.VarVariable'>
                # arg 0 <class 'int'>
                var = lit.arguments[0]
                val = lit.arguments[1]
                if isinstance(var, core.VarVariable):
                    var = v_vars[var]
                    solver.add('base', [], f':- not v_var({var},{val}).')
            elif isinstance(lit, core.GTEQ):
                # lit Cl>=0 <class 'popper.core.GTEQ'>
                # arg Cl <class 'popper.core.ClauseVariable'>
                # arg 0 <class 'int'>
                var = lit.arguments[0]
                val = lit.arguments[1]
                if isinstance(var, core.ClauseVariable):
                    var = c_vars[var]
                    for i in range(val):
                        solver.add('base', [], f':- c_var({var},{i}).')
            elif isinstance(lit, core.LT):
                var1 = c_vars[lit.arguments[0]]
                var2 = c_vars[lit.arguments[1]]
                solver.add('base', [], f':- c_var({var1},Val1), c_var({var2},Val2), Val1>=Val2.')

        solver.ground([("base", [])])

        out = []
        def on_model(m):
            xs = m.symbols(shown = True)
            c_var_assignments = {}
            v_var_assignments = {}
            for x in xs:
                var = x.arguments[0].number
                val = x.arguments[1].number
                if x.name == 'c_var':
                    c_var_assignments[var] = val
                if x.name == 'v_var':
                    v_var_assignments[var] = val
            assignments = {}
            for k, v in c_vars.items():
                assignments[k] = c_var_assignments[v]
            for k, v in v_vars.items():
                assignments[k] = v_var_assignments[v]
            out.append(assignments)

        solver.solve(on_model=on_model)
        return out