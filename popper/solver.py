import os
import re
import clingo
import numbers
from . import core
from collections import OrderedDict
from ortools.sat.python import cp_model

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

class SolutionPrinter(cp_model.CpSolverSolutionCallback):

    def __init__(self, cp_to_vars):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.cp_to_vars = cp_to_vars
        self.assignments = []

    def on_solution_callback(self):
        assignment = dict((self.cp_to_vars[k], self.Value(k)) for k in self.cp_to_vars.keys())
        self.assignments.append(assignment)

import popper
def aux(ast, max_clauses, max_vars):
    model = cp_model.CpModel()

    vars_to_cp = {}
    cp_to_vars = {}

    var_vals = []
    for var in ast.all_vars():
        if isinstance(var, core.ClauseVariable):
            cp_var = model.NewIntVar(0, max_clauses - 1, var.name)
        elif isinstance(var, core.VarVariable):
            cp_var = model.NewIntVar(0, max_vars - 1, var.name)
            var_vals.append(cp_var)
        else:
            assert False, 'Whut??' # Jk: Hahahaha
        vars_to_cp[var] = cp_var
        cp_to_vars[cp_var] = var

    # print(vars_to_cp)

    for lit in ast.body:
        if not isinstance(lit, core.ConstraintLiteral):
            continue
        def args():
            for arg in lit.arguments:
                if isinstance(arg, core.Variable):
                    yield vars_to_cp[arg]
                else:
                    yield arg
        x = lit.predicate.operator(*args())
        model.Add(x)


    # print('---')
    # print(model)
    cp_solver = cp_model.CpSolver()
    solution_printer = SolutionPrinter(cp_to_vars)
    cp_solver.SearchForAllSolutions(model, solution_printer)
    xs =  solution_printer.assignments
    # if len(vars_to_cp) == 0:
    #     print('WTF')
    #     print(xs)
    return xs

def aux_clingo(ast, max_clauses, max_vars):
    solver = clingo.Control([])

    # ask for all models
    solver.configuration.solve.models = 0

    # AC:tidy
    path = os.path.abspath('popper/')
    prevwd = os.getcwd()
    with open(path + '/cons.pl') as alan:
        os.chdir(path)
        solver.add('base', [], alan.read())
        os.chdir(prevwd)

    # for each var in the program, map it to an integer
    c_vars = {v.name:i for i,v in enumerate(var for var in ast.all_vars() if isinstance(var, core.ClauseVariable))}
    v_vars = {v.name:i for i,v in enumerate(var for var in ast.all_vars() if isinstance(var, core.VarVariable))}

    # print(c_vars)
    # print(v_vars)

    c_var_count = len(c_vars)

    v_var_count = len(v_vars)
    if c_var_count == 0 and v_var_count == 0:
        return [{}]

    # print(c_var_count, max_clauses)
    # print(v_var_count, max_vars)


    solver.add('base', [], f'#const num_c_vars={c_var_count}.')
    solver.add('base', [], f'#const num_c_vals={max_clauses}.')
    solver.add('base', [], f'#const num_v_vars={v_var_count}.')
    solver.add('base', [], f'#const num_v_vals={max_vars}.')

    for lit in ast.body:
        if not isinstance(lit, core.ConstraintLiteral):
            continue
        if isinstance(lit, popper.core.EQ):
            # lit A==0 <class 'popper.core.EQ'>
            # arg A <class 'popper.core.VarVariable'>
            # arg 0 <class 'int'>
            var = lit.arguments[0]
            val = lit.arguments[1]
            if isinstance(var, popper.core.VarVariable):
                var = v_vars[var.name]
                solver.add('base', [], f':- not v_var({var},{val}).')
        elif isinstance(lit, popper.core.GTEQ):
            # lit Cl>=0 <class 'popper.core.GTEQ'>
            # arg Cl <class 'popper.core.ClauseVariable'>
            # arg 0 <class 'int'>
            var = lit.arguments[0]
            val = lit.arguments[1]
            if isinstance(var, popper.core.ClauseVariable):
                var = c_vars[var.name]
                for i in range(val):
                    solver.add('base', [], f':- not c_var({var},{val}).')
        elif isinstance(lit, popper.core.LT):
            # print('LIT',lit)
            var1 = c_vars[lit.arguments[0].name]
            var2 = c_vars[lit.arguments[1].name]
            solver.add('base', [], f':- c_var({var1},Val1), c_var({var2},Val2), Val1>=Val2.')
            # print(var1,var2)
        # else:
            # print(lit)


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

class Clingo():
    def __init__(self, kbpath):
        self.solver = clingo.Control([])
        self.max_vars = 0
        self.max_clauses = 0

        # Runtime attributes
        # AC: what is this used for?
        self.grounded = []
        self.assigned = OrderedDict()
        # AC: why an OrderedDict? We never remove from it
        self.added = OrderedDict()

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
        self.grounded.extend(parts)

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

    # AC: what is base? If it is important, create separate functions to distinguish the reasoning
    def add(self, code, name = None, arguments = [], base = False):
        if name is None:
            # AC: does the dict maintain its len as a value, or does it recompute it each time?
            name = f'fragment{len(self.added) + 1}'

        if base:
            self.solver.add(name, arguments, code)
        self.added[name] = code

    # AC: rm will (hopefully) soon change the solver back to clingo
    # AC: I find this method to be incomprehensible
    # AC: it should be split into two: ground constraints and then add to the solver
    # AC: it is difficult to reason

    def ground(self, name, context = None):
        ast = self.added[name]
        clbody = tuple(lit for lit in ast.body if not isinstance(lit, core.ConstraintLiteral))
        clause = core.Clause(head = ast.head, body = clbody)

        with self.solver.backend() as backend:

            # cp_assignments = list(aux(ast,self.max_clauses,self.max_vars))
            # clingo_assignments = list(aux_clingo(ast,self.max_clauses,self.max_vars))

            # if len(cp_assignments) != len(clingo_assignments):
            #     print('--')
            #     print('clause',clause)
            #     print('mismatch',len(cp_assignments),len(clingo_assignments))
            #     print(cp_assignments)
            #     print(clingo_assignments)

            #     exit()

            for assignment in aux(ast,self.max_clauses,self.max_vars):
                ground_clause = clause.ground(assignment)

            # for assignment in aux_clingo(ast,self.max_clauses,self.max_vars):
                # ground_clause = clause.ground_new(assignment)

                # print(assignment)
                # print('ground_clause',ground_clause)

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

        self.grounded.extend((name, []))

