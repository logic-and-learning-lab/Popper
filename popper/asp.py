import os
import re
import sys
import clingo
import operator
import numbers
import pkg_resources
from . core import Grounding, ConstVar
from collections import OrderedDict
from clingo import Function, Number, Tuple_
import clingo.script
clingo.script.enable_python()

def arg_to_symbol(arg):
    if isinstance(arg, tuple):
        return Tuple_(tuple(arg_to_symbol(a) for a in arg))
    if isinstance(arg, numbers.Number):
        return Number(arg)
    if isinstance(arg, str):
        return Function(arg)
    assert False, f'Unhandled argtype({type(arg)}) in aspsolver.py arg_to_symbol()'

def atom_to_symbol(pred, args):
    xs = tuple(arg_to_symbol(arg) for arg in args)
    return Function(name = pred, arguments = xs)

class ClingoGrounder():
    def __init__(self):
        self.seen_assignments = {}

    def find_bindings(self, clause, max_clauses, max_vars):
        (_, body) = clause
        all_vars = Grounding.find_all_vars(body)
        if len(all_vars) == 0:
            return [{}]

        k = Grounding.grounding_hash(body, all_vars)
        if k in self.seen_assignments:
            return self.seen_assignments[k]

        # map each clause_var and var_var in the program to an integer
        c_vars = {v:i for i,v in enumerate(var for var in all_vars if var.type == 'Clause')}
        v_vars = {v:i for i,v in enumerate(var for var in all_vars if var.type == 'Variable')}

        # transpose for return lookup
        c_vars_ = {v:k for k,v in c_vars.items()}
        v_vars_ = {v:k for k,v in v_vars.items()}

        c_var_count = len(c_vars)
        v_var_count = len(v_vars)
        if c_var_count == 0 and v_var_count == 0:
            return [{}]

        solver = clingo.Control()

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

        # add constraints to the ASP program based on the AST thing
        for lit in body:
            if not lit.meta:
                continue
            if lit.predicate == '==':
                var, val = lit.arguments
                var = v_vars[var]
                solver.add('base', [], f':- not v_var({var},{val}).')
            elif lit.predicate == '>=':
                var, val = lit.arguments
                var = c_vars[var]
                for i in range(val):
                    solver.add('base', [], f':- c_var({var},{i}).')
            elif lit.predicate == '<':
                var1 = c_vars[lit.arguments[0]]
                var2 = c_vars[lit.arguments[1]]
                solver.add('base', [], f':- c_var({var1},Val1), c_var({var2},Val2), Val1>=Val2.')

        solver.ground([("base", [])])

        out = []

        def on_model(m):
            xs = m.symbols(shown = True)
            # map a variable to a program variable
            assignment = {}
            for x in xs:
                var = x.arguments[0].number
                val = x.arguments[1].number
                if x.name == 'c_var':
                    assignment[c_vars_[var]] = val
                elif x.name == 'v_var':
                    assignment[v_vars_[var]] = val
            out.append(assignment)

        solver.solve(on_model=on_model)
        self.seen_assignments[k] = out
        return out

class ClingoSolver():

    @staticmethod
    def load_alan(settings, ctrl):
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        ctrl.add('alan', [], alan)
        with open(settings.bias_file) as f:
            ctrl.add('bias', [], f.read())
        ctrl.ground([('alan', []), ('bias', [])])

    @staticmethod
    def get_hspace(settings, formatting):
        solver = clingo.Control(settings.clingo_args)
        ClingoSolver.load_alan(settings, solver)
        solver.configuration.solve.models = settings.hspace
        num_models=0
        def on_model(m):
            nonlocal num_models
            num_models+=1
            formatting(num_models, m.symbols(shown = True))
        solver.solve(on_model=on_model)

    def __init__(self, settings):
        self.solver = clingo.Control(settings.clingo_args)
        # AC: why an OrderedDict? We never remove from it
        self.assigned = OrderedDict()
        self.seen_symbols = {}

        ClingoSolver.load_alan(settings, self.solver)

        NUM_OF_LITERALS = (
        """
        %%% External atom for number of literals in the program %%%%%
        #external size_in_literals(n).
        :-
            size_in_literals(n),
            #sum{K+1,Clause : body_size(Clause,K)} != n.
        """)

        self.solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)

        max_vars_atoms = self.solver.symbolic_atoms.by_signature('max_vars', arity=1)
        self.max_vars = next(max_vars_atoms).symbol.arguments[0].number
        max_clauses_atoms = self.solver.symbolic_atoms.by_signature('max_clauses', arity=1)
        self.max_clauses = next(max_clauses_atoms).symbol.arguments[0].number

    def get_model(self):
        with self.solver.solve(yield_ = True) as handle:
            m = handle.model()
            if m:
                return m.symbols(shown = True)
            return m

    def update_number_of_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom[0] == 'size_in_literals' and truth_value:
                self.assigned[atom] = False
                symbol = clingo.Function('size_in_literals', [clingo.Number(atom[1])])
                self.solver.release_external(symbol)

        # 2. Ground the new size
        self.solver.ground([('number_of_literals', [clingo.Number(size)])])

        # 3. Assign the new size
        self.assigned[('size_in_literals', size)] = True

        # @NOTE: Everything passed to Clingo must be Symbol. Refactor after
        # Clingo updates their cffi API
        symbol = clingo.Function('size_in_literals', [clingo.Number(size)])
        self.solver.assign_external(symbol, True)

    def gen_symbol(self, literal, backend):
        (sign, pred, args) = literal
        k = hash(literal)
        if k in self.seen_symbols:
            symbol = self.seen_symbols[k]
        else:
            symbol = backend.add_atom(atom_to_symbol(pred, args))
            self.seen_symbols[k] = symbol
        return symbol

    def add_ground_clauses(self, clauses):
        with self.solver.backend() as backend:
            for (head, body) in clauses:
                head_literal = []
                if head:
                    head_literal = [self.gen_symbol(head, backend)]
                body_lits = []
                for literal in body:
                    (sign, _pred, _args) = literal
                    symbol = self.gen_symbol(literal, backend)
                    body_lits.append(symbol if sign else -symbol)
                backend.add_rule(head_literal, body_lits)