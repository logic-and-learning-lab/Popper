from ortools.sat.python import cp_model
from . import core
class CPSolver():
     def ground_program(ast, max_clauses, max_vars):
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

        for lit in ast.body:
            if not isinstance(lit, core.ConstraintLiteral):
                continue
            def args():
                for arg in lit.arguments:
                    if isinstance(arg, core.Variable):
                        yield vars_to_cp[arg]
                    else:
                        yield arg
            # AC: why they weird syntax? why not push the reasoning in the loop?
            x = lit.predicate.operator(*args())
            model.Add(x)

        cp_solver = cp_model.CpSolver()
        solution_printer = SolutionPrinter(cp_to_vars)
        status = cp_solver.SearchForAllSolutions(model, solution_printer)
        return solution_printer.assignments

class SolutionPrinter(cp_model.CpSolverSolutionCallback):
    def __init__(self, cp_to_vars):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.cp_to_vars = cp_to_vars
        self.assignments = []

    def on_solution_callback(self):
        assignment = dict((self.cp_to_vars[k], self.Value(k)) for k in self.cp_to_vars.keys())
        self.assignments.append(assignment)