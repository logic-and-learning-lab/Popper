# from ortools.sat.python import cp_model
# from . import core

# class CPSolver():

#     def __init__(self):
#         self.seen_assignments = {}

#     def ground_program(self, constraint, max_clauses, max_vars):
#         if len(constraint.all_vars) == 0:
#             return [{}]

#         k = constraint.myhash()
#         if k in self.seen_assignments:
#             return self.seen_assignments[k]

#         model = cp_model.CpModel()

#         vars_to_cp = {}
#         cp_to_vars = {}

#         var_vals = []
#         for var in constraint.all_vars:
#             if var.type == 'Clause':
#                 cp_var = model.NewIntVar(0, max_clauses - 1, var.name)
#             elif var.type == 'Variable':
#                 cp_var = model.NewIntVar(0, max_vars - 1, var.name)
#             vars_to_cp[var] = cp_var
#             cp_to_vars[cp_var] = var

#         for lit in constraint.body:
#             if not isinstance(lit, core.ConstOpt):
#                 continue
#             if lit.operation == 'AllDifferent':
#                 model.AddAllDifferent([vars_to_cp[x] for x in lit.arguments])
#                 continue
#             args = []
#             for arg in lit.arguments:
#                 if isinstance(arg, core.ConstVar):
#                     args.append(vars_to_cp[arg])
#                 else:
#                     args.append(arg)
#             a, b = args
#             x = lit.operator(a, b)
#             model.Add(x)

#         solution_printer = SolutionPrinter(cp_to_vars)
#         status = cp_model.CpSolver().SearchForAllSolutions(model, solution_printer)
#         assignments = solution_printer.assignments
#         self.seen_assignments[k] = assignments
#         return assignments

# class SolutionPrinter(cp_model.CpSolverSolutionCallback):
#     def __init__(self, cp_to_vars):
#         cp_model.CpSolverSolutionCallback.__init__(self)
#         self.cp_to_vars = cp_to_vars
#         self.assignments = []

#     def on_solution_callback(self):
#         assignment = dict((self.cp_to_vars[k], self.Value(k)) for k in self.cp_to_vars.keys())
#         self.assignments.append(assignment)