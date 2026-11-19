# code written by Andreas Niskanen (andreas.niskanen@helsinki.fi)
import math, subprocess, tempfile
from itertools import chain

from ortools.sat.python import cp_model

from pysat.card import *

ANYTIME_MAXSAT_SOLVER = 'NuWLS-c'
ANYTIME_MAXSAT_SOLVER_PARAMS = ""
ANYTIME_MAXSAT_SOLVER_SIGNAL = 15

def new_wcnf_to_file(hard_clauses, soft_clauses, weights, file):
    for clause in hard_clauses:
        #print("h " + " ".join(map(str, clause)) + " 0")
        file.write("h " + " ".join(map(str, clause)) + " 0" + "\n")
    for clause, w in zip(soft_clauses, weights):
        if w == 0:
            continue
        file.write(str(w) + " " + " ".join(map(str, clause)) + " 0" + "\n")
    file.flush()

def cp_maxsat_solve(hard_clauses, soft_clauses, weights, time_limit=None, upper_bound=None):
    """Solve a weighted MaxSAT instance with CP-SAT.

    Clauses are lists of non-zero ints (v or -v), and each soft clause costs its weight
    when it is false.
    time_limit is in seconds (None means no limit). If upper_bound is given, only models
    whose cost is strictly less than it are allowed.

    Returns (status, cost, model). status is a cp_model status: OPTIMAL, FEASIBLE (the time
    limit stopped the search; the model is valid but maybe not optimal), INFEASIBLE (no model,
    or none below upper_bound) or UNKNOWN (the time limit stopped the search before any model
    was found). model is a list with model[v-1] == v if v is true and -v if it is false, or
    None if there is no model.
    """
    if time_limit is not None and time_limit <= 0:
        return cp_model.UNKNOWN, None, None

    num_vars = max((abs(lit) for clause in chain(hard_clauses, soft_clauses) for lit in clause), default=0)
    m = cp_model.CpModel()
    xs = [None] + [m.NewBoolVar(f'x{i}') for i in range(1, num_vars + 1)]

    def lit(l):
        return xs[l] if l > 0 else xs[-l].Not()

    for clause in hard_clauses:
        m.AddBoolOr([lit(l) for l in clause])

    cost = []
    for clause, w in zip(soft_clauses, weights):
        if w == 0:
            continue
        if len(clause) == 1:
            v = abs(clause[0])
            # [v] costs w when v is false; [-v] costs w when v is true
            cost.append(w - w * xs[v] if clause[0] > 0 else w * xs[v])
        else:
            relax = m.NewBoolVar('')
            m.AddBoolOr([lit(l) for l in clause] + [relax])
            cost.append(w * relax)

    if cost:
        objective = sum(cost)
        if upper_bound is not None and upper_bound != float('inf'):
            m.Add(objective <= math.ceil(upper_bound) - 1)
        m.Minimize(objective)
    elif upper_bound is not None and upper_bound <= 0:
        # every model costs 0, so none is strictly below a bound <= 0
        return cp_model.INFEASIBLE, None, None

    solver = cp_model.CpSolver()
    solver.parameters.num_search_workers = 1
    solver.parameters.linearization_level = 2
    if time_limit is not None:
        solver.parameters.max_time_in_seconds = float(time_limit)

    status = solver.Solve(m)
    if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        return status, None, None

    model = [i if solver.Value(xs[i]) else -i for i in range(1, num_vars + 1)]
    return status, int(round(solver.ObjectiveValue())) if cost else 0, model

def anytime_maxsat_solve(hard_clauses, soft_clauses, weights, timeout):
    with tempfile.NamedTemporaryFile(mode="w", suffix=".wcnf") as tmp:
        new_wcnf_to_file(hard_clauses, soft_clauses, weights, tmp)
        try:
            # output = subprocess.check_output(["timeout", str(timeout), os.path.join(os.path.dirname(__file__), settings.anytime_maxsat_solver)] + settings.anytime_maxsat_solver_params.split() + [tmp.name]).decode("utf-8").split("\n")
            args = ["timeout", "-s", str(ANYTIME_MAXSAT_SOLVER_SIGNAL), str(timeout), ANYTIME_MAXSAT_SOLVER] + ANYTIME_MAXSAT_SOLVER_PARAMS.split() + [tmp.name]
            output = subprocess.check_output(args).decode("utf-8").split("\n")
        except subprocess.CalledProcessError as error:
            output = error.output.decode("utf-8").split("\n")
    if "s UNSATISFIABLE" in output:
        # print('UNSATISFIABLE')
        return float("inf"), None
    elif "s OPTIMUM FOUND" in output or "s SATISFIABLE" in output:
        cost_line = [line for line in output if line.startswith("o ")][-1]
        cost = int(cost_line.replace("o ", "").replace("-oo", "0"))
        model_line = [line for line in output if line.startswith("v ")][-1]
        model_line = model_line.replace("v ", "")
        model = [i if model_line[i-1] == "1" else -i for i in range(1, len(model_line)+1)]
        return cost, model
    else:
        # print("WARNING: No solution found.")
        return None, None