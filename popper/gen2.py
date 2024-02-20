import time
import re
from pysat.formula import CNF
from pysat.solvers import Solver
from pysat.card import *
import clingo
import numbers
import clingo.script
import pkg_resources
from . util import rule_is_recursive, Constraint, format_prog, bias_order, Literal
clingo.script.enable_python()
from clingo import Function, Number, Tuple_
from itertools import permutations

def arg_to_symbol(arg):
    if isinstance(arg, tuple):
        return Tuple_(tuple(arg_to_symbol(a) for a in arg))
    if isinstance(arg, numbers.Number):
        return Number(arg)
    if isinstance(arg, str):
        return Function(arg)

def atom_to_symbol(pred, args):
    xs = tuple(arg_to_symbol(arg) for arg in args)
    return Function(name = pred, arguments = xs)

DEFAULT_HEURISTIC = """
#heuristic size(N). [1000-N,true]
"""

class Generator:

    def __init__(self, settings, bkcons=[]):
        self.savings = 0
        self.settings = settings
        self.assigned = {}
        self.cached_clingo_atoms = {}
        self.handle = None
        self.cached4 = {}

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        encoding.append(alan)

        with open(settings.bias_file) as f:
            bias_text = f.read()
        bias_text = re.sub(r'max_vars\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_body\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_clauses\(\d*\).','', bias_text)
        encoding.append(bias_text)
        encoding.append(f'max_clauses({settings.max_rules}).')
        encoding.append(f'max_body({settings.max_body}).')
        encoding.append(f'max_vars({settings.max_vars}).')

        # ADD VARS, DIRECTIONS, AND TYPES
        encoding.append(f'head_vars({settings.head_literal.arity}, {tuple(range(settings.head_literal.arity))}).')
        arities = set(a for p, a in self.settings.body_preds)
        arities.add(settings.head_literal.arity)
        for arity in arities:
            for xs in permutations(range(settings.max_vars), arity):
                encoding.append(f'vars({arity}, {tuple(xs)}).')
                for i, x in enumerate(xs):
                    encoding.append(f'var_pos({x}, {tuple(xs)}, {i}).')

        type_encoding = set()
        types = tuple(self.settings.head_types)
        str_types = str(types).replace("'","")
        for i, x in enumerate(self.settings.head_types):
            type_encoding.add(f'type_pos({str_types}, {i}, {x}).')

        for pred, types in self.settings.body_types.items():
            types = tuple(types)
            str_types = str(types).replace("'","")
            for i, x in enumerate(types):
                type_encoding.add(f'type_pos({str_types}, {i}, {x}).')
        encoding.extend(type_encoding)

        for pred, xs in self.settings.directions.items():
            for i, v in xs.items():
                if v == '+':
                    encoding.append(f'direction_({pred}, {i}, in).')
                if v == '-':
                    encoding.append(f'direction_({pred}, {i}, out).')

        max_size = (1 + settings.max_body) * settings.max_rules
        if settings.max_literals < max_size:
            encoding.append(f'custom_max_size({settings.max_literals}).')

        if settings.pi_enabled:
            encoding.append(f'#show head_literal/4.')

        if settings.noisy:
            encoding.append("""
            program_bounds(0..K):- max_size(K).
            program_size_at_least(M):- size(N), program_bounds(M), M <= N.
            """)

        encoding.extend(bkcons)

        if settings.single_solve:
            encoding.append(DEFAULT_HEURISTIC)

        encoding = '\n'.join(encoding)

        with open('ENCODING-GEN.pl', 'w') as f:
            f.write(encoding)

        if self.settings.single_solve:
            solver = clingo.Control(['--heuristic=Domain','-Wnone'])

        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver

    def get_prog(self):
        if self.handle is None:
            self.handle = iter(self.solver.solve(yield_ = True))
        self.model = next(self.handle, None)
        if self.model is None:
            return None

        atoms = self.model.symbols(shown = True)

        return self.parse_model_single_rule(atoms)

    def parse_model_single_rule(self, model):
        settings = self.settings
        head = settings.head_literal
        body = set()
        cached_literals = settings.cached_literals
        for atom in model:
            args = atom.arguments
            predicate = args[1].name
            atom_args = tuple(args[3].arguments)
            literal = cached_literals[(predicate, atom_args)]
            body.add(literal)
        rule = head, frozenset(body)
        return frozenset([rule])

    def prune_size(self, size):
        size_con = [(atom_to_symbol("size", (size,)), True)]
        self.model.context.add_nogood(size_con)

    def constrain(self, tmp_new_cons):
        model = self.model

        new_ground_cons = set()

        for xs in tmp_new_cons:
            con_type = xs[0]
            con_prog = xs[1]

            if con_type == Constraint.GENERALISATION or con_type == Constraint.BANISH:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                ground_rules2 = self.build_generalisation_constraint3(con_prog, con_size)
                new_ground_cons.update(ground_rules2)
            elif con_type == Constraint.SPECIALISATION:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                ground_rules2 = self.build_specialisation_constraint3(con_prog, con_size)
                new_ground_cons.update(ground_rules2)
            elif con_type == Constraint.UNSAT:
                cons_ = self.unsat_constraint2(con_prog)
                new_ground_cons.update(cons_)

        tmp = model.context.add_nogood

        for ground_body in new_ground_cons:
            nogood = []
            for sign, pred, args in ground_body:
                k = hash((sign, pred, args))
                try:
                    x = self.cached_clingo_atoms[k]
                except KeyError:
                    x = (atom_to_symbol(pred, args), sign)
                    self.cached_clingo_atoms[k] = x
                nogood.append(x)
            tmp(nogood)

    def unsat_constraint2(self, body):
        assignments = self.find_deep_bindings4(body)
        for assignment in assignments:
            rule = []
            for atom in body:
                args2 = tuple(assignment[x] for x in atom.arguments)
                rule.append((True, 'body_literal', (0, atom.predicate, len(atom.arguments), args2)))
            yield frozenset(rule)

    def build_generalisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        for body in self.find_variants3(rule):
            body = list(body)
            body.append((True, 'body_size', (0, len(body))))
            if size:
                body.append((True, 'program_size_at_least', (size,)))
            yield frozenset(body)

    def build_specialisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        if not size:
            yield from self.find_variants3(rule)
            return

        for body in self.find_variants3(rule):
            body = list(body)
            body.append((True, 'program_size_at_least', (size,)))
            yield frozenset(body)

    def find_variants3(self, rule):
        head, body = rule
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head.arity)
        subset = range(head.arity, self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for atom in body:
                new_args = tuple(xs[arg] for arg in atom.arguments)
                new_literal = (True, 'body_literal', (0, atom.predicate, len(new_args), new_args))
                new_body.append(new_literal)
            yield frozenset(new_body)

    def find_deep_bindings4(self, body):
        head_types = self.settings.head_types
        body_types = self.settings.body_types

        var_type_lookup = {}

        head_vars = set()
        if head_types:
            for i, head_type in enumerate(head_types):
                var_type_lookup[i] = head_type
                head_vars.add(i)

        body_vars = set()
        for atom in body:
            pred = atom.predicate
            if pred not in body_types:
                continue
            for i, x in enumerate(atom.arguments):
                body_vars.add(x)
                var_type = body_types[pred][i]
                var_type_lookup[x] = var_type

        if body_vars:
            key = hash(frozenset((k,v) for k,v in var_type_lookup.items() if k in body_vars))
        else:
            assert(False)
            # all_vars = set(x for atom in body for x in atom.arguments)
            # key = hash(frozenset(all_vars))

        if key in self.cached4:
            return self.cached4[key]

        bad_matchings = set()
        for x in body_vars:
            if x not in var_type_lookup:
                continue
            for y in head_vars:
                if x == y:
                    continue
                if y not in var_type_lookup:
                    continue
                if var_type_lookup[x] == var_type_lookup[y]:
                    continue
                k = (x, y)
                bad_matchings.add(k)

        solver_values = tuple(range(self.settings.max_vars))
        var_lookup = {}
        solver_index = {}
        index = 1

        formula = CNF()

        for x in body_vars:
            x_clause = []
            for y in solver_values:
                # match x to y
                k = (x,y)
                if k in bad_matchings:
                    continue
                var_lookup[k] = index
                solver_index[index] = k
                index+=1
                x_clause.append(var_lookup[k])
            formula.append(x_clause)
            for z in CardEnc.equals(lits=x_clause, encoding=EncType.pairwise).clauses:
                formula.append(z)

        for y in solver_values:
            y_clause = []
            for x in body_vars:
                k = (x,y)
                if k in bad_matchings:
                    continue
                y_clause.append(var_lookup[k])
            for z in CardEnc.atmost(lits=y_clause, encoding=EncType.pairwise).clauses:
                formula.append(z)

        solver = Solver(name='m22')
        for x in formula.clauses:
            solver.add_clause(x)

        out = []
        for m in solver.enum_models():
            assignment = {}
            for x in m:
                if x < 0:
                    continue
                x, y = solver_index[x]
                assignment[x] = y
            out.append(assignment)
            # print('moo2', assignment)

        self.cached4[key] = out
        return out