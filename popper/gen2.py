import time
import re
import clingo
import numbers
import clingo.script
import pkg_resources
from . util import Constraint, Literal
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

NOISY_ENCODING = """
program_bounds(0..K):- max_size(K).
program_size_at_least(M):- size(N), program_bounds(M), M <= N.
"""

class Generator:

    def __init__(self, settings, bkcons=[]):
        self.savings = 0
        self.settings = settings
        self.cached_clingo_atoms = {}
        self.handle = None
        self.pruned_sizes = set()

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        encoding.append(alan)

        with open(settings.bias_file) as f:
            bias_text = f.read()
        bias_text = re.sub(r'max_vars\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_body\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_clauses\(\d*\).','', bias_text)

        for p,a in settings.pointless:
            bias_text = re.sub(rf'body_pred\({p},{a}\).','', bias_text)
            bias_text = re.sub(rf'constant\({p},.*?\).*', '', bias_text, flags=re.MULTILINE)

        encoding.append(bias_text)
        encoding.append(f'max_clauses({settings.max_rules}).')
        encoding.append(f'max_body({settings.max_body}).')
        encoding.append(f'max_vars({settings.max_vars}).')

        # ADD VARS, DIRECTIONS, AND TYPES
        head_arity = len(settings.head_literal.arguments)
        encoding.append(f'head_vars({head_arity}, {tuple(range(head_arity))}).')
        arities = set(a for p, a in self.settings.body_preds)
        arities.add(head_arity)
        for arity in arities:
            for xs in permutations(range(settings.max_vars), arity):
                encoding.append(f'vars({arity}, {tuple(xs)}).')
                for i, x in enumerate(xs):
                    encoding.append(f'var_pos({x}, {tuple(xs)}, {i}).')

        type_encoding = set()
        if self.settings.head_types:
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

        if settings.noisy:
            encoding.append(NOISY_ENCODING)

        encoding.extend(bkcons)

        if settings.single_solve:
            encoding.append(DEFAULT_HEURISTIC)

        encoding = '\n'.join(encoding)

        # with open('ENCODING-GEN.pl', 'w') as f:
            # f.write(encoding)

        if self.settings.single_solve:
            solver = clingo.Control(['--heuristic=Domain','-Wnone'])

        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver

    def update_solver(self, size):
        # not used when learning programs without pi or recursion
        pass

    def get_prog(self):
        if self.handle is None:
            self.handle = iter(self.solver.solve(yield_ = True))
        self.model = next(self.handle, None)
        if self.model is None:
            return None

        return self.parse_model_single_rule(self.model.symbols(shown = True))

    def parse_model_single_rule(self, model):
        settings = self.settings
        head = settings.head_literal
        body = set()
        cached_literals = settings.cached_literals
        for atom in model:
            args = atom.arguments
            body.add(cached_literals[args[1].name, tuple(args[3].arguments)])
        rule = head, frozenset(body)
        return frozenset({rule})

    def prune_size(self, size):
        if size in self.pruned_sizes:
            return
        self.pruned_sizes.add(size)
        size_con = [(atom_to_symbol("size", (size,)), True)]
        self.model.context.add_nogood(size_con)

    def constrain(self, tmp_new_cons):
        new_ground_cons = set()

        for xs in tmp_new_cons:
            con_type = xs[0]
            con_prog = xs[1]

            if con_type == Constraint.GENERALISATION or con_type == Constraint.BANISH:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                ground_rules2 = tuple(self.build_generalisation_constraint3(con_prog, con_size))
                new_ground_cons.update(ground_rules2)
            elif con_type == Constraint.SPECIALISATION:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                ground_rules2 = tuple(self.build_specialisation_constraint3(con_prog, con_size))
                new_ground_cons.update(ground_rules2)
            elif con_type == Constraint.UNSAT:
                cons_ = self.unsat_constraint2(con_prog)
                new_ground_cons.update(cons_)

        tmp = self.model.context.add_nogood
        cached_clingo_atoms = self.cached_clingo_atoms

        for ground_body in new_ground_cons:
            nogood = []
            for sign, pred, args in ground_body:
                k = hash((sign, pred, args))
                try:
                    x = cached_clingo_atoms[k]
                except KeyError:
                    x = (atom_to_symbol(pred, args), sign)
                    cached_clingo_atoms[k] = x
                nogood.append(x)
            tmp(nogood)

    def unsat_constraint2(self, body):
        # if no types, remap variables
        if len(self.settings.body_types) == 0:
            _, body = remap_variables((None, body))

        assignments = self.find_deep_bindings4(body)
        for assignment in assignments:
            rule = []
            for pred, args in body:
                args2 = tuple(assignment[x] for x in args)
                rule.append((True, 'body_literal', (0, pred, len(args), args2)))

            yield frozenset(rule)

    def build_generalisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        for body in self.find_variants(rule, max_rule_vars=True):
            body = list(body)
            body.append((True, 'body_size', (0, len(body))))
            if size:
                body.append((True, 'program_size_at_least', (size,)))
            yield frozenset(body)

    def build_specialisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        if not size:
            yield from self.find_variants(rule)
            return

        for body in self.find_variants(rule):
            body = list(body)
            body.append((True, 'program_size_at_least', (size,)))
            yield frozenset(body)

    def find_variants(self, rule, max_rule_vars=False):
        head, body = rule
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= len(head.arguments))
        if max_rule_vars:
            subset = range(len(head.arguments), len(body_vars | set(head.arguments)))
        else:
            subset = range(len(head.arguments), self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (True, 'body_literal', (0, pred, len(new_args), new_args))
                new_body.append(new_literal)
            yield frozenset(new_body)

    def find_deep_bindings4(self, body):
        head_types = self.settings.head_types
        body_types = self.settings.body_types

        # if no types, find all permutations of variables
        if len(body_types) == 0 or head_types is None:
            num_vars = len({var for atom in body for var in atom.arguments})
            for xs in permutations(range(self.settings.max_vars), num_vars):
                x = {i:xs[i] for i in range(num_vars)}
                yield x
            return

        # if there are types, only find type-safe permutations
        var_type_lookup = {i:head_type for i, head_type in enumerate(head_types)}

        head_vars = set(range(len(self.settings.head_literal.arguments)))
        body_vars = set()

        for pred, args in body:
            for i, x in enumerate(args):
                body_vars.add(x)
                if x in head_vars:
                    continue
                if pred in body_types:
                    var_type_lookup[x] = body_types[pred][i]

        # prohibit bad type matchings
        bad_type_matching = set()
        for x in body_vars:
            if x not in var_type_lookup:
                continue
            for y in head_vars:
                if y not in var_type_lookup:
                    continue
                if var_type_lookup[x] == var_type_lookup[y]:
                    continue
                k = (x, y)
                bad_type_matching.add(k)

        lookup = {x:i for i, x in enumerate(body_vars)}

        for xs in permutations(range(self.settings.max_vars), len(lookup)):
            assignment = {}
            bad = False
            for x in body_vars:
                v = xs[lookup[x]]
                if (x, v) in bad_type_matching:
                    bad = True
                    break
                assignment[x] = v
            if bad:
                continue
            yield assignment

def remap_variables(rule):
    head, body = rule
    head_vars = set()

    if head:
        head_vars.extend(head.arguments)

    next_var = len(head_vars)
    lookup = {i:i for i in head_vars}

    new_body = set()
    for pred, args in body:
        new_args = []
        for var in args:
            if var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(pred, tuple(new_args))
        new_body.add(new_atom)

    return head, frozenset(new_body)