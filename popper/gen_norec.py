import re
import clingo
from importlib import resources
from . util import Constraint, Literal
from itertools import permutations
from . import stats

def _unpack_symbol(sym):
    if sym.type == clingo.SymbolType.Number:
        return sym.number
    if sym.type == clingo.SymbolType.String:
        return sym.string
    if sym.type == clingo.SymbolType.Function:
        if len(sym.arguments) == 0:
            return sym.name
        if sym.name == "":
            return tuple(_unpack_symbol(a) for a in sym.arguments)
        return (sym.name, tuple(_unpack_symbol(a) for a in sym.arguments))
    return str(sym)

def _remap_body_variables(body):
    lookup = {}
    next_var = 0
    new_body = set()
    for pred, args in body:
        new_args = []
        for var in args:
            if var not in lookup:
                lookup[var] = next_var
                next_var += 1
            new_args.append(lookup[var])
        new_body.add(Literal(pred, tuple(new_args)))
    return frozenset(new_body)

class Generator:

    def __init__(self, settings, _state, bkcons=None):
        self.settings = settings
        self.cached_clingo_atoms = {}
        self.pruned_sizes = set()

        encoding = self._build_encoding(bkcons or [])
        solver = clingo.Control(['--heuristic=Domain', '-Wnone'])
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver

        self._build_caches()

    def _build_encoding(self, bkcons):
        encoding = []
        alan = resources.files(__package__).joinpath("lp/alan.pl").read_text()
        encoding.append(alan)

        with open(self.settings.bias_file) as f:
            bias_text = f.read()
        bias_text = re.sub(r'max_(?:vars|body|clauses)\(\d*\)\.', '', bias_text)

        for p, a in self.settings.pointless:
            bias_text = re.sub(rf'body_pred\({p},\s*{a}\)\.', '', bias_text)
            bias_text = re.sub(rf'constant\({p},.*?\).*', '', bias_text, flags=re.MULTILINE)

        encoding.append(bias_text)
        encoding.append(f'max_clauses({self.settings.max_rules}).')
        encoding.append(f'max_body({self.settings.max_body}).')
        encoding.append(f'max_vars({self.settings.max_vars}).')

        encoding.extend(self._build_symmetry_breaking_constraints())

        if self.settings.noisy:
            NOISY_ENCODING = """
            program_bounds(0..K):- max_size(K).
            program_size_at_least(M):- size(N), program_bounds(M), M <= N.
            """
            encoding.append(NOISY_ENCODING)

        encoding.extend(bkcons)
        DEFAULT_HEURISTIC = "#heuristic size(N). [1000-N,true]"
        encoding.append(DEFAULT_HEURISTIC)

        return '\n'.join(encoding)

    def _build_symmetry_breaking_constraints(self):
        encoding = []
        head_arity = len(self.settings.head_literal.arguments)
        encoding.append(f'head_vars({head_arity}, {tuple(range(head_arity))}).')
        arities = set(a for p, a in self.settings.body_preds)
        arities.add(head_arity)
        for arity in arities:
            for xs in permutations(range(self.settings.max_vars), arity):
                encoding.append(f'vars({arity}, {tuple(xs)}).')
                for i, x in enumerate(xs):
                    encoding.append(f'var_pos({x}, {tuple(xs)}, {i}).')
                encoding.append(f'ordered_vars({tuple(xs)},{tuple(sorted(xs))}).')

        encoding.extend(self._build_ordering_constraints(arities))

        if self.settings.head_types:
            type_encoding = set()
            head_str_types = str(tuple(self.settings.head_types)).replace("'", "")
            for i, x in enumerate(self.settings.head_types):
                type_encoding.add(f'type_pos({head_str_types}, {i}, {x}).')
            for pred, pred_types in self.settings.body_types.items():
                str_types = str(tuple(pred_types)).replace("'", "")
                for i, x in enumerate(pred_types):
                    type_encoding.add(f'type_pos({str_types}, {i}, {x}).')
            encoding.extend(type_encoding)

        for pred, xs in self.settings.directions.items():
            for i, v in xs.items():
                if v == '+':
                    encoding.append(f'direction_({pred}, {i}, in).')
                if v == '-':
                    encoding.append(f'direction_({pred}, {i}, out).')
        return encoding

    def _build_ordering_constraints(self, arities):
        order_cons = []
        max_arity = max(arities)

        for arity in range(2, max_arity + 1):
            xs1 = ','.join(f'V{i}' for i in range(arity))
            xs2 = ','.join(f'X{i}' for i in range(arity))

            if arity < max_arity:
                prefix = ','.join('0' for _ in range(arity, max_arity)) + ',' + xs1
            else:
                prefix = xs1

            order_cons.append(f'appears(({prefix})):- body_literal(_,_,_,({xs2})), ordered_vars(({xs2}), ({xs1})).')
            order_cons.append(f'var_tuple(({prefix})):- body_pred(P,{arity}), vars({arity},Vars), not bad_body(P,Vars), not type_mismatch(P,Vars), ordered_vars(Vars,OrderedVars), OrderedVars=({xs1}).')

            order_cons.append(f'var_member(V,({prefix})):-vars(_, Vars), Vars=({xs1}), var_member(V,Vars).')

        xs1 = ','.join(f'V{i}' for i in range(max_arity))
        for k in range(max_arity):
            xs2 = ','.join(f'V{i}' for i in range(k))
            if k > 0 and k < max_arity:
                xs2 += ','
            xs2 += ','.join(f'X{i}' for i in range(k, max_arity))
            order_cons.append(f'lower(({xs1}),({xs2})):- var_tuple(({xs1})), var_tuple(({xs2})), X{k} < V{k}.')

        for k in range(max_arity - 1):
            v0 = f'V{k}'
            v1 = f'V{k+1}'
            order_cons.append(f'seen_lower(Vars1, V):- V={v1}-1, Vars1 = ({xs1}), {v0} < V < {v1}, lower(Vars1, Vars2), var_tuple(Vars1), appears(Vars2), var_member(V, Vars2), not head_var(_,V).')
            order_cons.append(f'gap_(({xs1}),{v1}-1):- var_tuple(({xs1})), {v0} < V < {v1}, var(V).')

        order_cons.append(f'gap(({xs1}),V):- gap_(({xs1}), _), #max' + '{X :gap_((' + xs1 + '), X)} == V.')
        order_cons.append(f':- appears(({xs1})), gap(({xs1}), V), not seen_lower(({xs1}),V), not head_var(_,V).')

        return order_cons

    def _build_caches(self):
        self.symbol_to_literal = {}
        for sym_atom in self.solver.symbolic_atoms:
            sym = sym_atom.symbol
            args = tuple(_unpack_symbol(a) for a in sym.arguments)
            pos_id = sym_atom.literal
            self.cached_clingo_atoms[(True, sym.name, args)] = pos_id
            self.cached_clingo_atoms[(False, sym.name, args)] = -pos_id
            if sym.name == "body_literal" and len(sym.arguments) == 4:
                raw_args = sym.arguments
                predicate = raw_args[1].name
                atom_args = tuple(raw_args[3].arguments)
                self.symbol_to_literal[sym] = self.settings.cached_literals[predicate, atom_args]

    def get_prog(self):
        handle = iter(self.solver.solve(yield_=True))
        head = self.settings.head_literal
        gen_timer = stats.duration('generate')
        symbol_to_literal = self.symbol_to_literal

        while True:
            with gen_timer:
                model = next(handle, None)
                if model is None:
                    return
                self.model = model
                rule = head, frozenset([symbol_to_literal[atom] for atom in model.symbols(shown=True)])
            yield frozenset((rule,))

    def prune_size(self, size):
        if size in self.pruned_sizes:
            return
        self.pruned_sizes.add(size)
        atom = self.cached_clingo_atoms.get((True, "size", (size,)))
        if atom:
            self.model.context.add_nogood([atom])

    def constrain(self, cons):
        add_nogood = self.model.context.add_nogood
        for xs in cons:
            con_type = xs[0]
            con_prog = xs[1]
            con_size = xs[2] if len(xs) > 2 else None
            match con_type:
                case Constraint.SPECIALISATION:
                    ground_cons = self.build_specialisation_constraint(con_prog, con_size)
                case Constraint.UNSAT:
                    ground_cons = self.unsat_constraint(con_prog)
                case Constraint.BANISH | Constraint.GENERALISATION:
                    ground_cons = self.build_generalisation_constraint(con_prog, con_size)
            for ground_body in ground_cons:
                add_nogood(ground_body)

    def unsat_constraint(self, body):
        if len(self.settings.body_types) == 0:
            body = _remap_body_variables(body)
        cache = self.cached_clingo_atoms
        for assignment in self.find_deep_bindings(body):
            rule = []
            for pred, args in body:
                args2 = tuple(assignment[x] for x in args)
                clingo_literal = cache.get((True, 'body_literal', (0, pred, len(args), args2)))
                if clingo_literal is None:
                    break
                rule.append(clingo_literal)
            else:
                yield rule

    def build_specialisation_constraint(self, prog, size=None):
        rule = next(iter(prog))
        if not size:
            yield from self.find_variants(rule)
            return
        size_literal = self.cached_clingo_atoms[(True, 'program_size_at_least', (size,))]
        for body in self.find_variants(rule):
            yield body + [size_literal]

    def build_generalisation_constraint(self, prog, size=None):
        rule = next(iter(prog))
        for body in self.find_variants(rule, max_rule_vars=True):
            body.append(self.cached_clingo_atoms[(True, 'body_size', (0, len(body)))])
            if size:
                body.append(self.cached_clingo_atoms[(True, 'program_size_at_least', (size,))])
            yield body

    def find_variants(self, rule, max_rule_vars=False):
        head, body = rule
        body_vars = {x for literal in body for x in literal.arguments if x >= len(head.arguments)}
        if max_rule_vars:
            var_range = range(len(head.arguments), len(body_vars | set(head.arguments)))
        else:
            var_range = range(len(head.arguments), self.settings.max_vars)
        cache_get = self.cached_clingo_atoms.get
        body_list = [(lit.predicate, lit.arguments, len(lit.arguments)) for lit in body]
        for xs in permutations(var_range, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for pred, args, arity in body_list:
                new_args = tuple(xs[arg] for arg in args)
                clingo_literal = cache_get((True, 'body_literal', (0, pred, arity, new_args)))
                if clingo_literal is None:
                    break
                new_body.append(clingo_literal)
            else:
                yield new_body

    def find_deep_bindings(self, body):
        head_types = self.settings.head_types
        body_types = self.settings.body_types

        if len(body_types) == 0 or head_types is None:
            num_vars = len({var for atom in body for var in atom.arguments})
            for xs in permutations(range(self.settings.max_vars), num_vars):
                yield {i: xs[i] for i in range(num_vars)}
            return

        var_type_lookup = {i: t for i, t in enumerate(head_types)}
        head_vars = set(range(len(self.settings.head_literal.arguments)))
        body_vars = set()

        for pred, args in body:
            for i, x in enumerate(args):
                body_vars.add(x)
                if x not in head_vars and pred in body_types:
                    var_type_lookup[x] = body_types[pred][i]

        bad_type_matching = {
            (x, y)
            for x in body_vars if x in var_type_lookup
            for y in head_vars if y in var_type_lookup
            if var_type_lookup[x] != var_type_lookup[y]
        }

        lookup = {x: i for i, x in enumerate(body_vars)}
        for xs in permutations(range(self.settings.max_vars), len(lookup)):
            assignment = {}
            for x in body_vars:
                v = xs[lookup[x]]
                if (x, v) in bad_type_matching:
                    break
                assignment[x] = v
            else:
                yield assignment
