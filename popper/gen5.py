# @AC: I think it would be more efficient if the solver just returns an int for each possible body literal
import time
import re
import clingo
import numbers
import clingo.script
from importlib import resources
from . util import Constraint, Literal
from clingo import Function, Number, Tuple_
from itertools import permutations
from . import stats

DEFAULT_HEURISTIC = """
#heuristic size(N). [1000-N,true]
"""

NOISY_ENCODING = """
program_bounds(0..K):- max_size(K).
program_size_at_least(M):- size(N), program_bounds(M), M <= N.
"""

class Generator:

    def __init__(self, settings, state, bkcons=[]):
        self.state = state
        self.settings = settings
        self.cached_clingo_atoms = {}
        self.pruned_sizes = set()

        encoding = []
        alan = resources.files(__package__).joinpath("lp/alan.pl").read_text()
        encoding.append(alan)

        with open(settings.bias_file) as f:
            bias_text = f.read()
        bias_text = re.sub(r'max_vars\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_body\(\d*\).','', bias_text)
        bias_text = re.sub(r'max_clauses\(\d*\).','', bias_text)

        for p,a in settings.pointless:
            bias_text = re.sub(rf'body_pred\({p},\s*{a}\)\.', '', bias_text)
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
                encoding.append(f'ordered_vars({tuple(xs)},{tuple(sorted(xs))}).')



        # ORDERING THINGY
        # %% appears((0,0,V0)):- body_literal(_, _, _, (V0,)), not head_var(_,V0).
        # appears((0,V0,V1)):- body_literal(_, _, _, (A,B)), ordered_vars((A,B), (V0,V1)).
        # appears((V0,V1,V2)):- body_literal(_, _, _, (A,B,C)), ordered_vars((A,B,C), (V0,V1,V2)).
        order_cons = []
        max_arity = max(arities)
        for arity in range(2, max_arity+1):
            xs1 = ','.join(f'V{i}' for i in range(arity)) # Vs
            xs2 = ','.join(f'X{i}' for i in range(arity)) # Xs

            if arity < max_arity:
                prefix = ','.join(str(0) for i in range(arity, max_arity)) + ',' + xs1
            else:
                prefix = xs1


            order_cons.append(f'appears(({prefix})):- body_literal(_,_,_,({xs2})), ordered_vars(({xs2}), ({xs1})).')

            order_cons.append(f'var_tuple(({prefix})):- body_pred(P,{arity}), vars({arity},Vars), not bad_body(P,Vars), not type_mismatch(P,Vars), ordered_vars(Vars,OrderedVars), OrderedVars=({xs1}).')


            if arity == 1:
                order_cons.append(f'var_member(V,(0,0,0,V)):-var(V).')
            else:
                order_cons.append(f'var_member(V,({prefix})):-vars(_, Vars), Vars=({xs1}), var_member(V,Vars).')
            # print(f)
            # var_member(V,(0,0,V0,V1)):-vars(_, Vars), Vars=(V0,V1), var_member(V,Vars).
            # var_member(V,(0,V0,V1,V2)):-vars(_, Vars), Vars=(V0,V1,V2), var_member(V,Vars).

        xs1 = ','.join(f'V{i}' for i in range(max_arity)) # Vs
        for k in range(max_arity):
            xs2 = ','.join(f'V{i}' for i in range(k)) # Vs
            if k > 0 and k < max_arity:
                xs2 += ','
            xs2 += ','.join(f'X{i}' for i in range(k, max_arity))
            order_cons.append(f'lower(({xs1}),({xs2})):- var_tuple(({xs1})), var_tuple(({xs2})), X{k} < V{k}.')

        for k in range(max_arity-1):
            # A,B,C,D
            v0 = f'V{k}'
            v1 = f'V{k+1}'
            order_cons.append(f'seen_lower(Vars1, V):- V={v1}-1, Vars1 = ({xs1}), {v0} < V < {v1}, lower(Vars1, Vars2), var_tuple(Vars1), appears(Vars2), var_member(V, Vars2), not head_var(_,V).')
            order_cons.append(f'gap_(({xs1}),{v1}-1):- var_tuple(({xs1})), {v0} < V < {v1}, var(V).')


        order_cons.append(f'gap(({xs1}),V):- gap_(({xs1}), _), #max' + '{X :gap_((' + xs1 + '), X)} == V.')

        order_cons.append(f':- appears(({xs1})), gap(({xs1}), V), not seen_lower(({xs1}),V), not head_var(_,V).')

        # print('\n'.join(order_cons))
        encoding.extend(order_cons)


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
        # if state.max_literals < max_size:
            # encoding.append(f'custom_max_size({state.max_literals}).')

        if settings.noisy:
            encoding.append(NOISY_ENCODING)

        encoding.extend(bkcons)

        if settings.single_solve:
            encoding.append(DEFAULT_HEURISTIC)

        encoding = '\n'.join(encoding)

        # with open('ENCODING-GEN.pl', 'w') as f:
        #     f.write(encoding)

        if self.settings.single_solve:
            solver = clingo.Control(['--heuristic=Domain','-Wnone'])

        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver


        # --- NEW CACHING LOGIC ---
        # Pre-parse and map all possible ground Clingo Symbols to your Python Literals
        self.symbol_to_literal = {}

        literal_id = 0

        # Iterate only through the atoms we care about to build the lookup table once
        for sym_atom in self.solver.symbolic_atoms.by_signature("body_literal", 4):
            sym = sym_atom.symbol
            args = sym.arguments
            predicate = args[1].name
            atom_args = tuple(args[3].arguments)
            # raw_args = tuple(arg.number for arg in atom_args)

            # # Build Janus string: "father(_V0,_V1)"
            # janus_args_str = ','.join(f'_V{i}' for i in raw_args)
            # janus_args_str = f'{predicate}({janus_args_str})'

            # # Build Print string: "father(V0,V1)"
            # print_args_str = ','.join(f'V{i}' for i in raw_args)
            # print_args_str = f'{predicate}({print_args_str})'

            # print(print_args_str, janus_args_str)
            # print(args[1].name, atom_args)

            # Key: The clingo.Symbol object itself
            # Value: Your pre-existing Python literal object
            self.symbol_to_literal[sym] = self.settings.cached_literals[predicate, atom_args]

        # self.cached_clingo_atoms2 = {}
        # 1. Add this tiny helper to correctly unpack nested tuples
        def unpack_symbol(sym):
            if sym.type == clingo.SymbolType.Number:
                return sym.number
            elif sym.type == clingo.SymbolType.String:
                return sym.string
            elif sym.type == clingo.SymbolType.Function:
                if len(sym.arguments) == 0:
                    return sym.name
                # In Clingo, a tuple is a function with an empty name ("")
                if sym.name == "":
                    return tuple(unpack_symbol(a) for a in sym.arguments)
                # If it's a named function with args (e.g., f(1)), you might want:
                return (sym.name, tuple(unpack_symbol(a) for a in sym.arguments))
            return str(sym)

        # 2. Use it in your loop
        for sym_atom in self.solver.symbolic_atoms:
            sym = sym_atom.symbol
            pred = sym.name

            # Now this will correctly capture [0, 'c3', 1, (1,)]
            args = tuple(unpack_symbol(a) for a in sym.arguments)

            pos_id = sym_atom.literal
            neg_id = -sym_atom.literal
            # print((True, pred, args))
            # This will now store the exact tuple your constrain method expects
            self.cached_clingo_atoms[(True, pred, args)] = pos_id
            self.cached_clingo_atoms[(False, pred, args)] = neg_id

    def get_prog(self):
        handle = iter(self.solver.solve(yield_=True))
        head = self.settings.head_literal
        gen_timer = stats.duration('generate')
        symbol_to_literal = self.symbol_to_literal
        cached_literals = self.settings.cached_literals

        while True:
            with gen_timer:
                model = next(handle, None)
                if model is None:
                    return
                self.model = model
                body = [symbol_to_literal[atom] for atom in model.symbols(shown=True)]
                rule = head, frozenset(body)
            yield frozenset({rule})

    def prune_size(self, size):
        if size in self.pruned_sizes:
            return
        self.pruned_sizes.add(size)
        atom = (True, "size", (size,))
        atom = self.cached_clingo_atoms.get(atom)
        if atom:
            self.model.context.add_nogood([atom])

    def constrain(self, cons):
        all_ground_cons = []

        for xs in cons:
            con_type = xs[0]
            con_prog = xs[1]
            con_size = xs[2] if self.settings.noisy and len(xs) > 2 else None
            match con_type:
                case Constraint.SPECIALISATION:
                    ground_cons = self.build_specialisation_constraint3(con_prog, con_size)
                case Constraint.UNSAT:
                    ground_cons = self.unsat_constraint2(con_prog)
                case Constraint.BANISH | Constraint.GENERALISATION:
                    ground_cons = self.build_generalisation_constraint3(con_prog, con_size)
            all_ground_cons.extend(ground_cons)

        add_nogood = self.model.context.add_nogood
        cached_clingo_atoms = self.cached_clingo_atoms

        for ground_body in set(all_ground_cons):
            try:
                nogood = [cached_clingo_atoms[atom] for atom in ground_body]
            except:
                # means we are trying to add an atom that is not allowed
                # caused by our naive grounding
                # for atom in ground_body:
                    # if atom not in cached_clingo_atoms:
                        # print('missing atom', atom)
                continue
            add_nogood(nogood)

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

    def build_specialisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        if not size:
            yield from self.find_variants(rule)
            return

        for body in self.find_variants(rule):
            body = list(body)
            body.append((True, 'program_size_at_least', (size,)))
            yield frozenset(body)

    def build_generalisation_constraint3(self, prog, size=None):
        rule = tuple(prog)[0]
        for body in self.find_variants(rule, max_rule_vars=True):
            body = list(body)
            body.append((True, 'body_size', (0, len(body))))
            if size:
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