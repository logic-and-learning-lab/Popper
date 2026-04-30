import re
from importlib import resources
from . util import GENERALISATION, SPECIALISATION, UNSAT, REDUNDANCY_CONSTRAINT1, REDUNDANCY_CONSTRAINT2, TMP_ANDY, BANISH
from itertools import permutations
from . import stats
from . import _gen_norec_id_native
from ._gen_norec_clingo_capi import NativeNoRecControl

class Generator:

    def __init__(self, settings, _state, bkcons=None):
        self.settings = settings
        self.cached_clingo_atoms = {}
        self.pruned_sizes = set()

        encoding = self._build_encoding(bkcons or [])
        self.encoding = encoding
        self.native_control = NativeNoRecControl(encoding)
        self._build_native_caches()

        self._max_vars = settings.max_vars
        self._head_args = settings.head_literal.arguments
        self._head_types = settings.head_types
        self._body_types = settings.body_types
        self._literal_to_id = settings.literal_to_id
        self._literal_id_to_pred = settings.literal_id_to_pred
        self._literal_id_to_args = settings.literal_id_to_args
        self._literal_id_by_pred_args = settings.literal_id_by_pred_args
        self._literal_id_to_literal = settings.literal_id_to_literal

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

    def _build_native_caches(self):
        body_cache, special_cache = self.native_control.build_literal_caches(self.settings.literal_id_by_pred_args)
        self.body_literal_id_to_clingo = body_cache
        for (pred, args), lit in special_cache.items():
            self.cached_clingo_atoms[(True, pred, args)] = lit
            self.cached_clingo_atoms[(False, pred, args)] = -lit

    def get_prog(self):
        head = self.settings.head_literal
        gen_timer = stats.duration('generate')
        literal_id_to_literal = self._literal_id_to_literal

        while True:
            with gen_timer:
                body_ids = self.native_control.next_model_body_ids()
                if body_ids is None:
                    return
                rule = head, frozenset(literal_id_to_literal[lit_id] for lit_id in body_ids)
            yield frozenset((rule,))

    def prune_size(self, size):
        if size in self.pruned_sizes:
            return
        self.pruned_sizes.add(size)
        atom = self.cached_clingo_atoms.get((True, "size", (size,)))
        if atom:
            self.native_control.add_nogood([atom])

    def constrain(self, cons):
        _gen_norec_id_native.constrain(
            cons,
            self.native_control,
            self._max_vars,
            self._head_args,
            self._head_types,
            self._body_types,
            self._literal_to_id,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
            self.body_literal_id_to_clingo,
            self.cached_clingo_atoms,
            SPECIALISATION,
            UNSAT,
            BANISH,
            GENERALISATION,
        )

    def body_to_literal_ids(self, body):
        return _gen_norec_id_native.body_to_literal_ids(body, self._literal_to_id)

    def prog_to_id_rule(self, prog):
        return _gen_norec_id_native.prog_to_id_rule(prog, self._literal_to_id)

    def remap_body_literal_ids(self, body_ids):
        return _gen_norec_id_native.remap_body_literal_ids(
            body_ids,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
        )

    def unsat_constraint(self, body):
        yield from self.unsat_constraint_ids(self.body_to_literal_ids(body))

    def unsat_constraint_ids(self, body_ids):
        yield from _gen_norec_id_native.unsat_constraint_ids(
            body_ids,
            self._max_vars,
            self._head_args,
            self._head_types,
            self._body_types,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
            self.body_literal_id_to_clingo,
        )

    def build_specialisation_constraint(self, prog, size=None):
        yield from self.build_specialisation_constraint_ids(self.prog_to_id_rule(prog), size)

    def build_specialisation_constraint_ids(self, rule, size=None):
        yield from _gen_norec_id_native.build_specialisation_constraint_ids(
            rule,
            size,
            self._max_vars,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
            self.body_literal_id_to_clingo,
            self.cached_clingo_atoms,
        )

    def build_generalisation_constraint(self, prog, size=None):
        yield from self.build_generalisation_constraint_ids(self.prog_to_id_rule(prog), size)

    def build_generalisation_constraint_ids(self, rule, size=None):
        yield from _gen_norec_id_native.build_generalisation_constraint_ids(
            rule,
            size,
            self._max_vars,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
            self.body_literal_id_to_clingo,
            self.cached_clingo_atoms,
        )

    def find_variants(self, rule, max_rule_vars=False):
        yield from self.find_variants_ids((rule[0], self.body_to_literal_ids(rule[1])), max_rule_vars)

    def find_variants_ids(self, rule, max_rule_vars=False):
        head, body_ids = rule
        yield from _gen_norec_id_native.find_variants_ids(
            head.arguments,
            body_ids,
            self._max_vars,
            max_rule_vars,
            self._literal_id_to_pred,
            self._literal_id_to_args,
            self._literal_id_by_pred_args,
            self.body_literal_id_to_clingo,
        )

    def find_deep_bindings(self, body):
        yield from self.find_deep_bindings_ids(self.body_to_literal_ids(body))

    def find_deep_bindings_ids(self, body_ids):
        yield from _gen_norec_id_native.find_deep_bindings_ids(
            body_ids,
            self._max_vars,
            self._head_args,
            self._head_types,
            self._body_types,
            self._literal_id_to_pred,
            self._literal_id_to_args,
        )
