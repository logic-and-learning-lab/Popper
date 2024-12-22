import time
import re
from pysat.formula import CNF
from pysat.solvers import Solver
from pysat.card import *
import clingo
import operator
import numbers
import clingo.script
import pkg_resources
from collections import defaultdict
from . util import rule_is_recursive, Constraint, Literal
clingo.script.enable_python()
from clingo import Function, Number, Tuple_
from itertools import permutations

DEFAULT_HEURISTIC = """
#heuristic size(N). [1000-N,true]
"""

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

class Generator:

    def __init__(self, settings, bkcons=[]):
        self.settings = settings
        self.seen_handles = set()
        self.assigned = {}
        self.seen_symbols = {}
        self.cached_clingo_atoms = {}
        self.handle = None
        self.cached_handles = {}
        self.cached4 = {}
        self.pruned_sizes = set()


        self.debug = defaultdict(set)

        # handles for rules that are minimal and unsatisfiable
        self.bad_handles = set()
        # new rules added to the solver, such as: seen(id):- head_literal(...), body_literal(...)

        self.new_seen_rules = set()
        self.new_ground_cons = set()

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan-old.pl").decode()
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
        head_arity = len(settings.head_literal.arguments)
        encoding.append(f'head_vars({head_arity}, {tuple(range(head_arity))}).')
        arities = set(a for p, a in self.settings.body_preds)
        arities.add(head_arity)
        for arity in arities:
            for xs in permutations(range(settings.max_vars), arity):
                encoding.append(f'vars({arity}, {tuple(xs)}).')

                for i, x in enumerate(xs):
                    encoding.append(f'var_pos({x}, {tuple(xs)}, {i}).')

        # types = tuple(self.settings.head_types)
        # str_types = str(types).replace("'","")
        # for x, i in enumerate(self.settings.head_types):
        #     encoding.append(f'type_pos({str_types}, {i}, {x}).')

        # for pred, types in self.settings.body_types.items():
        #     types = tuple(types)
        #     str_types = str(types).replace("'","")
        #     for i, x in enumerate(types):

        #         encoding.append(f'type_pos({str_types}, {i}, {x}).')

        # for pred, xs in self.settings.directions.items():
        #     for i, v in xs.items():
        #         if v == '+':
        #             encoding.append(f'direction_({pred}, {i}, in).')
        #         if v == '-':
        #             encoding.append(f'direction_({pred}, {i}, out).')

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

        # if settings.bkcons:
        encoding.extend(bkcons)

        if settings.single_solve:
            if settings.order_space:
                encoding.append(DEFAULT_HEURISTIC)

        encoding = '\n'.join(encoding)

        # with open('ENCODING-GEN.pl', 'w') as f:
            # f.write(encoding)

        if self.settings.single_solve:
            solver = clingo.Control(['--heuristic=Domain','-Wnone'])
        else:
            solver = clingo.Control(['-Wnone'])
            NUM_OF_LITERALS = """
            %%% External atom for number of literals in the program %%%%%
            #external size_in_literals(n).
            :-
                size_in_literals(n),
                #sum{K+1,Clause : body_size(Clause,K)} != n.
            """
            solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)

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

        if self.settings.single_solve:
            return self.parse_model_single_rule(atoms)

        if self.settings.pi_enabled:
            return self.parse_model_pi(atoms)

        return self.parse_model_recursion(atoms)

    def gen_symbol(self, literal, backend):
        sign, pred, args = literal
        k = hash(literal)
        if k in self.seen_symbols:
            symbol = self.seen_symbols[k]
        else:
            symbol = backend.add_atom(atom_to_symbol(pred, args))
            self.seen_symbols[k] = symbol
        return symbol

    def parse_model_recursion(self, model):
        settings = self.settings
        rule_index_to_body = defaultdict(set)
        head = settings.head_literal
        cached_literals = settings.cached_literals

        for atom in model:
            args = atom.arguments
            rule_index = args[0].number
            predicate = args[1].name
            atom_args = tuple(args[3].arguments)
            literal = cached_literals[(predicate, atom_args)]
            rule_index_to_body[rule_index].add(literal)

        prog = []
        for rule_index, body in rule_index_to_body.items():
            body = frozenset(body)
            rule = head, body
            prog.append((rule))

        return frozenset(prog)

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

    def update_solver(self, size):
        self.update_number_of_literals(size)

        # rules to add via Clingo's backend interface
        to_add = []
        to_add.extend(([], x) for x in self.new_ground_cons)

        # new_seen_rules = set()

        # add handles for newly seen rules
        # for handle, rule in handles:
        for rule in self.new_seen_rules:
            # print(rule)
            head, body = rule
            head_pred, head_args = head
            new_head = (True, head_pred, head_args)
            new_body = frozenset((True, pred, args) for pred, args in body)
            to_add.append((new_head, new_body))

            if head_args[0] in self.debug:
                print('wtf???')
                print(head_args[0])
                print('NEW', rule)
                for x in self.debug[head_args[0]]:
                    print('OLD', x)
                assert(False)

            # print(head_args[0])

            # ADD SEEN HANDLE
            self.seen_handles.add(head_args[0])

        for rule in self.new_seen_rules:
            # print(rule)
            head, body = rule
            head_pred, head_args = head
            # new_head = (True, head_pred, head_args)
            # new_body = frozenset((True, pred, args) for pred, args in body)
            # to_add.append((new_head, new_body))
            self.debug[head_args[0]].add(rule)


        with self.solver.backend() as backend:
            for head, body in to_add:
                head_literal = []
                if head:
                    head_literal = [self.gen_symbol(head, backend)]
                body_lits = []
                for literal in body:
                    sign, _pred, _args = literal
                    symbol = self.gen_symbol(literal, backend)
                    body_lits.append(symbol if sign else -symbol)
                backend.add_rule(head_literal, body_lits)

        # self.seen_handles.update(new_seen_rules)
        # RESET SO WE DO NOT KEEP ADDING THEM
        self.new_ground_cons = set()
        self.new_seen_rules = set()

        self.handle = iter(self.solver.solve(yield_ = True))

    def update_number_of_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom[0] == 'size_in_literals' and truth_value:
                if atom[1] == size:
                    continue
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

    def prune_size(self, size):
        # pass
        if size in self.pruned_sizes:
            return
        self.pruned_sizes.add(size)
        size_con = [(atom_to_symbol("size", (size,)), True)]
        self.model.context.add_nogood(size_con)

    def constrain(self, tmp_new_cons):
        new_cons = set()

        for xs in tmp_new_cons:
            con_type = xs[0]
            con_prog = xs[1]

            if con_type == Constraint.GENERALISATION:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                xs = set(self.build_generalisation_constraint3(con_prog, con_size))
                new_cons.update(xs)
            elif con_type == Constraint.SPECIALISATION:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                xs = set(self.build_specialisation_constraint3(con_prog, con_size))
                new_cons.update(xs)
            elif con_type == Constraint.UNSAT:
                new_cons.update(self.unsat_constraint2(con_prog))
            elif con_type == Constraint.REDUNDANCY_CONSTRAINT1:
                xs = set(self.redundancy_constraint1(con_prog))
                new_cons.update(xs)
            elif con_type == Constraint.REDUNDANCY_CONSTRAINT2:
                if len(con_prog) == 1:
                    xs = set(self.redundancy_constraint1(con_prog))
                else:
                    xs = set(self.build_specialisation_constraint3(con_prog))
                new_cons.update(xs)
            elif con_type == Constraint.TMP_ANDY:
                assert(False)
            elif con_type == Constraint.BANISH:
                xs = set(self.build_banish_constraint(con_prog))
                new_cons.update(xs)

        tmp = self.model.context.add_nogood

        for ground_body in new_cons:
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

        # add ground cons
        self.new_ground_cons.update(new_cons)

    def make_rule_handle(self, rule):
        cached_handles = self.cached_handles
        k = hash(rule)
        if k in cached_handles:
            return cached_handles[k]
        head, body = rule
        body_literals = sorted(body, key = operator.attrgetter('predicate'))
        handle = ''.join(f'{pred}{"".join(map(str, args))}' for pred, args in [head] + body_literals)
        cached_handles[k] = handle
        return handle

    def build_specialisation_constraint3(self, prog, size=None):

        if len(prog) == 1:
            rule = list(prog)[0]
            head, body = rule
            handle = self.make_rule_handle(rule)
            if handle in self.seen_handles:
                con = []
                con.append((True, 'seen_rule', (handle, 0)))
                con.append((False, 'clause', (1, )))
                if size:
                    con.append((True, 'program_size_at_least', (size,)))
                yield frozenset(con)
                return

            self.new_seen_rules.update(self.build_seen_rule2(rule, False))

            for variant in self.find_variants3(rule):
                con = []
                con.extend(variant)
                con.append((False, 'clause', (1, )))
                if size:
                    con.append((True, 'program_size_at_least', (size,)))
                yield frozenset(con)
            return

        base = [rule for rule in prog if not rule_is_recursive(rule)][0]

        bases = []
        handle = self.make_rule_handle(base)
        if handle in self.seen_handles:
            bases.append(frozenset([(True, 'seen_rule', (handle, 0))]))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(base, False))
            for body in self.find_variants3(base, ruleid=0):
                body = list(body)
                bases.append(frozenset(body))

        recs = []
        rec = [rule for rule in prog if rule_is_recursive(rule)][0]
        handle = self.make_rule_handle(rec)
        if handle in self.seen_handles:
            recs.append(frozenset([(True, 'seen_rule', (handle, 1))]))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(rec, True))
            for body in self.find_variants3(rec, ruleid=1):
                body = list(body)
                recs.append(frozenset(body))

        for r1 in bases:
            for r2 in recs:
                if not size:
                    yield r1 | r2
                else:
                    yield r1 | r2 | {(True, 'program_size_at_least', (size,))}
                    # yield frozenset(con)

    def build_generalisation_constraint3(self, prog, size=None):

        if len(prog) == 1:
            rule = list(prog)[0]
            head, body = rule
            handle = self.make_rule_handle(rule)

            if handle in self.seen_handles:
                con = []
                con.append((True, 'seen_rule', (handle, 0)))
                con.append((True, 'body_size', (0, len(body))))
                if size:
                    con.append((True, 'program_size_at_least', (size,)))
                yield frozenset(con)
                return

            self.new_seen_rules.update(self.build_seen_rule2(rule, False))
            for variant in self.find_variants3(rule, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (0, len(body))))
                if size:
                    con.append((True, 'program_size_at_least', (size,)))
                yield frozenset(con)
            return

        base = [rule for rule in prog if not rule_is_recursive(rule)][0]
        base_head, base_body = base

        bases = []
        handle = self.make_rule_handle(base)
        if handle in self.seen_handles:
            con = []
            con.append((True, 'seen_rule', (handle, 0)))
            con.append((True, 'body_size', (0, len(base_body))))
            bases.append(frozenset(con))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(base, False))
            for variant in self.find_variants3(base, ruleid=0, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (0, len(base_body))))
                bases.append(frozenset(con))

        rec = [rule for rule in prog if rule_is_recursive(rule)][0]
        rec_head, rec_body = rec
        recs = []
        handle = self.make_rule_handle(rec)
        if handle in self.seen_handles:
            con = []
            con.append((True, 'seen_rule', (handle, 1)))
            con.append((True, 'body_size', (1, len(rec_body))))
            bases.append(frozenset(con))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(rec, True))
            for variant in self.find_variants3(rec, ruleid=1, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (1, len(rec_body))))
                recs.append(frozenset(con))

        for r1 in bases:
            for r2 in recs:
                # con = r1 | r2
                if not size:
                    yield r1 | r2
                else:
                    # con.add()
                    yield r1 | r2 | {(True, 'program_size_at_least', (size,))}


    def build_banish_constraint(self, prog):

        if len(prog) == 1:
            rule = list(prog)[0]
            head, body = rule
            handle = self.make_rule_handle(rule)
            if handle in self.seen_handles:
                con = []
                con.append((True, 'seen_rule', (handle, 0)))
                con.append((True, 'body_size', (0, len(body))))
                con.append((False, 'clause', (1, )))
                yield frozenset(con)
                return

            self.new_seen_rules.update(self.build_seen_rule2(rule, False))

            for variant in self.find_variants3(rule, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (0, len(body))))
                con.append((False, 'clause', (1, )))
                yield frozenset(con)
            return

        base = [rule for rule in prog if not rule_is_recursive(rule)][0]
        base_head, base_body = base

        bases = []
        handle = self.make_rule_handle(base)
        if handle in self.seen_handles:
            con = []
            con.append((True, 'seen_rule', (handle, 0)))
            con.append((True, 'body_size', (0, len(base_body))))
            bases.append(frozenset(con))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(base, False))
            for variant in self.find_variants3(base, ruleid=0, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (0, len(base_body))))
                bases.append(frozenset(con))

        rec = [rule for rule in prog if rule_is_recursive(rule)][0]
        rec_head, rec_body = rec
        recs = []
        handle = self.make_rule_handle(rec)
        if handle in self.seen_handles:
            con = []
            con.append((True, 'seen_rule', (handle, 1)))
            con.append((True, 'body_size', (1, len(rec_body))))
            bases.append(frozenset(con))
        else:
            self.new_seen_rules.update(self.build_seen_rule2(rec, True))
            for variant in self.find_variants3(rec, ruleid=1, max_rule_vars=True):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (1, len(rec_body))))
                recs.append(frozenset(con))

        for r1 in bases:
            for r2 in recs:
                yield r1 | r2

    def find_variants3(self, rule, ruleid=0, max_rule_vars=False):
        head, body = rule
        head_arity = len(self.settings.head_literal.arguments)
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head_arity)
        if max_rule_vars:
            subset = range(len(head.arguments), len(body_vars | set(head.arguments)))
        else:
            subset = range(len(head.arguments), self.settings.max_vars)
        subset = range(head_arity, self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (True, 'body_literal', (ruleid, pred, len(new_args), new_args))
                new_body.append(new_literal)
            yield frozenset(new_body)

    def build_seen_rule2(self, rule, is_rec):
        pi = self.settings.pi_enabled

        handle = self.make_rule_handle(rule)
        head, body = rule

        head_vars = set(head.arguments)
        body_vars = set(x for atom in body for x in atom.arguments if x not in head_vars)

        possible_values = list(range(len(head_vars), self.settings.max_vars))
        perms = list(permutations(possible_values, len(body_vars)))
        indexes = {x:i for i, x in enumerate(list(body_vars))}

        ground_head_args = tuple(range(len(head_vars)))

        out = []
        for rule_id in range(self.settings.max_rules):
            if is_rec and rule_id == 0:
                continue
            # new_head = (True, 'seen_rule', (handle, rule_id))
            new_head = ('seen_rule', (handle, rule_id))
            for xs in perms:
                new_body = []
                if pi:
                    new_body.append(('head_literal', (rule_id, head.predicate, len(head.arguments), ground_head_args)))
                for pred, args in body:
                    new_args = []
                    for x in args:
                        if x in head_vars:
                            # v = ord(x)- ord('A')
                            new_args.append(x)
                        else:
                            new_args.append(xs[indexes[x]])
                    new_args = tuple(new_args)
                    new_body.append(('body_literal', (rule_id, pred, len(args), new_args)))
                new_rule = (new_head, frozenset(new_body))
                out.append(new_rule)
        return frozenset(out)

    # only works with single rule programs
    # if a single rule R is unsatisfiable, then for R to appear in an optimal solution H it must be the case that H has a recursive rule that does not specialise R
    def redundancy_constraint1(self, prog):
        rule = list(prog)[0]
        handle = self.make_rule_handle(rule)
        if handle in self.seen_handles:
            assert(False)
        head, body_ = rule
        self.seen_handles.update(self.build_seen_rule2(rule, False))

        for body in self.find_variants3(rule, 0):
            body = list(body)
            body.append((True, 'num_recursive', (head.predicate, 0)))
            yield frozenset(body)

        for ruleid in range(1, self.settings.max_rules):
            for body in self.find_variants3(rule, ruleid):
                body = list(body)
                body.append((True, 'num_recursive', (head.predicate, 1)))
                yield frozenset(body)

    def unsat_constraint2(self, body):
        # if no types, remap variables
        if len(self.settings.body_types) == 0:
            _, body = remap_variables((None, body))

        assignments = self.find_deep_bindings4(body)
        for rule_id in range(self.settings.max_rules):
            for assignment in assignments:
                rule = []
                for pred, args in body:
                    args2 = tuple(assignment[x] for x in args)
                    rule.append((True, 'body_literal', (rule_id, pred, len(args), args2)))
                yield frozenset(rule)

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
    for atom in body:
        new_args = []
        for var in atom.arguments:
            if var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(atom.predicate, tuple(new_args))
        new_body.add(new_atom)

    return head, frozenset(new_body)