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
from . util import rule_is_recursive, Constraint, format_prog, bias_order, Literal
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
        encoding.append(f'head_vars({settings.head_literal.arity}, {tuple(range(settings.head_literal.arity))}).')
        arities = set(a for p, a in self.settings.body_preds)
        arities.add(settings.head_literal.arity)
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

        with open('ENCODING-GEN.pl', 'w') as f:
            f.write(encoding)

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

    def parse_model_pi(self, model):
        settings = self.settings
        # directions = defaultdict(lambda: defaultdict(lambda: '?'))
        rule_index_to_body = defaultdict(set)
        rule_index_to_head = {}
        # rule_index_ordering = defaultdict(set)

        for atom in model:
            args = atom.arguments
            name = atom.name

            if name == 'body_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = settings.cached_atom_args[tuple(atom_args)]
                arity = len(atom_args)
                body_literal = (predicate, atom_args, arity)
                rule_index_to_body[rule_index].add(body_literal)

            elif name == 'head_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = settings.cached_atom_args[tuple(atom_args)]
                arity = len(atom_args)
                head_literal = (predicate, atom_args, arity)
                rule_index_to_head[rule_index] = head_literal

            # # TODO AC: STOP READING THESE THE MODELS
            # elif name == 'direction_':
            #     pred_name = args[0].name
            #     arg_index = args[1].number
            #     arg_dir_str = args[2].name

            #     if arg_dir_str == 'in':
            #         arg_dir = '+'
            #     elif arg_dir_str == 'out':
            #         arg_dir = '-'
            #     else:
            #         raise Exception(f'Unrecognised argument direction "{arg_dir_str}"')
            #     directions[pred_name][arg_index] = arg_dir

        prog = []

        for rule_index in rule_index_to_head:
            head_pred, head_args, head_arity = rule_index_to_head[rule_index]
            head = Literal(head_pred, head_args, {})
            body = set()
            for (body_pred, body_args, body_arity) in rule_index_to_body[rule_index]:
                body.add(Literal(body_pred, body_args, {}))
            body = frozenset(body)
            rule = head, body
            prog.append((rule))

        return frozenset(prog)

    def update_solver(self, size, num_vars, num_rules):
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

        # for handle in self.bad_handles:
        #     # if we know that rule_xyz is bad
        #     # we add the groundings of bad_stuff(R,ThisSize):- seen_rule(rule_xyz, R), R=0..MaxRules.
        #     for rule_id in range(0, self.settings.max_rules):
        #         h = (True, 'bad_stuff', (rule_id, size))
        #         b = (True, 'seen_rule', (handle, rule_id))
        #         new_rule = (h, (b,))
        #         to_add.append(new_rule)

        #     # we now eliminate bad stuff
        #     # :- seen_rule(rule_xyz,R1), bad_stuff(R2,Size), R1=0..MaxRules, R2=0..MaxRules, Size=0..ThisSize.
        #     for smaller_size in range(1, size+1):
        #         for r1 in range(1, self.settings.max_rules):
        #             atom1 = (True, 'seen_rule', (handle, r1))
        #             for r2 in range(1, self.settings.max_rules):
        #                 if r1 == r2:
        #                     continue
        #                 atom2 = (True, 'bad_stuff', (r2, smaller_size))
        #                 new_rule = ([], (atom1, atom2))
        #                 to_add.append(new_rule)

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
        size_con = [(atom_to_symbol("size", (size,)), True)]
        self.model.context.add_nogood(size_con)

    def constrain(self, tmp_new_cons):

        new_cons = set()
        for xs in tmp_new_cons:
            con_type = xs[0]
            con_prog = xs[1]

            if con_type == Constraint.GENERALISATION:
                xs = set(self.build_generalisation_constraint3(con_prog))
                new_cons.update(xs)
            elif con_type == Constraint.SPECIALISATION:
                xs = set(self.build_specialisation_constraint3(con_prog))
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
                assert(False)

        nogoods = []
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
            nogoods.append(nogood)

        model = self.model
        for x in nogoods:
            model.context.add_nogood(x)

        # add ground cons
        self.new_ground_cons.update(new_cons)

    def make_rule_handle(self, rule):
        cached_handles = self.cached_handles
        k = hash(rule)
        if k in cached_handles:
            return cached_handles[k]
        head, body = rule
        body_literals = sorted(body, key = operator.attrgetter('predicate'))
        handle = ''.join(f'{literal.predicate}{"".join(map(str,literal.arguments))}' for literal in [head] + body_literals)
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
                yield r1 | r2

    def build_generalisation_constraint3(self, prog, size=None):

        if len(prog) == 1:
            rule = list(prog)[0]
            head, body = rule
            handle = self.make_rule_handle(rule)

            if handle in self.seen_handles:
                con = []
                con.append((True, 'seen_rule', (handle, 0)))
                con.append((True, 'body_size', (0, len(body))))
                yield frozenset(con)
                return

            self.new_seen_rules.update(self.build_seen_rule2(rule, False))
            for variant in self.find_variants3(rule):
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
            for variant in self.find_variants3(base, ruleid=0):
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
            for variant in self.find_variants3(rec, ruleid=1):
                con = []
                con.extend(variant)
                con.append((True, 'body_size', (1, len(rec_body))))
                recs.append(frozenset(con))

        for r1 in bases:
            for r2 in recs:
                yield r1 | r2

    def find_variants3(self, rule, ruleid=0):
        head, body = rule
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head.arity)
        subset = range(head.arity, self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for atom in body:
                new_args = tuple(xs[arg] for arg in atom.arguments)
                new_literal = (True, 'body_literal', (ruleid, atom.predicate, len(new_args), new_args))
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
                # new_body.append((True, 'head_literal', (rule_id, head.predicate, len(head.arguments), ground_head_args)))
                if pi:
                    new_body.append(('head_literal', (rule_id, head.predicate, len(head.arguments), ground_head_args)))
                for atom in body:
                    new_args = []
                    for x in atom.arguments:
                        if x in head_vars:
                            # v = ord(x)- ord('A')
                            new_args.append(x)
                        else:
                            new_args.append(xs[indexes[x]])
                    new_args = tuple(new_args)
                    new_body.append(('body_literal', (rule_id, atom.predicate, len(atom.arguments), new_args)))
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
        assignments = self.find_deep_bindings4(body)
        out = []
        for rule_id in range(self.settings.max_rules):
            for assignment in assignments:
                rule = []
                for atom in body:
                    args2 = tuple(assignment[x] for x in atom.arguments)
                    rule.append((True, 'body_literal', (rule_id, atom.predicate, len(atom.arguments), args2)))
                out.append(frozenset(rule))
        return out

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