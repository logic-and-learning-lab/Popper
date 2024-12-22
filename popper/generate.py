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
from . util import rule_is_recursive, Constraint, Literal, format_rule, remap_variables
clingo.script.enable_python()
from clingo import Function, Number, Tuple_
from itertools import permutations
import dataclasses

@dataclasses.dataclass(frozen=True)
class Var:
    name: str

@dataclasses.dataclass(frozen=True)
class RuleVar(Var):
    pass

@dataclasses.dataclass(frozen=True)
class VarVar(Var):
    rule: RuleVar

META_PREDS = {'<', '==', '>='}

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

def find_all_vars(body):
    all_vars = set()
    for literal in body:
        for arg in literal.arguments:
            if isinstance(arg, Var):
                all_vars.add(arg)
            elif isinstance(arg, tuple):
                for t_arg in arg:
                    if isinstance(t_arg, Var):
                        all_vars.add(t_arg)
    return all_vars

# AC: When grounding constraint rules, we only care about the vars and the constraints, not the actual literals
def grounding_hash(body, all_vars):
    cons = frozenset((lit.predicate, lit.arguments) for lit in body if lit.predicate in META_PREDS)
    return hash((frozenset(all_vars), cons))

def build_seen_rule_literal(handle, rule_var):
    return Literal('seen_rule', (handle, rule_var))

def build_rule_literals(rule, rule_var, pi=False):
    literals = []
    head, body = rule
    if pi:
        yield Literal('head_literal', (rule_var, head.predicate, len(head.arguments), tuple(vo_variable2(rule_var, v) for v in head.arguments)))

    for body_literal in body:
        yield Literal('body_literal', (rule_var, body_literal.predicate, len(body_literal.arguments), tuple(vo_variable2(rule_var, v) for v in body_literal.arguments)))
    for idx, var in enumerate(head.arguments):
        yield eq(vo_variable2(rule_var, var), idx)

    if rule_is_recursive(rule):
        yield gteq(rule_var, 1)

class Generator:

    def __init__(self, settings, bkcons=[]):
        self.savings = 0
        self.settings = settings
        self.seen_handles = set()
        self.assigned = {}
        self.seen_symbols = {}
        self.cached_clingo_atoms = {}
        self.handle = None
        self.cached_handles = {}
        self.cached_grounded = {}

        # ex-grounder
        self.seen_assignments = {}
        self.cached4 = {}

        # handles for rules that are minimal and unsatisfiable
        self.bad_handles = set()
        # new rules added to the solver, such as: seen(id):- head_literal(...), body_literal(...)
        self.all_handles = set()

        # TODO: dunno
        self.all_ground_cons = set()
        # TODO: dunno
        self.new_ground_cons = set()

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan-old.pl").decode()
        # alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
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

        # FG Heuristic for single solve
        # - considering a default order of minimum rules, then minimum literals, and then minimum variables
        # - considering a preference for minimum hspace size parameters configuration
        if settings.single_solve:
            if settings.order_space:
                assert(False)
                pass
                # horder = bias_order(settings, max_size)
                # iorder = 1
                # for (size, n_vars, n_rules, _) in horder:
                #     encoding.append(f'h_order({iorder},{size},{n_vars},{n_rules}).')
                #     iorder += 1
                # HSPACE_HEURISTIC = """
                # #heuristic hspace(N). [1000-N@30,true]
                # hspace(N) :- h_order(N,K,V,R), size(K), size_vars(V), size_rules(R).
                # size_vars(V):- #count{K : clause_var(_,K)} == V.
                # size_rules(R):- #count{K : clause(K)} == R.
                # """

                # encoding.append(HSPACE_HEURISTIC)
            elif settings.no_bias:
                DEFAULT_HEURISTIC = """
                size_vars(V):- #count{K : clause_var(_,K)} == V.
                size_rules(R):- #count{K : clause(K)} == R.
                #heuristic size_rules(R). [1500-R@30,true]
                #heuristic size(N). [1000-N@20,true]
                #heuristic size_vars(V). [500-V@10,true]
                """
                encoding.append(DEFAULT_HEURISTIC)
            else:
                DEFAULT_HEURISTIC = """
                #heuristic size(N). [1000-N,true]
                """
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

            if self.settings.no_bias:
                NUM_OF_VARS = """
                %%% External atom for number of variables in the program %%%%%
                #external size_in_vars(v).
                :-
                    size_in_vars(v),
                    #max{V : clause_var(_,V)} != v - 1.
                """
                solver.add('number_of_vars', ['v'], NUM_OF_VARS)

                NUM_OF_RULES = """
                %%% External atom for number of rules in the program %%%%%
                #external size_in_rules(r).
                :-
                    size_in_rules(r),
                    #max{R : clause(R)} != r - 1.
                """
                solver.add('number_of_rules', ['r'], NUM_OF_RULES)



        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver

    # @profile
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
            head = Literal(head_pred, head_args)
            body = set()
            for (body_pred, body_args, body_arity) in rule_index_to_body[rule_index]:
                body.add(Literal(body_pred, body_args))
            body = frozenset(body)
            rule = head, body
            prog.append((rule))

        return frozenset(prog)

    def update_solver(self, size):
        self.update_number_of_literals(size)
        # self.update_number_of_vars(num_vars)
        # self.update_number_of_rules(num_rules)

        # rules to add via Clingo's backend interface
        to_add = []
        to_add.extend(([], x) for x in self.all_ground_cons)

        new_seen_rules = set()

        # add handles for newly seen rules
        # for handle, rule in handles:
        for rule in self.all_handles:
            head, body = rule
            head_pred, head_args = head

            # print(head, body)

            if head_pred == 'seen_rule':
                new_seen_rules.add(head_args[0])
            else:
                assert(False)

            new_head = (True, head_pred, head_args)
            new_body = frozenset((True, pred, args) for pred, args in body)
            to_add.append((new_head, new_body))


        if self.settings.no_bias:
            self.bad_handles = []
        for handle in self.bad_handles:
            # print(handle)
            # if we know that rule_xyz is bad
            # we add the groundings of bad_stuff(R,ThisSize):- seen_rule(rule_xyz, R), R=0..MaxRules.
            for rule_id in range(0, self.settings.max_rules):
                h = (True, 'bad_stuff', (rule_id, size))
                b = (True, 'seen_rule', (handle, rule_id))
                new_rule = (h, (b,))
                to_add.append(new_rule)

            # we now eliminate bad stuff
            # :- seen_rule(rule_xyz,R1), bad_stuff(R2,Size), R1=0..MaxRules, R2=0..MaxRules, Size=0..ThisSize.
            for smaller_size in range(1, size+1):
                for r1 in range(1, self.settings.max_rules):
                    atom1 = (True, 'seen_rule', (handle, r1))
                    for r2 in range(1, self.settings.max_rules):
                        if r1 == r2:
                            continue
                        atom2 = (True, 'bad_stuff', (r2, smaller_size))
                        new_rule = ([], (atom1, atom2))
                        to_add.append(new_rule)

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

        self.seen_handles.update(new_seen_rules)

        # RESET SO WE DO NOT KEEP ADDING THEM
        self.all_ground_cons = set()
        self.bad_handles = set()
        self.all_handles = set()

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

    def update_number_of_vars(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom[0] == 'size_in_vars' and truth_value:
                if atom[1] == size:
                    continue
                self.assigned[atom] = False
                symbol = clingo.Function('size_in_vars', [clingo.Number(atom[1])])
                self.solver.release_external(symbol)

        # 2. Ground the new size
        self.solver.ground([('number_of_vars', [clingo.Number(size)])])

        # 3. Assign the new size
        self.assigned[('size_in_vars', size)] = True

        # @NOTE: Everything passed to Clingo must be Symbol. Refactor after
        # Clingo updates their cffi API
        symbol = clingo.Function('size_in_vars', [clingo.Number(size)])
        self.solver.assign_external(symbol, True)

    def update_number_of_rules(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom[0] == 'size_in_rules' and truth_value:
                if atom[1] == size:
                    continue
                self.assigned[atom] = False
                symbol = clingo.Function('size_in_rules', [clingo.Number(atom[1])])
                self.solver.release_external(symbol)

        # 2. Ground the new size
        self.solver.ground([('number_of_rules', [clingo.Number(size)])])

        # 3. Assign the new size
        self.assigned[('size_in_rules', size)] = True

        # @NOTE: Everything passed to Clingo must be Symbol. Refactor after
        # Clingo updates their cffi API
        symbol = clingo.Function('size_in_rules', [clingo.Number(size)])
        self.solver.assign_external(symbol, True)

    def prune_size(self, size):
        size_con = [(atom_to_symbol("size", (size,)), True)]
        # print('moo', size)
        self.model.context.add_nogood(size_con)

    # @profile
    def get_ground_rules(self, rule):
        head, body = rule
        all_vars = find_all_vars(body)

        # find bindings for variables in the rule
        assignments = self.find_bindings(body, all_vars)

        # keep only standard literals
        body = tuple(literal for literal in body if not literal.predicate in META_PREDS)
        # ground the rule for each variable assignment
        return set(self.ground_rule((head, body), assignment) for assignment in assignments)

    def parse_handles(self, new_handles):
        out = []
        for rule in new_handles:
            head, body = rule
            for h, b in self.get_ground_rules(rule):
                _, p, args = h
                out_h = (p, args)
                out_b = frozenset((b_pred, b_args) for _, b_pred, b_args in b)
                out.append((out_h, out_b))
        return out

    # @profile
    def constrain(self, tmp_new_cons):
        model = self.model
        new_cons = set()
        debug = True
        # debug = False

        # for con_type, con_prog, con_prog_ordering in tmp_new_cons:
        for xs in tmp_new_cons:
            con_type = xs[0]
            con_prog = xs[1]
            # con_prog_ordering = xs[2]
            # print(con_type)

            # if con_type not in (1, 2):
                # print(con_type)
            # con_prog, con_prog_ordering
            # if debug and con_type != Constraint.UNSAT:
                # print('')
                # print('\t','--', con_type)
                # for rule in order_prog(con_prog):
                    # print('\t', format_rule(order_rule(rule)))
            if con_type == Constraint.GENERALISATION:
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                new_rule_handles2, con = self.build_generalisation_constraint2(con_prog, gen_size=con_size)
                self.all_handles.update(new_rule_handles2)
                new_cons.add(con)
            elif con_type == Constraint.SPECIALISATION:
                # con_size = xs[2]
                con_size = None
                if self.settings.noisy and len(xs)>2:
                    con_size = xs[2]
                new_rule_handles2, con = self.build_specialisation_constraint2(con_prog, spec_size=con_size)
                self.all_handles.update(new_rule_handles2)
                new_cons.add(con)
            elif con_type == Constraint.UNSAT:
                cons_ = self.unsat_constraint2(con_prog)
                self.new_ground_cons.update(cons_)
            elif con_type == Constraint.REDUNDANCY_CONSTRAINT1:
                bad_handle, new_rule_handles2, con = self.redundancy_constraint1(con_prog)
                self.bad_handles.add(bad_handle)
                self.all_handles.update(new_rule_handles2)
                new_cons.add(con)
            elif con_type == Constraint.REDUNDANCY_CONSTRAINT2:
                new_rule_handles2, cons = self.redundancy_constraint2(con_prog)
                self.all_handles.update(new_rule_handles2)
                new_cons.update(cons)
            elif con_type == Constraint.TMP_ANDY:
                new_cons.update(self.andy_tmp_con(con_prog))
            elif con_type == Constraint.BANISH:
                new_rule_handles2, con = self.build_banish_constraint(con_prog)
                self.all_handles.update(new_rule_handles2)
                new_cons.add(con)

        self.all_ground_cons.update(self.new_ground_cons)
        ground_bodies = set()
        ground_bodies.update(self.new_ground_cons)

        for con in new_cons:
            ground_rules = self.get_ground_rules((None, con))
            for ground_rule in ground_rules:
                _ground_head, ground_body = ground_rule
                ground_bodies.add(ground_body)
                # if con_type == Constraint.REDUNDANCY_CONSTRAINT1:
                # print(sorted(ground_body))
                self.all_ground_cons.add(frozenset(ground_body))

        nogoods = []
        for ground_body in ground_bodies:
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

        # with self.settings.stats.duration('constrain_clingo'):
        for x in nogoods:
            model.context.add_nogood(x)

        self.new_ground_cons = set()

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

    def build_generalisation_constraint2(self, prog, gen_size=False):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        recs = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var

            if self.settings.single_solve:
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            else:
                is_rec = rule_is_recursive(rule)
                if is_rec:
                    recs.append((len(body), rule))
                if is_rec:
                    literals.append(gteq(rule_var, 1))
                else:
                    literals.append(lt(rule_var, 1))
                handle = self.make_rule_handle(rule)
                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                    if is_rec:
                        literals.append(gteq(rule_var, 1))
                else:
                    xs = self.build_seen_rule2(rule, is_rec)
                    # NEW!!
                    # self.seen_handles.update(xs)
                    new_handles.update(xs)
                    literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(body_size_literal(rule_var, len(body)))

        if gen_size:
            literals.append(Literal('program_size_at_least', (gen_size,)))

        # if rule_ordering:
        #     literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        # else:
        #     for k1, r1 in recs:
        #         r1v = rule_index[r1]
        #         for k2, r2 in recs:
        #             r2v = rule_index[r2]
        #             if k1 < k2:
        #                 literals.append(lt(r1v, r2v))
        return new_handles, tuple(literals)


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



    # @profile
    def build_specialisation_constraint2(self, prog, spec_size=False):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        recs = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var

            if self.settings.single_solve:
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            else:
                is_rec = rule_is_recursive(rule)
                if is_rec:
                    recs.append((len(body), rule))
                handle = self.make_rule_handle(rule)
                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                    if is_rec:
                        literals.append(gteq(rule_var, 1))
                else:
                    xs = self.build_seen_rule2(rule, is_rec)
                    # NEW!!
                    # self.seen_handles.update(xs)
                    new_handles.update(xs)
                    literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(lt(rule_var, len(prog)))

        if not self.settings.single_solve:
            literals.append(Literal('clause', (len(prog), )))

        if spec_size:
            literals.append(Literal('program_size_at_least', (spec_size,)))

        # if rule_ordering:
        #     literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        # else:
        #     for k1, r1 in recs:
        #         r1v = rule_index[r1]
        #         for k2, r2 in recs:
        #             r2v = rule_index[r2]
        #             if k1 < k2:
        #                 literals.append(lt(r1v, r2v))
        return new_handles, tuple(literals)

    def build_banish_constraint(self, prog, rule_ordering=None):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        recs = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            if self.settings.single_solve:
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            else:
                is_rec = rule_is_recursive(rule)
                if is_rec:
                    recs.append((len(body), rule))
                handle = self.make_rule_handle(rule)
                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                    if is_rec:
                        literals.append(gteq(rule_var, 1))
                else:
                    xs = self.build_seen_rule2(rule, is_rec)
                    new_handles.update(xs)
                    literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(body_size_literal(rule_var, len(body)))
        literals.append(Literal('clause', (len(prog), )))

        # if rule_ordering:
        #     literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        # else:
        #     for k1, r1 in recs:
        #         r1v = rule_index[r1]
        #         for k2, r2 in recs:
        #             r2v = rule_index[r2]
        #             if k1 < k2:
        #                 literals.append(lt(r1v, r2v))
        return new_handles, tuple(literals)

    def andy_tmp_con(self, prog):
    # :-
    # seen_rule(fABfCBtailAC,R1),
    # seen_rule(fABfCBtailAC,R2),
    # R1 < R2,
    # body_size(R1,2).
        for rule in prog:
            if not rule_is_recursive(rule):
                continue
            head, body = rule
            handle = self.make_rule_handle(rule)
            if handle not in self.seen_handles:
                continue
            rule_var1 = vo_clause(1)
            rule_var2 = vo_clause(2)
            literals = []
            literals.append(build_seen_rule_literal(handle, rule_var1))
            literals.append(build_seen_rule_literal(handle, rule_var2))
            literals.append(lt(rule_var1, rule_var2))
            literals.append(gteq(rule_var1, 1))
            literals.append(body_size_literal(rule_var1, len(body)))
            yield tuple(literals)

    # only works with single rule programs
    # if a single rule R is unsatisfiable, then for R to appear in an optimal solution H it must be the case that H has a recursive rule that does not specialise R
    def redundancy_constraint1(self, prog):

        new_handles = set()
        literals = []

        rule_id = 0
        rule = list(prog)[0]
        head, body = rule
        rule_var = vo_clause(rule_id)
        handle = self.make_rule_handle(rule)
        if handle in self.seen_handles:
            literals.append(build_seen_rule_literal(handle, rule_var))
        else:
            xs = self.build_seen_rule2(rule, False)
            new_handles.update(xs)
            literals.extend(tuple(build_rule_literals(rule, rule_var)))

        # UNSURE
        # if self.settings.max_rules > 2:
        literals.append(gteq(rule_var, 1))
        literals.append(Literal('recursive_clause',(rule_var, head.predicate, len(head.arguments))))
        literals.append(Literal('num_recursive', (head.predicate, 1)))
        # else:
            # literals.append(gteq(rule_var, 1))

        return handle, new_handles, tuple(literals)

    def redundancy_constraint2(self, prog):
        # assert(Fa)

        lits_num_rules = defaultdict(int)
        lits_num_recursive_rules = defaultdict(int)
        for rule in prog:
            head, _ = rule
            lits_num_rules[head.predicate] += 1
            if rule_is_recursive(rule):
                lits_num_recursive_rules[head.predicate] += 1

        recursively_called = set()
        while True:
            something_added = False
            for rule in prog:
                head, body = rule
                is_rec = rule_is_recursive(rule)
                for body_literal in body:
                    if body_literal.predicate not in lits_num_rules:
                        continue
                    if (body_literal.predicate != head.predicate and is_rec) or (head.predicate in recursively_called):
                        something_added |= not body_literal.predicate in recursively_called
                        recursively_called.add(body_literal.predicate)
            if not something_added:
                break

        new_handles = set()
        out_cons = []
        for lit in lits_num_rules.keys() - recursively_called:
            rule_index = {}
            literals = []

            for rule_id, rule in enumerate(prog):
                head, body = rule
                rule_var = vo_clause(rule_id)
                rule_index[rule] = rule_var
                handle = self.make_rule_handle(rule)

                is_rec = rule_is_recursive(rule)
                if is_rec:
                    literals.append(gteq(rule_var, 1))
                else:
                    literals.append(lt(rule_var, 1))

                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                else:
                    xs = self.build_seen_rule2(rule, is_rec)
                    # NEW!!
                    # self.seen_handles.update(xs)
                    new_handles.update(xs)
                    literals.extend(tuple(build_rule_literals(rule, rule_var)))

            for other_lit, num_clauses in lits_num_rules.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_rules[lit]
            literals.append(Literal('num_recursive', (lit, num_recursive)))
            # if rule_ordering != None:
                # literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
            out_cons.append(tuple(literals))

            # print(':- ' + ', '.join(map(str,literals)))

        return new_handles, out_cons

    # def redundant_rules_check(self, rule1, rule2):-

    def unsat_constraint2(self, body):
        if len(self.settings.body_types) == 0:
            _, body = remap_variables((None, body))

        assignments = self.find_deep_bindings4(body)
        out = []
        # print(format_prog([(None, body)]))
        # :- empty(V3),tail(V3,V1).
        for rule_id in range(self.settings.max_rules):
            for assignment in assignments:
                # for i in range(self.settings.head_literal.arity):
                    # assignment[i] = i
                # print(assignment)
                rule = []
                for atom in body:
                    # print('\t', atom.arguments)
                    args2 = tuple(assignment[x] for x in atom.arguments)
                    rule.append((True, 'body_literal', (rule_id, atom.predicate, len(atom.arguments), args2)))
                out.append(frozenset(rule))
        return out

    # @profile
    def ground_literal(self, arguments, assignment, tmp):
        # print(arguments)
        k = hash((arguments, tmp))
        if k in self.cached_grounded:
            return self.cached_grounded[k]
        ground_args = []
        for arg in arguments:
            if isinstance(arg, tuple):
                ground_args.append(tuple(assignment[t_arg] for t_arg in arg))
            elif arg in assignment:
                ground_args.append(assignment[arg])
            else:
                ground_args.append(arg)
        ground_args = tuple(ground_args)
        self.cached_grounded[k] = ground_args
        return ground_args

    # @profile
    def ground_rule(self, rule, assignment):
        k = hash(frozenset(assignment.items()))
        head, body = rule
        ground_head = None
        if head:
            ground_args = self.ground_literal(head.arguments, assignment, k)
            ground_head = (True, head.predicate, ground_args)
        ground_body = set()
        for literal in body:
            ground_args = self.ground_literal(literal.arguments, assignment, k)
            # AC: NASTY TMP HACK!!!!!!!!
            # IN OUR CONSTRAINTS, WE ALWAYS USE `not clause`
            if literal.predicate == 'clause':
                ground_literal = (False, literal.predicate, ground_args)
            else:
                ground_literal = (True, literal.predicate, ground_args)
            ground_body.add(ground_literal)
        return ground_head, frozenset(ground_body)

    def find_bindings(self, body, all_vars):

        k = grounding_hash(body, all_vars)
        if k in self.seen_assignments:
            return self.seen_assignments[k]

        max_rules, max_vars = self.settings.max_rules, self.settings.max_vars

        # map each rule and var_var in the program to an integer
        rule_var_to_int = {v:i for i, v in enumerate(var for var in all_vars if isinstance(var, RuleVar))}

        # transpose for later lookup
        int_to_rule_var = {i:v for v,i in rule_var_to_int.items()}

        # find all variables for each rule
        rule_vars = {k:set() for k in rule_var_to_int}
        for var in all_vars:
            if isinstance(var, VarVar):
                rule_vars[var.rule].add(var)

        encoding = []
        encoding.append(BINDING_ENCODING)
        encoding.append(f'#const max_rules={max_rules}.')
        encoding.append(f'#const max_vars={max_vars}.')

        int_lookup = {}
        tmp_lookup = {}
        for rule_var, xs in rule_vars.items():
            rule_var_int = rule_var_to_int[rule_var]
            encoding.append(f'rule({rule_var_int}).')

            for var_var_int, var_var in enumerate(xs):
                encoding.append(f'rule_var({rule_var_int},{var_var_int}).')
                int_lookup[(rule_var_int, var_var_int)] = var_var
                tmp_lookup[(rule_var, var_var)] = var_var_int
                # rule_var_lookup[(rule_var, i)] = var
                # rule_var_to_int[var] = i

        # rule_var_lookup[(rule_var, i)] = var
        # rule_var_to_int[var] = i
        # add constraints to the ASP program based on the AST thing
        for lit in body:
            if not lit.predicate in META_PREDS:
                continue
            if lit.predicate == '==':
                # pass
                var, value = lit.arguments
                rule_var = var.rule
                rule_var_int = rule_var_to_int[rule_var]
                var_var_int = tmp_lookup[(rule_var, var)]
                encoding.append(f':- not bind_var({rule_var_int},{var_var_int},{value}).')
            elif lit.predicate == '>=':
                var, val = lit.arguments
                rule_var_int1 = rule_var_to_int[var]
                # var = c_vars[var]
                # for i in range(val):
                # encoding.append(f':- c_var({var},{i}).')
                encoding.append(f':- bind_rule({rule_var_int1},Val1), Val1 < {val}.')
            elif lit.predicate == '<':
                a, b = lit.arguments
                if isinstance(b, int):
                # ABSOLUTE HACK
                    rule_var_int1 = rule_var_to_int[a]
                    encoding.append(f':- bind_rule({rule_var_int1},Val1), Val1 >= {b}.')
                else:
                    rule_var_int1 = rule_var_to_int[a]
                    rule_var_int2 = rule_var_to_int[b]
                    encoding.append(f':- bind_rule({rule_var_int1},Val1), bind_rule({rule_var_int2},Val2), Val1>=Val2.')

        encoding = '\n'.join(encoding)

        # print(encoding)

        # print('ASDASDA')
        # solver = clingo.Control()
        solver = clingo.Control(['-Wnone'])
        # solver = clingo.Control(["-t4"])
        # ask for all models
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([("base", [])])

        out = []

        def on_model(m):
            xs = m.symbols(shown = True)
            # map a variable to a program variable
            # print('xs', xs)
            assignment = {}
            for x in xs:
                name = x.name
                args = x.arguments
                if name == 'bind_var':
                    rule_var_int = args[0].number
                    var_var_int = args[1].number
                    value = args[2].number
                    var_var = int_lookup[(rule_var_int, var_var_int)]
                    assignment[var_var] = value
                else:
                    rule_var_int = args[0].number
                    value = args[1].number
                    rule_var = int_to_rule_var[rule_var_int]
                    assignment[rule_var] = value
            out.append(assignment)
        solver.solve(on_model=on_model)
        self.seen_assignments[k] = out
        return out

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


BINDING_ENCODING = """\
#defined rule_var/2.
#show bind_rule/2.
#show bind_var/3.

% bind a rule_id to a value
{bind_rule(Rule,Value)}:-
    rule(Rule),
    Value=0..max_rules-1.
{bind_var(Rule,Var,Value)}:-
    rule_var(Rule,Var),
    Value=0..max_vars-1.

% every rule must be bound to exactly one value
:-
    rule(Rule),
    #count{Value: bind_rule(Rule,Value)} != 1.
% for each rule, each var must be bound to exactly one value
:-
    rule_var(Rule,Var),
    #count{Value: bind_var(Rule,Var,Value)} != 1.
% a rule value cannot be bound to more than one rule
:-
    Value=0..max_rules-1,
    #count{Rule : bind_rule(Rule,Value)} > 1.
% a var value cannot be bound to more than one var per rule
:-
    rule(Rule),
    Value=0..max_vars-1,
    #count{Var : bind_var(Rule,Var,Value)} > 1.
"""

def vo_variable2(rule, variable):
    key = f'{rule.name}_V{variable}'
    return VarVar(rule=rule, name=key)

def vo_clause(variable):
    return RuleVar(name=f'R{variable}')

def lt(a, b):
    return Literal('<', (a,b))

def eq(a, b):
    return Literal('==', (a,b))

def gteq(a, b):
    return Literal('>=', (a,b))

def body_size_literal(clause_var, body_size):
    return Literal('body_size', (clause_var, body_size))