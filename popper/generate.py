import time
import clingo
import clingo.script
import numbers
import itertools
from . core import Literal, ConstVar
from . util import format_rule
from collections import defaultdict
# from . constrain import format_constraint, find_all_vars
from clingo import Function, Number, Tuple_
clingo.script.enable_python()

arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}

class Generator:
    def __init__(self, settings, bootstrap_cons):
        self.settings = settings
        # track size literals
        self.tracker = {}
        # track number of constraints added
        self.con_count = 0

        prog = []
        with open('popper/lp/alan.pl') as f:
            prog.append(f.read())
        with open(settings.bias_file) as f:
            prog.append(f.read())

        # add bootstap constraints
        for con in bootstrap_cons:
            prog.append(format_constraint(con))

        prog = '\n'.join(prog)

        # with open('gen.pl', 'w') as f:
            # f.write(prog)
        solver = clingo.Control()
        solver.add('base', [], prog)
        solver.ground([('base', [])])
        solver.add('number_of_literals', ['n'], NUM_LITERALS)

        self.solver = solver

    def update_num_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.tracker.items():
            if atom[0] == 'size_in_literals' and truth_value:
                self.tracker[atom] = False
                symbol = clingo.Function('size_in_literals', [clingo.Number(atom[1])])
                self.solver.release_external(symbol)
        self.solver.ground([('number_of_literals', [clingo.Number(size)])])
        self.tracker[('size_in_literals', size)] = True
        symbol = clingo.Function('size_in_literals', [clingo.Number(size)])
        self.solver.assign_external(symbol, True)

    # con_count = 0
    def gen_prog(self):
        with self.solver.solve(yield_ = True) as handle:
            m = handle.model()
            if m:
                atoms = m.symbols(shown = True)
                return self.parse_model(atoms)

    # TODO: COULD CACHE TUPLES OF ARGS FOR TINY OPTIMISATION
    # def parse_model(self, atoms):
    #     body_atoms = []
    #     for atom in atoms:
    #         atom_args = atom.arguments
    #         pred = atom_args[0].name
    #         args = atom_args[2].arguments
    #         args = tuple(arg_lookup[arg] for arg in args)
    #         literal = Literal(pred, args, [])
    #         body_atoms.append(literal)
    #     return self.settings.head_literal, frozenset(body_atoms)

    # TODO: COULD CACHE TUPLES OF ARGS FOR TINY OPTIMISATION
    def parse_model(self, model):
        directions = defaultdict(lambda: defaultdict(lambda: '?'))
        rule_index_to_body = defaultdict(set)
        rule_index_to_head = {}

        for atom in model:
            args = atom.arguments

            if atom.name == 'body_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = tuple(arg_lookup[arg] for arg in atom_args)
                # arguments = gen_args(args[3].arguments)
                arity = len(atom_args)
                body_literal = (predicate, atom_args, arity)
                rule_index_to_body[rule_index].add(body_literal)

            elif atom.name == 'head_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = tuple(arg_lookup[arg] for arg in atom_args)
                # arguments = args[3].arguments
                arity = len(atom_args)
                # arguments = gen_args(arguments)
                head_literal = (predicate, atom_args, arity)
                rule_index_to_head[rule_index] = head_literal

            elif atom.name == 'direction_':
                pred_name = args[0].name
                arg_index = args[1].number
                arg_dir_str = args[2].name

                if arg_dir_str == 'in':
                    arg_dir = '+'
                elif arg_dir_str == 'out':
                    arg_dir = '-'
                else:
                    raise Exception(f'Unrecognised argument direction "{arg_dir_str}"')
                directions[pred_name][arg_index] = arg_dir

        rules = []
        for rule_index in rule_index_to_head:
            (head_pred, head_args, head_arity) = rule_index_to_head[rule_index]
            head_modes = tuple(directions[head_pred][i] for i in range(head_arity))
            head = Literal(head_pred, head_args, head_modes)

            body = set()
            for (body_pred, body_args, body_arity) in rule_index_to_body[rule_index]:
                body_modes = tuple(directions[body_pred][i] for i in range(body_arity))
                body.add(Literal(body_pred, body_args, body_modes))
            body = frozenset(body)
            rule = head, body
            return rule
            # rules.append(rule)
        # rule = rules[0]
        # return self.settings.head_literal, frozenset(body_atoms)
        # return frozenset(rules)
    def add_constraint(self, con):
        self.con_count+=1
        k = f'cons_{self.con_count}'
        self.solver.add(k, [], format_constraint(con))
        self.solver.ground([(k, [])])


NUM_LITERALS = """
%%% External atom for number of literals in the program %%%%%
#external size_in_literals(n).
:-
    size_in_literals(n),
    #sum{K,Rule : body_size(Rule,K)} != n.
"""


class Constrainer:
    def __init__(self, settings):
        self.elim_cons = set()
        self.gen_cons = set()
        self.spec_cons = {x: set() for x in settings.pos}

    def add_elimination(self, con):
        self.elim_cons.add(con)

    def add_specialisation(self, con, e):
        self.spec_cons[e].add(con)

def find_all_vars(body):
    all_vars = set()
    for literal in body:
        for arg in literal.arguments:
            all_vars.add(arg)
            if isinstance(arg, ConstVar):
                all_vars.add(arg)
            if isinstance(arg, tuple):
                for t_arg in arg:
                    # if isinstance(t_arg, ConstVar):
                    all_vars.add(t_arg)
    return sorted(all_vars)

def specialisation_constraint(rule):
    literals = []
    head, body = rule
    literals.append(Literal('head_literal', (0, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
    for body_literal in body:
        literals.append(Literal('body_literal', (0, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
    xs = find_all_vars(body)
    for v1,v2 in itertools.combinations(xs, 2):
        vo_variable(v1),
        vo_variable(v2),
        literals.append(Literal('!=', (v1,v2), meta=True))
    return None, tuple(literals)

def elimination_constraint(rule):
    literals = []
    head, body = rule
    literals.append(Literal('head_literal', (0, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
    for body_literal in body:
        literals.append(Literal('body_literal', (0, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
    literals.append(Literal('body_size', (0,len(body),)))
    xs = find_all_vars(body)

    for v1,v2 in itertools.combinations(xs, 2):
        vo_variable(v1),
        vo_variable(v2),
        literals.append(Literal('!=', (v1,v2), meta=True))
    return None, tuple(literals)

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

# def format_constraint(con):
#     head, body = con
#     constraint_literals = []
#     for constobj in body:
#         if not constobj.meta:
#             constraint_literals.append(str(constobj))
#             continue
#         arga, argb = constobj.arguments
#         if isinstance(arga, ConstVar):
#             arga = arga.name
#         else:
#             arga = str(arga)
#         if isinstance(argb, ConstVar):
#             argb = argb.name
#         else:
#             argb = str(argb)
#         constraint_literals.append(f'{arga}{constobj.predicate}{argb}')

#     x = f':- {", ".join(constraint_literals)}.'
#     if head:
#         x = f'{head} {x}'
#     return x

def format_constraint(con):
    head, body = con
    constraint_literals = []
    for constobj in body:
        if not constobj.meta:
            constraint_literals.append(str(constobj))
            continue
        arga, argb = constobj.arguments
        if isinstance(arga, ConstVar):
            arga = arga.name
        else:
            arga = str(arga)
        if isinstance(argb, ConstVar):
            argb = argb.name
        else:
            argb = str(argb)
        constraint_literals.append(f'{arga}{constobj.predicate}{argb}')

    x = f':- {", ".join(constraint_literals)}.'
    if head:
        x = f'{head} {x}'
    return x
