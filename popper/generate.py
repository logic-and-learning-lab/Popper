import time
import clingo
import clingo.script
import numbers
from . core import Literal
from . util import format_rule
from . constrain import format_constraint, find_all_vars
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

        solver = clingo.Control(["-t1"]) #

        with open('popper/lp/alan.pl') as f:
            solver.add('base', [], f.read())
        with open(settings.bias_file) as f:
            solver.add('base', [], f.read())

        # add bootstap constraints
        cons = '\n'.join(format_constraint(con) for con in bootstrap_cons)
        solver.add('base', [], cons)
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
    def parse_model(self, atoms):
        body_atoms = []
        for atom in atoms:
            atom_args = atom.arguments
            pred = atom_args[0].name
            args = atom_args[2].arguments
            args = tuple(arg_lookup[arg] for arg in args)
            literal = Literal(pred, args, [])
            body_atoms.append(literal)
        return self.settings.head_literal, frozenset(body_atoms)

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
    #count{P,Vars : body_literal(P,_,Vars)} != n.
"""






# # def gen_symbol(pred, args, backend):
# #     # sign, pred, args = literal
# #     # k = hash(literal)
# #     # if k in self.seen_symbols:
# #         # symbol = self.seen_symbols[k]
# #     # else:

# #         # self.seen_symbols[k] = symbol
# #     return symbol

# def arg_to_symbol(arg):
#     # print(arg)
#     if isinstance(arg, tuple):
#         return Tuple_(tuple(arg_to_symbol(a) for a in arg))
#     if isinstance(arg, numbers.Number):
#         return Number(arg)
#     if isinstance(arg, str):
#         return Function(arg)
#     # if isinstance(arg, Function):
#         # return arg
#     # return arg
#     # print(arg,type(arg))
#     assert False, f'Unhandled argtype() in aspsolver.py arg_to_symbol()'

# def atom_to_symbol(pred, args):
#     xs = tuple(arg_to_symbol(arg) for arg in args)
#     x = Function(name = pred, arguments = xs)
#     return x

# def apply_constraints_(solver, rules):
#     seen_symbols = {}
#     xs = set()

#     t1 = time.time()
#     for rule in rules:
#         head, body = rule
#         # print('fo', head, body)
#         bindings = find_bindings(rule, max_vars=7)
#         for binding in bindings:
#             r = ground_rule(rule, binding)
#             xs.add(r)
#     t2 = time.time()
#     # print('A',t2-t1)

#     t1 = time.time()
#     with solver.backend() as backend:
#         for head, body in xs:
#             # head, body = rule
#             # print('ground', sorted(list(body)))
#             head_literal = []
#             # if head:
#                 # head_literal = [gen_symbol(head, backend)]
#             body_lits = []
#             for literal in body:
#                 sign, pred, args = literal

#                 tmp = atom_to_symbol(pred, args)

#                 # if tmp in seen_symbols:
#                     # symbol = seen_symbols[tmp]
#                 # else:
#                 symbol = backend.add_atom(tmp)
#                     # seen_symbols[tmp] = symbol
#                 # print(symbol)
#                 # print('x2',symbol)
#                 # (sign, _pred, _args) = literal
#                 # symbol = gen_symbol(literal, backend)
#                 # print(sign, )
#                 body_lits.append(symbol if sign else -symbol)
#             # print(body)
#             # print(head_literal, body_lits)
#             backend.add_rule(head_literal, body_lits)
#     t2 = time.time()
#     # print('B',t2-t1)

# def ground_rule(rule, assignment):
#     out = set()
#     head, body = rule
#     body = tuple(literal for literal in body if not literal.meta)
#     ground_head = None
#     if head:
#         ground_head = ground_literal(head, assignment)
#     ground_body = frozenset(ground_literal(literal, assignment) for literal in body)
#     return ground_head, ground_body

# def ground_literal(literal, assignment):
#     # print('ground_literal',literal)
#     ground_args = []
#     for arg in literal.arguments:
#         if isinstance(arg, tuple):
#             ground_t_args = []
#             for t_arg in arg:
#                 if t_arg in assignment:
#                     ground_t_args.append(assignment[t_arg])
#                 else:
#                     ground_t_args.append(t_arg)
#             ground_args.append(tuple(ground_t_args))
#         else:
#             ground_args.append(arg)
#     return literal.positive, literal.predicate, tuple(ground_args)

# def find_bindings(rule, max_vars=7):
#     head, body = rule
#     all_vars = find_all_vars(body)
#     if len(all_vars) == 0:
#         return [{}]

#     # print('head',head)
#     # exit()

#     # print('all_vars',all_vars)

#     v_vars = {v:i for i,v in enumerate(var for var in all_vars if var.type == 'Variable')}
#     v_vars_ = {v:k for k,v in v_vars.items()}

#     v_var_count = len(v_vars)
#     if v_var_count == 0:
#         return [{}]

#     solver = clingo.Control()

#     # ask for all models
#     solver.configuration.solve.models = 0

#     # add the base reasoning
#     solver.add('base', [], """\
#         #show v_var/2.
#         :- not v_var(0,0).
#         :- not v_var(1,1).
#         v_val(0..num_v_vals-1).
#         1 {v_var(V,X): v_val(X)} 1:- V=0..num_v_vars-1.
#         :- v_val(X), #count{I : v_var(I,X)} > 1."""
#         +
#         f"""\
#         #const num_v_vars={v_var_count}.
#         #const num_v_vals={max_vars}.
#     """)

#     # add constraints to the ASP program based on the AST thing
#     # for lit in body:
#     #     if not lit.meta:
#     #         continue
#     #     if lit.predicate == '==':
#     #         var, val = lit.arguments
#     #         var = v_vars[var]
#     #         solver.add('base', [], f':- not v_var({var},{val}).')

#     solver.ground([("base", [])])

#     out = []

#     def on_model(m):
#         xs = m.symbols(shown = True)
#         # print(xs)
#         # map a variable to a program variable
#         assignment = {}
#         for x in xs:
#             var = x.arguments[0].number
#             val = x.arguments[1].number
#             # print('var',var,'val',val)
#             if x.name == 'v_var':
#                 assignment[v_vars_[var]] = val
#         # print(assignment)
#         out.append(assignment)

#     solver.solve(on_model=on_model)
#     return out
