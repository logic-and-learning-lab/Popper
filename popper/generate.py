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

def arg_to_symbol(arg):
    if isinstance(arg, tuple):
        return Tuple_(tuple(arg_to_symbol(a) for a in arg))
    if isinstance(arg, numbers.Number):
        return Number(arg)
    if isinstance(arg, str):
        return Function(arg)
    assert False, f'Unhandled argtype({type(arg)}) in aspsolver.py arg_to_symbol()'

def atom_to_symbol(pred, args):
    xs = tuple(arg_to_symbol(arg) for arg in args)
    return Function(name = pred, arguments = xs)

class Generator:
    def __init__(self, settings, bootstrap_cons):
        self.settings = settings
        # track size literals
        self.tracker = {}
        # track number of constraints added
        self.con_count = 0

        self.constrain = Constrain()
        self.grounder = ClingoGrounder()
        self.seen_symbols = {}


        prog = []
        with open('popper/lp/alan.pl') as f:
            prog.append(f.read())
        with open(settings.bias_file) as f:
            prog.append(f.read())

        prog = '\n'.join(prog)
        # with open('gen.pl', 'w') as f:
            # f.write(prog)
        # solver = clingo.Control(['-t6'])
        solver = clingo.Control()
        solver.add('base', [], prog)
        solver.ground([('base', [])])
        solver.add('number_of_literals', ['n'], NUM_LITERALS)
        self.solver = solver

        specs, elims, gens = bootstrap_cons
        cons = set()
        for prog in specs:
            cons.update(self.build_specialisation_constraint(prog))
        for prog in elims:
            cons.update(self.build_elimination_constraint(prog))
        for prog in gens:
            cons.update(self.build_generalisation_constraint(prog))
        self.add_constraints(cons)

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

    def gen_prog(self):
        with self.solver.solve(yield_ = True) as handle:
            m = handle.model()
            if m:
                atoms = m.symbols(shown = True)
                return self.parse_model(atoms)
        return None

    # TODO: COULD CACHE TUPLES OF ARGS FOR TINY OPTIMISATION
    def parse_model(self, model):
        # print('model', model)
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

        prog = []
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
            prog.append((rule))
        return frozenset(prog)

    def add_constraints(self, rules):
        out = set()
        for clause in rules:
            head, body = clause

            # find bindings for variables in the constraint
            assignments = self.grounder.find_bindings(clause, self.settings.max_rules, self.settings.max_vars)

            # keep only standard literals
            body = tuple(literal for literal in body if not literal.meta)

            # ground the clause for each variable assignment
            for assignment in assignments:
                out.add(Grounding.ground_clause((head, body), assignment))
        self.add_ground_rules(out)

    def gen_symbol(self, literal, backend):
        sign, pred, args = literal
        k = hash(literal)
        if k in self.seen_symbols:
            symbol = self.seen_symbols[k]
        else:
            symbol = backend.add_atom(atom_to_symbol(pred, args))
            self.seen_symbols[k] = symbol
        return symbol

    def add_ground_rules(self, rules):
        with self.solver.backend() as backend:
            for rule in rules:
                # print(format_constraint_rule(rule))
                head, body = rule
                head_literal = []
                if head:
                    head_literal = [self.gen_symbol(head, backend)]
                body_lits = []
                for literal in body:
                    sign, _pred, _args = literal
                    symbol = self.gen_symbol(literal, backend)
                    body_lits.append(symbol if sign else -symbol)
                backend.add_rule(head_literal, body_lits)

    def build_specialisation_constraint(self, prog):
        return self.constrain.specialisation_constraint(prog)

    def build_generalisation_constraint(self, prog):
        return self.constrain.generalisation_constraint(prog)

    def build_elimination_constraint(self, prog):
        return self.constrain.banish_constraint(prog)

def format_literal(literal):
    sign, pred, args = literal
    out = ''
    if sign == False:
        out = 'not '
    arg_str = ','.join(str(arg) for arg in args)
    return out + f'{pred}({arg_str})'

def format_constraint_rule(rule):
    # print('---')
    # print(rule)
    head, body = rule
    rule_str = ''
    if head:
        rule_str = format_literal(head)
    rule_str += ':- ' + ','.join(format_literal(literal) for literal in body) + '.'
    return rule_str

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

NUM_LITERALS = """
%%% External atom for number of literals in the program %%%%%
#external size_in_literals(n).
:-
    size_in_literals(n),
    #sum{K+1,Rule : body_size(Rule,K)} != n.
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

# def specialisation_constraint(prog):
#     # TMP!
#     rule = list(prog)[0]
#     literals = []
#     head, body = rule
#     literals.append(Literal('head_literal', (0, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
#     for body_literal in body:
#         literals.append(Literal('body_literal', (0, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
#     xs = find_all_vars(body)
#     for v1,v2 in itertools.combinations(xs, 2):
#         vo_variable(v1),
#         vo_variable(v2),
#         literals.append(Literal('!=', (v1,v2), meta=True))
#     return None, tuple(literals)

# def elimination_constraint(prog):
#     # TMP!
#     rule = list(prog)[0]
#     literals = []
#     head, body = rule
#     literals.append(Literal('head_literal', (0, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
#     for body_literal in body:
#         literals.append(Literal('body_literal', (0, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
#     literals.append(Literal('body_size', (0,len(body),)))
#     xs = find_all_vars(body)

#     for v1,v2 in itertools.combinations(xs, 2):
#         vo_variable(v1),
#         vo_variable(v2),
#         literals.append(Literal('!=', (v1,v2), meta=True))
#     return None, tuple(literals)

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


import operator
from collections import defaultdict
import pkg_resources
from . core import *
from clingo import Function, Number, Tuple_
import clingo.script
clingo.script.enable_python()

# GROUND_BOUNDS = pkg_resources.resource_string(__name__, "lp/ground-bounds.pl").decode()

def alldiff(args):
    return Literal('AllDifferent', args, meta=True)

def lt(a, b):
    return Literal('<', (a,b), meta=True)

def eq(a, b):
    return Literal('==', (a,b), meta=True)

def gteq(a, b):
    return Literal('>=', (a,b), meta=True)

def vo_clause(variable):
    return ConstVar(f'C{variable}', 'Clause')

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

def body_size_literal(clause_var, body_size):
    return Literal('body_size', (clause_var, body_size))

def make_literal_handle(literal):
    return f'{literal.predicate}({".".join(literal.arguments)})'

def deduce_min_rule(rule):
    if rule_is_invented(rule):
        head, body = rule
        # inv symbols are inv1, inv2, etc ...
        min_rule = int(head.predicate[-1])
        if rule_is_recursive(rule):
            min_rule += 1
        return min_rule

    if rule_is_recursive(rule):
        return 1

    return 0

def deduce_ordering(ordered_rules):
    if len(ordered_rules) == 1:
        return []

    prog = []
    for i, rule in enumerate(ordered_rules):
        head, body = rule
        level = 0
        if rule_is_invented(rule):
            level = int(head.predicate[-1])
        x = f'rule({i},{head.predicate},{len(body)},{int(rule_is_recursive(rule))},{level}). '
        prog.append(x)
    prog.append(GROUND_BOUNDS)
    # TODO: CHECK THESE VALUES IN CACHE!!!
    prog = '\n'.join(prog)
    solver = clingo.Control()
    solver.add('base', [], prog)
    solver.ground([("base", [])])
    out = []
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown=True):
                a = atom.arguments[0]
                b = atom.arguments[1]
                out.append((a,b))
    return out

import operator
from collections import defaultdict
from . core import ConstVar, Literal

def alldiff(args):
    return Literal('AllDifferent', args, meta=True)

def lt(a, b):
    return Literal('<', (a,b), meta=True)

def eq(a, b):
    return Literal('==', (a,b), meta=True)

def gteq(a, b):
    return Literal('>=', (a,b), meta=True)

def vo_clause(variable):
    return ConstVar(f'C{variable}', 'Clause')

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

# restrict a clause id to have a specific body size
def body_size_literal(clause_var, body_size):
    return Literal('body_size', (clause_var, body_size))

class Constrain:
    def __init__(self):
        self.seen_clause_handle = {}
        self.added_clauses = set()

    def all_vars(self, clause):
        (head, body) = clause
        xs = set()
        if head:
            xs.update(head.arguments)
        for literal in body:
            for arg in literal.arguments:
                if isinstance(arg, ConstVar):
                    xs.add(arg)
                elif isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            xs.add(t_arg)
        return xs

    def make_literal_handle(self, literal):
        return f'{literal.predicate}{"".join(literal.arguments)}'

    def make_clause_handle(self, clause):
        if clause in self.seen_clause_handle:
            return self.seen_clause_handle[clause]
        (head, body) = clause
        body_literals = sorted(body, key = operator.attrgetter('predicate'))
        clause_handle = ''.join(self.make_literal_handle(literal) for literal in [head] + body_literals)
        self.seen_clause_handle[clause] = clause_handle
        return clause_handle

    def make_clause_inclusion_rule(self, clause, min_num, clause_handle):
        if clause_handle in self.added_clauses:
            return
            yield

        head, body = clause

        self.added_clauses.add(clause_handle)
        clause_number = vo_clause('l')

        literals = []
        literals.append(Literal('head_literal', (clause_number, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))

        for body_literal in body:
            literals.append(Literal('body_literal', (clause_number, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))

        literals.append(gteq(clause_number, min_num))

        # ensure that each var_var is ground to a unique value
        literals.append(alldiff(tuple(vo_variable(v) for v in self.all_vars(clause))))

        for idx, var in enumerate(head.arguments):
            literals.append(eq(vo_variable(var), idx))

        yield (Literal('included_clause', (clause_handle, clause_number)), tuple(literals))

    def banish_constraint(self, program, before={}, min_clause=defaultdict(int)):
        literals = []
        for clause_number, clause in enumerate(program):
            head, body = clause
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause, min_clause[clause_number], clause_handle)

            literals.append(Literal('included_clause', (clause_handle, vo_clause(clause_number))))
            literals.append(body_size_literal(vo_clause(clause_number), len(body)))

        for clause_number1, clause_numbers in before.items():
            for clause_number2 in clause_numbers:
                literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

        for clause_number, clause in enumerate(program):
            literals.append(gteq(vo_clause(clause_number), min_clause[clause]))

        num_clauses = len(program)
        # ensure that each clause_var is ground to a unique value
        literals.append(alldiff(tuple(vo_clause(c) for c in range(num_clauses))))
        literals.append(Literal('clause', (num_clauses, ), positive = False))

        yield (None, tuple(literals))

    def generalisation_constraint(self, program, before={}, min_clause=defaultdict(int)):
        literals = []
        for clause_number, clause in enumerate(program):
            (_head, body) = clause
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause,  min_clause[clause], clause_handle)

            literals.append(Literal('included_clause', (clause_handle, vo_clause(clause_number))))
            literals.append(body_size_literal(vo_clause(clause_number), len(body)))

        for clause_number1, clause_numbers in before.items():
            for clause_number2 in clause_numbers:
                literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

        for clause_number, clause in enumerate(program):
            literals.append(gteq(vo_clause(clause_number), min_clause[clause]))

        # ensure that each clause_var is ground to a unique value
        literals.append(alldiff(tuple(vo_clause(c) for c in range(len(program)))))

        yield (None, tuple(literals))

    # def specialisation_constraint_old(self, program, before={}, min_clause=defaultdict(int)):
    #     literals = []

    #     for clause_number, clause in enumerate(program):
    #         clause_handle = self.make_clause_handle(clause)
    #         yield from self.make_clause_inclusion_rule(clause, min_clause[clause], clause_handle)
    #         clause_variable = vo_clause(clause_number)
    #         literals.append(Literal('included_clause', (clause_handle, clause_variable)))

    #     for clause_number1, clause_numbers in before.items():
    #         for clause_number2 in clause_numbers:
    #             literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

    #     num_clauses = len(program)
    #     # ensure that each clause_var is ground to a unique value
    #     literals.append(alldiff(tuple(vo_clause(c) for c in range(num_clauses))))
    #     literals.append(Literal('clause', (num_clauses, ), positive = False))

    #     yield (None, tuple(literals))


    def specialisation_constraint(self, program, before={}, min_clause=defaultdict(int)):
        clause_variable = vo_clause('')
        literals = [Literal('clause', (clause_variable,))]
        prog_handle = "prog"
        for clause in program:
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause, min_clause[clause], clause_handle)
            literals.append(Literal('included_clause', (clause_handle, clause_variable), positive = False))
            prog_handle += '_' + clause_handle

        yield (Literal('clause_not_spec', (prog_handle,)), tuple(literals))
        yield (None, (Literal('clause_not_spec', (prog_handle,), positive = False),))


    # def specialisation_constraint(self, rules):
    #     literals = []
    #     ordered_rules = sorted(rules)

    #     for clause_number, clause in enumerate(ordered_rules):
    #         clause_handle = self.make_rule_handle(clause)
    #         # yield from self.make_clause_inclusion_rule(clause, min_clause[clause], clause_handle)
    #         yield from self.make_clause_inclusion_rule(clause, clause_handle)
    #         clause_var = vo_clause(clause_number)
    #         literals.append(Literal('included_clause', (clause_handle, clause_var)))
    #         literals.append(self.min_rule_literal(clause, clause_var))

    #     literals.extend(self.before_literals(ordered_rules))

    #     num_clauses = len(rules)
    #     # ensure that each clause_var is ground to a unique value
    #     if num_clauses > 1:
    #         literals.append(alldiff(tuple(vo_clause(c) for c in range(num_clauses))))
    #     literals.append(Literal('clause', (num_clauses, ), positive = False))

    #     yield (None, tuple(literals))

    # AC: THIS CONSTRAINT DUPLICATES THE GENERALISATION CONSTRAINT AND NEEDS REFACTORING
    def redundant_literal_constraint(self, clause, before, min_clause):
        (_head, body) = clause
        clause_handle = self.make_clause_handle(clause)
        yield from self.make_clause_inclusion_rule(clause, min_clause[clause], clause_handle)
        literals = []
        clause_variable = vo_clause(0)
        literals.append(Literal('included_clause', (clause_handle, clause_variable)))
        literals.append(body_size_literal(clause_variable, len(body)))
        yield (None, tuple(literals))

    # Jk: AC, I cleaned this up a bit, but this reorg is for you. Godspeed!
    # AC: @JK, I made another pass through it. It was tough. I will try again once we have the whole codebase tidied.
    def redundancy_constraint(self, program, before, min_clause):
        lits_num_clauses = defaultdict(int)
        lits_num_recursive_clauses = defaultdict(int)
        for clause in program:
            (head, _) = clause
            lits_num_clauses[head.predicate] += 1
            if Clause.is_recursive(clause):
                lits_num_recursive_clauses[head.predicate] += 1

        recursively_called = set()
        while True:
            something_added = False
            for clause in program:
                (head, body) = clause
                is_rec = Clause.is_recursive(clause)
                for body_literal in body:
                    if body_literal.predicate not in lits_num_clauses:
                        continue
                    if (body_literal.predicate != head.predicate and is_rec) or (head.predicate in recursively_called):
                        something_added |= not body_literal.predicate in recursively_called
                        recursively_called.add(body_literal.predicate)
            if not something_added:
                break

        for lit in lits_num_clauses.keys() - recursively_called:
            literals = []

            for clause_number, clause in enumerate(program):
                clause_handle = self.make_clause_handle(clause)
                yield from self.make_clause_inclusion_rule(clause, min_clause[clause], clause_handle)
                clause_variable = vo_clause(clause_number)
                literals.append(Literal('included_clause', (clause_handle, clause_variable)))

            for clause_number1, clause_numbers in before.items():
                for clause_number2 in clause_numbers:
                    literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

            # ensure that each clause_var is ground to a unique value
            literals.append(alldiff(tuple(vo_clause(c) for c in range(len(program)))))

            for other_lit, num_clauses in lits_num_clauses.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_clauses[lit]

            literals.append(Literal('num_recursive', (lit, num_recursive)))

            yield (None, tuple(literals))

    @staticmethod
    def format_constraint(con):
        (head, body) = con
        constraint_literals = []
        for constobj in body:
            if not constobj.meta:
                constraint_literals.append(str(constobj))
                continue
            if constobj.predicate == 'AllDifferent':
                # AC: TODO!!!
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


class ClingoGrounder():
    def __init__(self):
        self.seen_assignments = {}

    def find_bindings(self, clause, max_clauses, max_vars):
        (_, body) = clause
        all_vars = Grounding.find_all_vars(body)
        if len(all_vars) == 0:
            return [{}]

        k = Grounding.grounding_hash(body, all_vars)
        if k in self.seen_assignments:
            return self.seen_assignments[k]

        # map each clause_var and var_var in the program to an integer
        c_vars = {v:i for i,v in enumerate(var for var in all_vars if var.type == 'Clause')}
        v_vars = {v:i for i,v in enumerate(var for var in all_vars if var.type == 'Variable')}

        # transpose for return lookup
        c_vars_ = {v:k for k,v in c_vars.items()}
        v_vars_ = {v:k for k,v in v_vars.items()}

        c_var_count = len(c_vars)
        v_var_count = len(v_vars)
        if c_var_count == 0 and v_var_count == 0:
            return [{}]

        solver = clingo.Control()

        # ask for all models
        solver.configuration.solve.models = 0

        # add the base reasoning
        solver.add('base', [], """\
            #show v_var/2.
            #show c_var/2.
            c_val(0..num_c_vals-1).
            v_val(0..num_v_vals-1).
            1 {c_var(V,X): c_val(X)} 1:- V=0..num_c_vars-1.
            1 {v_var(V,X): v_val(X)} 1:- V=0..num_v_vars-1.
            :- c_val(X), #count{I : c_var(I,X)} > 1.
            :- v_val(X), #count{I : v_var(I,X)} > 1."""
            +
            f"""\
            #const num_c_vars={c_var_count}.
            #const num_c_vals={max_clauses}.
            #const num_v_vars={v_var_count}.
            #const num_v_vals={max_vars}.
        """)

        # add constraints to the ASP program based on the AST thing
        for lit in body:
            if not lit.meta:
                continue
            if lit.predicate == '==':
                var, val = lit.arguments
                var = v_vars[var]
                solver.add('base', [], f':- not v_var({var},{val}).')
            elif lit.predicate == '>=':
                var, val = lit.arguments
                var = c_vars[var]
                for i in range(val):
                    solver.add('base', [], f':- c_var({var},{i}).')
            elif lit.predicate == '<':
                var1 = c_vars[lit.arguments[0]]
                var2 = c_vars[lit.arguments[1]]
                solver.add('base', [], f':- c_var({var1},Val1), c_var({var2},Val2), Val1>=Val2.')

        solver.ground([("base", [])])

        out = []

        def on_model(m):
            xs = m.symbols(shown = True)
            # map a variable to a program variable
            assignment = {}
            for x in xs:
                var = x.arguments[0].number
                val = x.arguments[1].number
                if x.name == 'c_var':
                    assignment[c_vars_[var]] = val
                elif x.name == 'v_var':
                    assignment[v_vars_[var]] = val
            out.append(assignment)

        solver.solve(on_model=on_model)
        self.seen_assignments[k] = out
        return out

class Grounding:
    @staticmethod
    # IMPROVE/REFACTOR
    def ground_literal(literal, assignment):
        ground_args = []
        for arg in literal.arguments:
            if arg in assignment:
                ground_args.append(assignment[arg])
            # handles tuples of ConstVars
            # TODO: AC: EXPLAIN BETTER
            elif isinstance(arg, tuple):
                ground_t_args = []
                # AC: really messy
                for t_arg in arg:
                    if t_arg in assignment:
                        ground_t_args.append(assignment[t_arg])
                    else:
                        ground_t_args.append(t_arg)
                ground_args.append(tuple(ground_t_args))
            else:
                ground_args.append(arg)
        return (literal.positive, literal.predicate, tuple(ground_args))

    @staticmethod
    def ground_clause(clause, assignment):
        (head, body) = clause
        ground_head = None
        if head:
            ground_head = Grounding.ground_literal(head, assignment)
        ground_body = frozenset(Grounding.ground_literal(literal, assignment) for literal in body)
        return (ground_head, ground_body)

    # AC: When grounding constraint rules, we only care about the vars and the constraints, not the actual literals
    @staticmethod
    def grounding_hash(body, all_vars):
        cons = set()
        for lit in body:
            if lit.meta:
                cons.add((lit.predicate, lit.arguments))
        return hash((frozenset(all_vars), frozenset(cons)))

    @staticmethod
    def find_all_vars(body):
        all_vars = set()
        for literal in body:
            for arg in literal.arguments:
                if isinstance(arg, ConstVar):
                    all_vars.add(arg)
                elif isinstance(arg, tuple):
                    for t_arg in arg:
                        if isinstance(t_arg, ConstVar):
                            all_vars.add(t_arg)
        return all_vars