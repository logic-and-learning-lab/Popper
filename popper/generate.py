import time
import clingo
import clingo.script
import numbers
import itertools
import operator
import pkg_resources
from . core import Literal, ConstVar
from . util import format_rule
from collections import defaultdict
from clingo import Function, Number, Tuple_
import clingo.script
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
    def __init__(self, settings, grounder, bootstrap_cons):
        self.settings = settings
        self.last_size = None

        self.constrain = Constrain()
        self.grounder = grounder
        self.seen_symbols = {}

        # build generator program
        prog = []
        with open('popper/lp/alan.pl') as f:
            prog.append(f.read())
        with open(settings.bias_file) as f:
            prog.append(f.read())
        prog.append(f'max_clauses({settings.max_rules}).')
        prog.append(f'max_body({settings.max_body}).')
        prog.append(f'max_vars({settings.max_vars}).')
        prog = '\n'.join(prog)

        # build solver
        solver = clingo.Control()
        solver.add('base', [], prog)
        solver.ground([('base', [])])
        solver.add('number_of_literals', ['n'], NUM_LITERALS)
        self.solver = solver

        # bootstrap with constraints
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
        # release atoms already assigned
        if self.last_size != None:
            symbol = clingo.Function('size_in_literals', [clingo.Number(self.last_size)])
            self.solver.release_external(symbol)
        # set new size
        self.last_size = size
        # ground new size
        self.solver.ground([('number_of_literals', [clingo.Number(size)])])
        # assign new size
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
                arity = len(atom_args)
                body_literal = (predicate, atom_args, arity)
                rule_index_to_body[rule_index].add(body_literal)

            elif atom.name == 'head_literal':
                rule_index = args[0].number
                predicate = args[1].name
                atom_args = args[3].arguments
                atom_args = tuple(arg_lookup[arg] for arg in atom_args)
                arity = len(atom_args)
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
            head_pred, head_args, head_arity = rule_index_to_head[rule_index]
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

    # cached_groundings = {}

    def add_constraints(self, rules):
        ground_rules = set()
        for rule in rules:
            # if rule in self.cached_groundings:
                # ground_rules.update(self.cached_groundings[rule])
                # continue
            head, body = rule

            # find bindings for variables in the rule
            assignments = self.grounder.find_bindings(rule, self.settings.max_rules, self.settings.max_vars)

            # keep only standard literals
            body = tuple(literal for literal in body if not literal.meta)

            # ground the rule for each variable assignment
            xs = set(self.grounder.ground_rule((head, body), assignment) for assignment in assignments)
            ground_rules.update(xs)
            # self.cached_groundings[rule] = xs
        self.add_ground_rules(ground_rules)

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

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

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
        # literals.append(alldiff(tuple(vo_variable(v) for v in self.all_vars(clause))))

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
        # literals.append(alldiff(tuple(vo_clause(c) for c in range(num_clauses))))
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
        # literals.append(alldiff(tuple(vo_clause(c) for c in range(len(program)))))

        yield (None, tuple(literals))

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


class Grounder():
    def __init__(self):
        self.seen_assignments = {}

    def find_bindings(self, clause, max_clauses, max_vars):
        _, body = clause
        all_vars = self.find_all_vars(body)
        if len(all_vars) == 0:
            return [{}]

        k = self.grounding_hash(body, all_vars)
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

    def ground_literal(self, literal, assignment):
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
        return literal.positive, literal.predicate, tuple(ground_args)

    def ground_rule(self, rule, assignment):
        head, body = rule
        ground_head = None
        if head:
            ground_head = self.ground_literal(head, assignment)
        ground_body = frozenset(self.ground_literal(literal, assignment) for literal in body)
        return ground_head, ground_body

    # AC: When grounding constraint rules, we only care about the vars and the constraints, not the actual literals
    def grounding_hash(self, body, all_vars):
        cons = set()
        for lit in body:
            if lit.meta:
                cons.add((lit.predicate, lit.arguments))
        return hash((frozenset(all_vars), frozenset(cons)))

    def find_all_vars(self, body):
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