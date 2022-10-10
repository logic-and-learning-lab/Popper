import clingo
import operator
import clingo.script
import pkg_resources
from . core import Literal, RuleVar, VarVar, Var
from collections import defaultdict
from . util import rule_is_recursive
clingo.script.enable_python()

arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}

def find_all_vars(body):
    # print(','.join(str(x) for x in body))
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
    cons = set()
    for lit in body:
        if lit.meta:
            cons.add((lit.predicate, lit.arguments))
    return hash((frozenset(all_vars), frozenset(cons)))

cached_grounded = {}
def ground_literal(literal, assignment, tmp):
    k = hash((literal.predicate, literal.arguments, tmp))
    if k in cached_grounded:
        v = cached_grounded[k]
        return literal.positive, literal.predicate, v
    ground_args = []
    for arg in literal.arguments:
        if isinstance(arg, tuple):
            ground_args.append(tuple(assignment[t_arg] for t_arg in arg))
        elif arg in assignment:
            ground_args.append(assignment[arg])
        else:
            ground_args.append(arg)
    ground_args = tuple(ground_args)
    cached_grounded[k] = ground_args
    return literal.positive, literal.predicate, ground_args

def ground_rule(rule, assignment):
    k = hash(frozenset(assignment.items()))
    head, body = rule
    ground_head = None
    if head:
        ground_head = ground_literal(head, assignment, k)
    ground_body = frozenset(ground_literal(literal, assignment, k) for literal in body)
    return ground_head, ground_body

def make_literal_handle(literal):
    return f'{literal.predicate}{"".join(literal.arguments)}'

def make_rule_handle(rule):
    # if clause in self.seen_clause_handle:
        # return self.seen_clause_handle[clause]
    head, body = rule
    body_literals = sorted(body, key = operator.attrgetter('predicate'))
    handle = ''.join(make_literal_handle(literal) for literal in [head] + body_literals)
    # self.seen_clause_handle[clause] = clause_handle
    return handle

def build_seen_rule(rule):
    rule_var = vo_clause('l')
    handle = make_rule_handle(rule)
    head = Literal('seen_rule', (handle, rule_var))
    body = tuple(build_rule_literals(rule, rule_var))
    return head, body

def build_seen_rule_literal(handle, rule_var):
    return Literal('seen_rule', (handle, rule_var))

# TODO: COULD CACHE TUPLES OF ARGS FOR TINY OPTIMISATION
def parse_model(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    rule_index_to_body = defaultdict(set)
    rule_index_to_head = {}
    rule_index_ordering = defaultdict(set)

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

        elif atom.name == 'before':
            rule1 = args[0].number
            rule2 = args[1].number
            rule_index_ordering[rule1].add(rule2)

    prog = []
    rule_lookup = {}

    # rules = set(rule_index_to_head.keys()).union(set(rule_index_to_body.keys()))
    # for rule_index in rules:
    #     head = None
    #     if rule_index in rule_index_to_head:
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
        rule_lookup[rule_index] = rule

    rule_ordering = defaultdict(set)
    for r1_index, lower_rule_indices in rule_index_ordering.items():
        r1 = rule_lookup[r1_index]
        rule_ordering[r1] = set(rule_lookup[r2_index] for r2_index in lower_rule_indices)

    return frozenset(prog), rule_ordering, directions

def build_rule_literals(rule, rule_var):
    literals = []
    head, body = rule
    yield Literal('head_literal', (rule_var, head.predicate, head.arity, tuple(vo_variable2(rule_var, v) for v in head.arguments)))

    for body_literal in body:
        yield Literal('body_literal', (rule_var, body_literal.predicate, body_literal.arity, tuple(vo_variable2(rule_var, v) for v in body_literal.arguments)))
    for idx, var in enumerate(head.arguments):
        yield eq(vo_variable2(rule_var, var), idx)

def build_rule_ordering_literals(rule_index, rule_ordering):
    for r1, higher_rules in rule_ordering.items():
        r1v = rule_index[r1]
        for r2 in higher_rules:
            r2v = rule_index[r2]
            yield lt(r1v, r2v)

class Generator:

    # def con_to_strings(self, con):
    #      for grule in get_ground_rules((None, con)):
    #         # print('grule', grule)
    #         h, b = grule
    #         rule = []
    #         for sign, pred, args in b:
    #             if not sign:
    #                 rule.append(f'not {pred}{args}')
    #             else:
    #                 rule.append(f'{pred}{args}')
    #         rule = ':- ' + ', '.join(sorted(rule)) + '.'
    #         rule = rule.replace("'","")
    #         rule = rule.replace('not clause(1,)','not clause(1)')
    #         yield rule


    def update_solver(self, size, handles, bad_handles, ground_cons):
        encoding = []
        for handle, rule in handles:
            if handle in self.seen_handles:
                continue
            self.seen_handles.add(handle)
            rule = rule.replace("'","")
            encoding.append(rule)

        for con in ground_cons:
            if con in self.seen_cons:
                continue
            self.seen_cons.add(con)
            encoding.append(con)
        # encoding.extend(ground_cons)

        for handle in bad_handles:
            encoding.append(f"bad_handle({handle}).")

        encoding = '\n'.join(encoding)
        # print(encoding)
        k = f'base-{size}'
        self.solver.add(k, [], encoding)
        self.solver.ground([(k, [])])

    # def __init__(self, settings, grounder):
    def __init__(self, settings, grounder, size, handles, bad_handles, ground_cons):
        self.settings = settings
        self.grounder = grounder
        self.seen_handles = set()
        # self.assigned = {}
        # self.seen_cons = set()

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        encoding.append(alan)
        with open(settings.bias_file) as f:
            encoding.append(f.read())
        encoding.append(f'max_clauses({settings.max_rules}).')
        encoding.append(f'max_body({settings.max_body}).')
        encoding.append(f'max_vars({settings.max_vars}).')
        encoding.append(f':- not size({size}).')


        # encoding = []
        for handle, rule in handles:
            # if handle in self.seen_handles:
                # continue
            self.seen_handles.add(handle)
            rule = rule.replace("'","")
            encoding.append(rule)

        # for con in ground_cons:
            # if con in self.seen_cons:
                # continue
            # self.seen_cons.add(con)
            # encoding.append(con)
        encoding.extend(ground_cons)

        for handle in bad_handles:
            encoding.append(f"bad_handle({handle}).")

        # encoding = '\n'.join(encoding)
        # print(encoding)
        # k = f'base'
        # self.solver.add(k, [], encoding)
        # self.solver.ground([(k, [])])


        if self.settings.bkcons:
            encoding.append(self.settings.bkcons)

        encoding = '\n'.join(encoding)

        solver = clingo.Control([])
        solver.configuration.solve.models = 0


        # NUM_OF_LITERALS = """
        # %%% External atom for number of literals in the program %%%%%
        # #external size_in_literals(n).
        # :-
        #     size_in_literals(n),
        #     #sum{K+1,Clause : body_size(Clause,K)} != n.
        # """

        # solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver

    def update_number_of_literals(self, size):
        # 1. Release those that have already been assigned
        for atom, truth_value in self.assigned.items():
            if atom[0] == 'size_in_literals' and truth_value:
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

    # @profile
    def get_ground_rules(self, rule):
        head, body = rule
        # find bindings for variables in the rule
        assignments = self.grounder.find_bindings(rule, self.settings.max_rules, self.settings.max_vars)
        # keep only standard literals
        body = tuple(literal for literal in body if not literal.meta)
        # ground the rule for each variable assignment
        return set(ground_rule((head, body), assignment) for assignment in assignments)


    def build_generalisation_constraint(self, prog, rule_ordering={}):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            handle = make_rule_handle(rule)
            if rule in self.seen_handles:
                literals.append(build_seen_rule_literal(rule, rule_var))
            else:
                new_handles.add((handle, build_seen_rule(rule)))
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(body_size_literal(rule_var, len(body)))
        literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))

        return new_handles, tuple(literals)

    def build_specialisation_constraint(self, prog, rule_ordering={}):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            handle = make_rule_handle(rule)
            if handle in self.seen_handles:
                literals.append(build_seen_rule_literal(handle, rule_var))
            else:
                new_handles.add((handle, build_seen_rule(rule)))
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(lt(rule_var, len(prog)))
        literals.append(Literal('clause', (len(prog), ), positive = False))
        literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        return new_handles, tuple(literals)

    # # DOES NOT WORK WITH PI!!!
    # only works with single rule programs
    def redundancy_constraint1(self, prog, rule_ordering={}):
        new_handles = set()
        rule_index = {}
        literals = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            handle = make_rule_handle(rule)

            if handle in self.seen_handles:
                literals.append(build_seen_rule_literal(handle, rule_var))
            else:
                new_handles.add((handle, build_seen_rule(rule)))
                literals.extend(build_rule_literals(rule, rule_var))
            literals.append(gteq(rule_var, 1))
            literals.append(Literal('recursive_clause',(rule_var, head.predicate, head.arity)))
            literals.append(Literal('num_recursive', (head.predicate, 1)))
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
            return handle, new_handles, tuple(literals)

    def redundancy_constraint2(self, prog, rule_ordering={}):
        prog = list(prog)
        # rule_index = {}
        # literals = []
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


        for lit in lits_num_rules.keys() - recursively_called:
            rule_index = {}
            literals = []

            for rule_id, rule in enumerate(prog):
                head, body = rule
                rule_var = vo_clause(rule_id)
                rule_index[rule] = rule_var
                literals.extend(build_rule_literals(rule, rule_var))

            for other_lit, num_clauses in lits_num_rules.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_rules[lit]
            literals.append(Literal('num_recursive', (lit, num_recursive)))
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
            return tuple(literals)

BINDING_ENCODING = """\
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

# def vo_variable(variable):
    # return ConstVar(f'{variable}', 'Variable')

def vo_variable2(rule, variable):
    key = f'{rule.name}_V{variable}'
    return VarVar(rule=rule, name=key)

def vo_clause(variable):
    return RuleVar(name=f'R{variable}')

def alldiff(args):
    return Literal('AllDifferent', args, meta=True)

def lt(a, b):
    return Literal('<', (a,b), meta=True)

def eq(a, b):
    return Literal('==', (a,b), meta=True)

def gteq(a, b):
    return Literal('>=', (a,b), meta=True)

def body_size_literal(clause_var, body_size):
    return Literal('body_size', (clause_var, body_size))

def alldiff(args):
    return Literal('AllDifferent', args, meta=True)

class Grounder():
    def __init__(self):
        self.seen_assignments = {}

    def find_bindings(self, rule, max_rules, max_vars):
        _, body = rule
        # TODO: add back
        all_vars = find_all_vars(body)

        if len(all_vars) == 0:
            assert(False)
        #     return [{}]

        k = grounding_hash(body, all_vars)
        if k in self.seen_assignments:
            return self.seen_assignments[k]

        # print('ASDA!!')

        # map each rule and var_var in the program to an integer
        rule_var_to_int = {v:i for i, v in enumerate(var for var in all_vars if isinstance(var, RuleVar))}
        # var_var_to_int = {v:i for i,v in enumerate(var for var in all_vars if isinstance(var, VarVar))}

        int_to_rule_var = {i:v for v,i in rule_var_to_int.items()}
        # int_to_var_var = {i:v for v,i in var_var_to_int.items()}

        # print('HERE!!!')
        # print('all_vars1\t', all_vars)
        # print('rule_var_to_int\t', rule_var_to_int)
        # print('var_var_to_int', len(var_var_to_int), var_var_to_int)
        # print('int_to_rule_var', int_to_rule_var)
        # print('int_to_var_var', int_to_var_var)

        encoding = []
        encoding.append(BINDING_ENCODING)
        encoding.append(f'#const max_rules={max_rules}.')
        encoding.append(f'#const max_vars={max_vars}.')

        # find all variables for each rule
        rule_vars = {k:set() for k in rule_var_to_int}
        for var in all_vars:
            if isinstance(var, VarVar):
                rule_vars[var.rule].add(var)


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
            if not lit.meta:
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
        solver = clingo.Control()
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
        # for x in body:
            # if x.predicate == 'seen_rule':
                # print(encoding)
                # print('all_vars:\t',all_vars)
                # print('rule_vars:\t',rule_vars)
                # print(','.join(str(y) for y in body))
                # for z in out:
                    # print(z)
        return out
