import clingo
import operator
import numbers
import clingo.script
import pkg_resources
from . core import Literal, RuleVar, VarVar, Var
from collections import defaultdict
from . util import rule_is_recursive, format_rule
clingo.script.enable_python()
from clingo import Function, Number, Tuple_

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

cached_handles = {}
def make_rule_handle(rule):
    k = hash(rule)
    if k in cached_handles:
        return cached_handles[k]
    head, body = rule
    body_literals = sorted(body, key = operator.attrgetter('predicate'))
    handle = ''.join(make_literal_handle(literal) for literal in [head] + body_literals)
    cached_handles[k] = handle
    return handle

def build_seen_rule(rule, is_rec):
    rule_var = vo_clause('l')
    handle = make_rule_handle(rule)
    head = Literal('seen_rule', (handle, rule_var))
    body = []
    body.extend(build_rule_literals(rule, rule_var))
    if is_rec:
        body.append(gteq(rule_var, 1))
    return head, tuple(body)

def build_seen_rule_literal(handle, rule_var):
    return Literal('seen_rule', (handle, rule_var))

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
    yield Literal('head_literal', (rule_var, head.predicate, len(head.arguments), tuple(vo_variable2(rule_var, v) for v in head.arguments)))

    for body_literal in body:
        yield Literal('body_literal', (rule_var, body_literal.predicate, len(body_literal.arguments), tuple(vo_variable2(rule_var, v) for v in body_literal.arguments)))
    for idx, var in enumerate(head.arguments):
        yield eq(vo_variable2(rule_var, var), idx)

    if rule_is_recursive(rule):
        yield gteq(rule_var, 1)

def build_rule_ordering_literals(rule_index, rule_ordering):
    for r1, higher_rules in rule_ordering.items():
        r1v = rule_index[r1]
        for r2 in higher_rules:
            r2v = rule_index[r2]
            yield lt(r1v, r2v)

def load_types(settings):
    enc = """
#defined clause/1.
#defined clause_var/2.
#defined var_type/3."""
    # solver = clingo.Control()
    solver = clingo.Control(['-Wnone'])
    with open(settings.bias_file) as f:
        solver.add('bias', [], f.read())
    solver.add('bias', [], enc)
    solver.ground([('bias', [])])

    for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
        head_pred = x.symbol.arguments[0].name
        head_arity = x.symbol.arguments[1].number

    head_types = None
    body_types = {}
    for x in solver.symbolic_atoms.by_signature('type', arity=2):
        pred = x.symbol.arguments[0].name
        # xs = (str(t) for t in )
        xs = [y.name for y in x.symbol.arguments[1].arguments]
        if pred == head_pred:
            head_types = xs
        else:
            body_types[pred] = xs

    return head_types, body_types

class Generator:

    def __init__(self, settings, grounder):
        self.settings = settings
        self.grounder = grounder
        self.seen_handles = set()
        self.assigned = {}
        self.seen_symbols = {}

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        encoding.append(alan)
        with open(settings.bias_file) as f:
            encoding.append(f.read())
        encoding.append(f'max_clauses({settings.max_rules}).')
        encoding.append(f'max_body({settings.max_body}).')
        encoding.append(f'max_vars({settings.max_vars}).')
        max_size = (1 + settings.max_body) * settings.max_rules
        if settings.max_literals < max_size:
            encoding.append(f'custom_max_size({settings.max_literals}).')

        if self.settings.bkcons:
            encoding.append(self.settings.bkcons)

        encoding = '\n'.join(encoding)

        if self.settings.single_solve:
            # solver = clingo.Control(["--heuristic=Domain", "-t2"])
            # solver = clingo.Control(["--heuristic=Domain", '-Wnone'])
            solver = clingo.Control(['--heuristic=Domain','-Wnone'])
            # solver = clingo.Control(['-Wnone'])
        else:
            # solver = clingo.Control(["-t4"])
            # solver = clingo.Control([])
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

        self.settings.head_types, self.settings.body_types = load_types(settings)

    def gen_symbol(self, literal, backend):
        sign, pred, args = literal
        k = hash(literal)
        if k in self.seen_symbols:
            symbol = self.seen_symbols[k]
        else:
            symbol = backend.add_atom(atom_to_symbol(pred, args))
            self.seen_symbols[k] = symbol
        return symbol

    def update_solver(self, size, handles, bad_handles, ground_cons):
        # rules to add via Clingo's backend interface
        to_add = []
        to_add.extend(([], x) for x in ground_cons)


        new_seen_rules = set()

        # add handles for newly seen rules
        # for handle, rule in handles:
        for rule in handles:
            head, body = rule
            head_pred, head_args = head

            if head_pred == 'seen_rule':
                new_seen_rules.add(head_args[0])

            new_head = (True, head_pred, head_args)
            new_body = frozenset((True, pred, args) for pred, args in body)
            to_add.append((new_head, new_body))




        # bad_handles = []
        if bad_handles:
            for handle in bad_handles:
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

        # for x in set(handle for handle, rule in handles):
        self.seen_handles.update(new_seen_rules)

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

    def get_ground_rules(self, rule):
        head, body = rule
        # find bindings for variables in the rule
        assignments = self.grounder.find_bindings(rule, self.settings.max_rules, self.settings.max_vars)
        # keep only standard literals
        body = tuple(literal for literal in body if not literal.meta)
        # ground the rule for each variable assignment
        return set(ground_rule((head, body), assignment) for assignment in assignments)

    def get_ground_deep_rules(self, body):
        # find bindings for variables in the rule
        assignments = self.grounder.find_deep_bindings(body, self.settings.max_rules, self.settings.max_vars)
        # ground the rule for each variable assignment
        return set(ground_rule((False, body), assignment) for assignment in assignments)

    def build_generalisation_constraint(self, prog, rule_ordering=None):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        recs = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            handle = make_rule_handle(rule)
            is_rec = rule_is_recursive(rule)
            if is_rec:
                recs.append((len(body), rule))
            if handle in self.seen_handles:
                literals.append(build_seen_rule_literal(handle, rule_var))
                if is_rec:
                    literals.append(gteq(rule_var, 1))
            else:
                new_handles.add(build_seen_rule(rule, is_rec))
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(body_size_literal(rule_var, len(body)))
        if rule_ordering:
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        else:
            for k1, r1 in recs:
                r1v = rule_index[r1]
                for k2, r2 in recs:
                    r2v = rule_index[r2]
                    if k1 < k2:
                        literals.append(lt(r1v, r2v))
        return new_handles, tuple(literals)

    def build_specialisation_constraint(self, prog, rule_ordering=None):
        new_handles = set()
        prog = list(prog)
        rule_index = {}
        literals = []
        recs = []
        for rule_id, rule in enumerate(prog):
            head, body = rule
            rule_var = vo_clause(rule_id)
            rule_index[rule] = rule_var
            handle = make_rule_handle(rule)
            is_rec = rule_is_recursive(rule)
            if is_rec:
                recs.append((len(body), rule))
            if handle in self.seen_handles:
                literals.append(build_seen_rule_literal(handle, rule_var))
                if is_rec:
                    literals.append(gteq(rule_var, 1))
            else:
                new_handles.add(build_seen_rule(rule, is_rec))
                literals.extend(tuple(build_rule_literals(rule, rule_var)))
            literals.append(lt(rule_var, len(prog)))
        literals.append(Literal('clause', (len(prog), ), positive = False))
        if rule_ordering:
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
        else:
            for k1, r1 in recs:
                r1v = rule_index[r1]
                for k2, r2 in recs:
                    r2v = rule_index[r2]
                    if k1 < k2:
                        literals.append(lt(r1v, r2v))
        return new_handles, tuple(literals)

    def andy_tmp_con(self, prog, rule_ordering={}):
    # :-
    # seen_rule(fABfCBtailAC,R1),
    # seen_rule(fABfCBtailAC,R2),
    # R1 < R2,
    # body_size(R1,2).
        for rule in prog:
            if not rule_is_recursive(rule):
                continue
            head, body = rule
            handle = make_rule_handle(rule)
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
    def redundancy_constraint1(self, prog, rule_ordering={}):
        assert(len(prog) == 1)
        new_handles = set()
        literals = []

        rule_id = 0
        rule = list(prog)[0]
        assert(not rule_is_recursive(rule))
        head, body = rule
        rule_var = vo_clause(rule_id)
        handle = make_rule_handle(rule)

        if handle in self.seen_handles:
            literals.append(build_seen_rule_literal(handle, rule_var))
        else:
            # new_handles.add((handle, build_seen_rule(rule)))
            new_handles.add(build_seen_rule(rule, False))
            literals.extend(build_rule_literals(rule, rule_var))
        literals.append(gteq(rule_var, 1))
        literals.append(Literal('recursive_clause',(rule_var, head.predicate, len(head.arguments))))
        literals.append(Literal('num_recursive', (head.predicate, 1)))
        # print('RED1', format_rule(rule))
        # print('\n'.join(str(x) for x in literals))
        return handle, new_handles, tuple(literals)

    # def redundant_rules_check(self, rule1, rule2):-


    def unsat_constraint(self, body):
        rule_var = vo_clause('X')
        return tuple(Literal('body_literal', (rule_var, body_literal.predicate, len(body_literal.arguments), tuple(vo_variable2(rule_var, v) for v in body_literal.arguments))) for body_literal in body)


    def redundancy_constraint4(self, program, rule_ordering={}):
        new_handles = set()
        new_rules = []

        prog_handle = "prog"
        for clause in program:
            clause_handle = make_rule_handle(clause)
            prog_handle += '_' + clause_handle
        clause_variable = vo_clause('')

        # rules encoding which clauses specialises any clause of the program
        for rule in program:
            handle = make_rule_handle(rule)

            if handle not in self.seen_handles:
                # new_handles.add((handle, build_seen_rule(rule)))
                new_handles.add(build_seen_rule(rule))

            h = Literal('specialises', (clause_variable, prog_handle))
            b = tuple([build_seen_rule_literal(handle, clause_variable)])
            new_rules.append((h, b))

        # # rules exhaustively checking for all cases where a clause is not redundant
        clause_var1 = vo_clause('1')
        clause_var2 = vo_clause('2')
        r1 = (Literal('not_redundant', (clause_variable, prog_handle)), (Literal('specialises', (clause_variable, prog_handle), positive = False),))
        r2 = (Literal('not_redundant', (clause_var1, prog_handle)), (Literal('specialises', (clause_var1, prog_handle)),Literal('depends_on', (clause_var1, clause_var2)), Literal('specialises', (clause_var2, prog_handle), positive = False)))
        r3 = (Literal('not_redundant', (clause_var1, prog_handle)), (Literal('specialises', (clause_var1, prog_handle)), Literal('depends_on', (clause_var2, clause_var1)), Literal('specialises', (clause_var2, prog_handle), positive = False)))
        # r4 = (None, (Literal('not_redundant', (clause_variable, prog_handle), positive = False),))
        # new_rules.extend([r1,r2,r3])
        # new_rules.extend([r1])

        # for x in new_rules:
        #     h, b = x
        #     print(h, ', '.join(map(str,b)))
            # print(x, type(x))
            # h,b = x
            # print(h, ','.join(str(y) for y in b))
        # a rule is redundant iff it is not not redundant - if there is a redundant rule, we prune

        con = [Literal('not_redundant', (clause_variable, prog_handle), positive = False)]
        return new_rules, tuple(con)

    def redundancy_constraint2(self, prog, rule_ordering={}):

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
                handle = make_rule_handle(rule)

                is_rec = rule_is_recursive(rule)

                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                    if is_rec:
                        literals.append(gteq(rule_var, 1))
                else:
                    new_handles.add(build_seen_rule(rule, is_rec))
                    literals.append(build_seen_rule_literal(handle, rule_var))

            for other_lit, num_clauses in lits_num_rules.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_rules[lit]
            literals.append(Literal('num_recursive', (lit, num_recursive)))
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))
            out_cons.append(tuple(literals))

            # print(':- ' + ', '.join(map(str,literals)))

        return new_handles, out_cons



    def redundancy_constraint3(self, prog, rule_ordering={}):

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
            recs = []
            for rule_id, rule in enumerate(prog):
                head, body = rule
                rule_var = vo_clause(rule_id)
                rule_index[rule] = rule_var
                handle = make_rule_handle(rule)

                if rule_is_recursive(rule):
                    recs.append((len(body), rule))
                if handle in self.seen_handles:
                    literals.append(build_seen_rule_literal(handle, rule_var))
                    if rule_is_recursive(rule):
                        literals.append(gteq(rule_var, 1))
                else:
                    new_handles.add(build_seen_rule(rule))
                    literals.append(build_seen_rule_literal(handle, rule_var))

            for other_lit, num_clauses in lits_num_rules.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_rules[lit]
            literals.append(Literal('num_recursive', (lit, num_recursive)))
            literals.extend(build_rule_ordering_literals(rule_index, rule_ordering))


            for k1, r1 in recs:
                r1v = rule_index[r1]
                for k2, r2 in recs:
                    r2v = rule_index[r2]
                    if k1 < k2:
                        literals.append(lt(r1v, r2v))

            out_cons.append(tuple(literals))

            # print(':- ' + ', '.join(map(str,literals)))

        return new_handles, out_cons


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
    def __init__(self, settings):
        self.seen_assignments = {}
        self.seen_deep_assignments = {}
        self.settings = settings

    def find_bindings(self, rule, max_rules, max_vars):
        _, body = rule

        all_vars = find_all_vars(body)

        k = grounding_hash(body, all_vars)
        if k in self.seen_assignments:
            return self.seen_assignments[k]

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
        # for x in body:
            # if x.predicate == 'seen_rule':
                # print(encoding)
                # print('all_vars:\t',all_vars)
                # print('rule_vars:\t',rule_vars)
                # print(','.join(str(y) for y in body))
                # for z in out:
                    # print(z)
        return out


    # find_deep_bindings(body, head_types, self.settings.max_vars)
    def find_deep_bindings(self, body, max_rules, max_vars):
        all_vars = find_all_vars(body)
        head_types = self.settings.head_types
        body_types = self.settings.body_types

        body_hash = grounding_hash(body, all_vars)
        # print(','.join(map(str,body)), all_vars, body_hash)
        if body_hash in self.seen_deep_assignments:
            # print('moo')
            return self.seen_deep_assignments[body_hash]


        encoding = set()
        encoding.add("""\
#show bind_var/2.
% bind a rule_id to a value
{bind_var(Var,Value)}:-
    var(Var),
    Value=0..max_vars-1.
% a rule value cannot be bound to more than one rule

:- var(Var), #count{Value : bind_var(Var,Value)} != 1.
:- Value=0..max_vars-1, #count{Var : bind_var(Var,Value)} > 1.
:- bind_var(Var,Value), type(Var,T1), type(Value,T2), T1 != T2.
""")
        encoding.add(f'#const max_vars={max_vars}.')
        var_count = 0
        # var_index = {}
        var_lookup = {}
        if head_types:
            for i, head_type in enumerate(head_types):
                encoding.add(f'type({i},{head_type}).')
            # encoding.add(f':- var({i}), not bind_var({i}, {i}).')

        var_count = 0
        var_index = {}
        for lit in body:
            pred = lit.arguments[1]
            for i, x in enumerate(lit.arguments[3]):
                var_name = x.name
                v = var_name.split('_')[1][1:]
                k = ord(v)- ord('A')
                # print(v, k)
                var_lookup[k] = x
                encoding.add(f'var({k}).')
                if pred in body_types:
                    var_type = body_types[pred][i]
                    encoding.add(f'type({k},{var_type}).')




        # for rule_var, xs in rule_vars.items():
        #     rule_var_int = rule_var_to_int[rule_var]
        #     encoding.append(f'rule({rule_var_int}).')

        #     for var_var_int, var_var in enumerate(xs):
        #         encoding.append(f'rule_var({rule_var_int},{var_var_int}).')
        #         int_lookup[(rule_var_int, var_var_int)] = var_var
        #         tmp_lookup[(rule_var, var_var)] = var_var_int

        # for p, types in settings.body_types.items():
        #     for i, t in enumerate(types):
        #         encoding.append(f'arg_type({i},{head_type}'.)



        encoding = '\n'.join(encoding)

        # print('*'*10)
        # print(encoding)

        solver = clingo.Control()
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([("base", [])])

        out = []

        def on_model(m):
            xs = m.symbols(shown = True)
            assignment = {}
            for x in xs:
                name = x.name
                args = x.arguments
                if name == 'bind_var':
                    var_var_int = args[0].number
                    value = args[1].number
                    var_var = var_lookup[var_var_int]
                    assignment[var_var] = value

            for i in range(max_rules):
                x = assignment.copy()
                x[vo_clause('X')] = i
                out.append(x)
            # out.append(assignment)
            # print(assignment)

        solver.solve(on_model=on_model)
        # for x in out:
            # print(x)
        # exit()

        self.seen_deep_assignments[body_hash] = out
        return out
