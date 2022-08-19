import clingo
import clingo.script
import pkg_resources
from . core import Literal, ConstVar
from collections import defaultdict
clingo.script.enable_python()

arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}

class Generator:

    def con_to_strings(self, con):
         for grule in self.get_ground_rules((None, con)):
            h, b = grule
            rule = []
            for sign, pred, args in b:
                if not sign:
                    rule.append(f'not {pred}{args}')
                else:
                    rule.append(f'{pred}{args}')
            rule = ':- ' + ', '.join(sorted(rule)) + '.'
            rule = rule.replace("'","")
            rule = rule.replace('not clause(1,)','not clause(1)')
            yield rule

    def __init__(self, settings, grounder):
        self.settings = settings
        self.grounder = grounder

        encoding = []
        alan = pkg_resources.resource_string(__name__, "lp/alan.pl").decode()
        encoding.append(alan)
        with open(settings.bias_file) as f:
            encoding.append(f.read())
        encoding.append(f'max_clauses({settings.max_rules}).')
        encoding.append(f'max_body({settings.max_body}).')
        encoding.append(f'max_vars({settings.max_vars}).')

        if self.settings.bkcons:
            encoding.append(self.settings.bkcons)

        encoding = '\n'.join(encoding)

        solver = clingo.Control(["--heuristic=Domain"])
        # solver = clingo.Control(["-t2"])
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        self.solver = solver


    # TODO: COULD CACHE TUPLES OF ARGS FOR TINY OPTIMISATION
    def parse_model(self, model):
        # print('model', model)
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

        return frozenset(prog), rule_ordering

    def get_ground_rules(self, rule):
        head, body = rule

        # find bindings for variables in the rule
        assignments = self.grounder.find_bindings(rule, self.settings.max_rules, self.settings.max_vars)

        # keep only standard literals
        body = tuple(literal for literal in body if not literal.meta)

        # ground the rule for each variable assignment
        return set(self.grounder.ground_rule((head, body), assignment) for assignment in assignments)

    # def orderings(self, prog):
    #     prog = list(prog)
    #     headpred = {}
    #     for i in range(len(prog))
    #         head, body = prog[i]

    #     lower = set()
    #     for i in range(1,10):
    #         p1 = f'inv{i}'
    #         lower.add(settings.headpred, p1)
    #         for j in range(i+1,10):
    #             p2 = f'inv{j}'
    #             x = (p1,p2)
    #             lower.add(x)

    #     def before(r1, r2):

    #         for i

    # h1 = headpred(r1)
    # h2 = headpred(r2)

    # if (h1, h2) in lower:
    #     return True

    # if h1 == h2:
    #     if not recursive(r1) and recursive(r2):
    #         return True

    #     if (not recursive(r1) and not recursive(r2)) or (recursive(r1) and recursive(r2)):
    #         if size(r1) < size(r2):
    #             return True
    # return False

    def build_generalisation_constraint(self, prog, rule_ordering={}):
        prog = list(prog)
        rule_index = {}
        literals = []
        for clause_number, rule in enumerate(prog):
            rule_index[rule] = vo_clause(clause_number)
            head, body = rule
            clause_number = vo_clause(clause_number)
            literals.append(Literal('head_literal', (clause_number, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))

            for body_literal in body:
                literals.append(Literal('body_literal', (clause_number, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))

            for idx, var in enumerate(head.arguments):
                literals.append(eq(vo_variable(var), idx))

            literals.append(body_size_literal(clause_number, len(body)))

        for r1, higher_rules in rule_ordering.items():
            r1v = rule_index[r1]
            for r2 in higher_rules:
                r2v = rule_index[r2]
                literals.append(lt(r1v, r2v))

        return tuple(literals)

    def build_specialisation_constraint(self, prog, rule_ordering={}):
        prog = list(prog)
        rule_index = {}
        literals = []
        for clause_number, rule in enumerate(prog):
            rule_index[rule] = vo_clause(clause_number)
            head, body = rule
            clause_number = vo_clause(clause_number)
            literals.append(Literal('head_literal', (clause_number, head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))

            for body_literal in body:
                literals.append(Literal('body_literal', (clause_number, body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))

            for idx, var in enumerate(head.arguments):
                literals.append(eq(vo_variable(var), idx))
            literals.append(lt(clause_number, len(prog)))
        literals.append(Literal('clause', (len(prog), ), positive = False))

        for r1, higher_rules in rule_ordering.items():
            r1v = rule_index[r1]
            for r2 in higher_rules:
                r2v = rule_index[r2]
                literals.append(lt(r1v, r2v))
        return tuple(literals)


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

def alldiff(args):
    return Literal('AllDifferent', args, meta=True)

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
                a = lit.arguments[0]
                b = lit.arguments[1]
                if type(lit.arguments[1]) == int:
                # ABSOLUTE HACK
                    var1 = c_vars[a]
                    solver.add('base', [], f':- c_var({var1},Val1), Val1 >= {b}.')
                    # pass
                else:
                    var1 = c_vars[a]
                    var2 = c_vars[b]
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