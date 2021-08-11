import operator
from collections import defaultdict
from . core import ConstVar, ConstOpt, Literal

class Outcome:
    ALL = 'all'
    SOME = 'some'
    NONE = 'none'

class Con:
    GENERALISATION = 'generalisation'
    SPECIALISATION = 'specialisation'
    REDUNDANCY = 'redundancy'
    BANISH = 'banish'

OUTCOME_TO_CONSTRAINTS = {
    (Outcome.ALL, Outcome.NONE)  : (Con.BANISH,),
    (Outcome.ALL, Outcome.SOME)  : (Con.GENERALISATION,),
    (Outcome.SOME, Outcome.NONE) : (Con.SPECIALISATION,),
    (Outcome.SOME, Outcome.SOME) : (Con.SPECIALISATION, Con.GENERALISATION),
    (Outcome.NONE, Outcome.NONE) : (Con.SPECIALISATION, Con.REDUNDANCY),
    (Outcome.NONE, Outcome.SOME) : (Con.SPECIALISATION, Con.REDUNDANCY, Con.GENERALISATION)
}

def alldiff(vars):
    return ConstOpt(None, vars, 'AllDifferent')

def lt(a, b):
    return ConstOpt(operator.lt, (a, b), '<')

def eq(a, b):
    return ConstOpt(operator.eq, (a, b), '==')

def gteq(a, b):
    return ConstOpt(operator.ge, (a, b), '>=')

def vo_clause(variable):
    return ConstVar(f'C{variable}', 'Clause')

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

# restrict a clause id to have a specific body size
def body_size_literal(clause_var, body_size):
    return Literal('body_size', (clause_var, body_size))

class Constrain:
    def __init__(self, experiment):
        self.seen_clause_handle = {}
        self.added_clauses = set()

    @staticmethod
    def decide_outcome(tp, fn, tn, fp):
        # complete
        if fn == 0:
            positive_outcome = Outcome.ALL
        # totally incomplete
        elif tp == 0 and fn > 0: # AC: we must use TP==0 rather than FN=|E+| because of minimal testing
            positive_outcome = Outcome.NONE
        # incomplete
        else:
            positive_outcome = Outcome.SOME

        # consistent
        if fp == 0:
            negative_outcome = Outcome.NONE
        # totally inconsistent
        # AC: this line may not work with minimal testing
        # elif FP == self.num_neg:
            # negative_outcome = Outcome.ALL
        # inconsistent
        else:
            negative_outcome = Outcome.SOME

        return (positive_outcome, negative_outcome)

    def build_constraints(self, program, outcome):
        (positive_outcome, negative_outcome) = outcome
        # RM: If you don't use these two lines you need another three entries in the OUTCOME_TO_CONSTRAINTS table (one for every positive outcome combined with negative outcome ALL).
        if negative_outcome == Outcome.ALL:
             negative_outcome = Outcome.SOME
        for constraint_type in OUTCOME_TO_CONSTRAINTS[((positive_outcome, negative_outcome))]:
            if constraint_type == Con.GENERALISATION:
                yield from self.generalisation_constraint(program)
            elif constraint_type == Con.SPECIALISATION:
                yield from self.specialisation_constraint(program)
            elif constraint_type == Con.REDUNDANCY:
                yield from self.redundancy_constraint(program)
            elif constraint_type == Con.BANISH:
                yield from self.banish_constraint(program)

    def make_literal_handle(self, literal):
        return f'{literal.predicate}{"".join(literal.arguments)}'

    def make_clause_handle(self, clause):
        if clause in self.seen_clause_handle:
            return self.seen_clause_handle[clause]
        body_literals = sorted(clause.body, key = operator.attrgetter('predicate'))
        clause_handle = ''.join(self.make_literal_handle(literal) for literal in [clause.head] + body_literals)
        self.seen_clause_handle[clause] = clause_handle
        return clause_handle

    def make_clause_inclusion_rule(self, clause, clause_handle):
        if clause_handle in self.added_clauses:
            return
            yield

        self.added_clauses.add(clause_handle)
        clause_number = vo_clause('l')

        literals = []
        literals.append(Literal('head_literal', (clause_number, clause.head.predicate,
            clause.head.arity, tuple(vo_variable(v) for v in clause.head.arguments))))

        for body_literal in clause.body:
            literals.append(Literal('body_literal', (clause_number, body_literal.predicate,
            body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))

        literals.append(gteq(clause_number, clause.min_num))

        # ensure that each var_var is ground to a unique value
        literals.append(alldiff(tuple(vo_variable(v) for v in clause.all_vars())))

        for idx, var in enumerate(clause.head.arguments):
            literals.append(eq(vo_variable(var), idx))

        yield (Literal('included_clause', (clause_handle, clause_number)), tuple(literals))

    def banish_constraint(self, program):
        literals = []
        for clause_number, clause in enumerate(program.clauses):
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause, clause_handle)

            literals.append(Literal('included_clause', (clause_handle, vo_clause(clause_number))))
            literals.append(body_size_literal(vo_clause(clause_number), len(clause.body)))

        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

        for clause_number, clause in enumerate(program.clauses):
            literals.append(gteq(vo_clause(clause_number), clause.min_num))

        # ensure that each clause_var is ground to a unique value
        literals.append(alldiff(tuple(vo_clause(c) for c in range(program.num_clauses))))
        literals.append(Literal('clause', (program.num_clauses, ), positive = False))

        yield (None, tuple(literals))


    def generalisation_constraint(self, program):
        literals = []
        for clause_number, clause in enumerate(program.clauses):
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause, clause_handle)

            literals.append(Literal('included_clause', (clause_handle, vo_clause(clause_number))))
            literals.append(body_size_literal(vo_clause(clause_number), len(clause.body)))

        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

        for clause_number, clause in enumerate(program.clauses):
            literals.append(gteq(vo_clause(clause_number), clause.min_num))

        # ensure that each clause_var is ground to a unique value
        literals.append(alldiff(tuple(vo_clause(c) for c in range(program.num_clauses))))

        yield (None, tuple(literals))

    def specialisation_constraint(self, program):
        literals = []

        for clause_number, clause in enumerate(program.clauses):
            clause_handle = self.make_clause_handle(clause)
            yield from self.make_clause_inclusion_rule(clause, clause_handle)
            clause_variable = vo_clause(clause_number)
            literals.append(Literal('included_clause', (clause_handle, clause_variable)))

        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

        # ensure that each clause_var is ground to a unique value
        literals.append(alldiff(tuple(vo_clause(c) for c in range(program.num_clauses))))

        literals.append(Literal('clause', (program.num_clauses, ), positive = False))

        yield (None, tuple(literals))

    # AC: THIS CONSTRAINT DUPLICATES THE GENERALISATION CONSTRAINT AND NEEDS REFACTORING
    def redundant_literal_constraint(self, clause):
        clause_handle = self.make_clause_handle(clause)
        yield from self.make_clause_inclusion_rule(clause, clause_handle)
        literals = []
        clause_variable = vo_clause(0)
        literals.append(Literal('included_clause', (clause_handle, clause_variable)))
        literals.append(body_size_literal(clause_variable, len(clause.body)))
        yield (None, tuple(literals))

    # Jk: AC, I cleaned this up a bit, but this reorg is for you. Godspeed!
    # AC: @JK, I made another pass through it. It was tough. I will try again once we have the whole codebase tidied.
    def redundancy_constraint(self, program):
        lits_num_clauses = defaultdict(int)
        lits_num_recursive_clauses = defaultdict(int)
        for clause in program.clauses:
            head_pred = clause.head.predicate
            lits_num_clauses[head_pred] += 1
            if clause.is_recursive():
                lits_num_recursive_clauses[head_pred] += 1

        recursively_called = set()
        while True:
            something_added = False
            for clause in program.clauses:
                head_pred = clause.head.predicate
                for body_literal in clause.body:
                    body_pred = body_literal.predicate
                    if body_pred not in lits_num_clauses:
                        continue
                    if (body_pred != head_pred and clause.is_recursive()) or (head_pred in recursively_called):
                        something_added |= not body_pred in recursively_called
                        recursively_called.add(body_pred)
            if not something_added:
                break

        for lit in lits_num_clauses.keys() - recursively_called:
            literals = []

            for clause_number, clause in enumerate(program.clauses):
                clause_handle = self.make_clause_handle(clause)
                yield from self.make_clause_inclusion_rule(clause, clause_handle)
                clause_variable = vo_clause(clause_number)
                literals.append(Literal('included_clause', (clause_handle, clause_variable)))

            for clause_number1, clause_numbers in program.before.items():
                for clause_number2 in clause_numbers:
                    literals.append(lt(vo_clause(clause_number1), vo_clause(clause_number2)))

            # ensure that each clause_var is ground to a unique value
            literals.append(alldiff(tuple(vo_clause(c) for c in range(program.num_clauses))))

            for other_lit, num_clauses in lits_num_clauses.items():
                if other_lit == lit:
                    continue
                literals.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_clauses[lit]

            literals.append(Literal('num_recursive', (lit, num_recursive)))

            yield (None, tuple(literals))


    @staticmethod
    def print_constraint(con):
        (head, body) = con
        constraint_literals = []
        for constobj in body:
            if isinstance(constobj, Literal):
                constraint_literals.append(str(constobj))
            elif isinstance(constobj, ConstOpt):
                if constobj.operation == 'AllDifferent':
                    # print(f'ALLDIFF:{constobj.arguments}')
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
                constraint_literals.append(f'{arga}{constobj.operation}{argb}')

        x = f':- {", ".join(constraint_literals)}.'
        if head:
            x = f'{head} {x}'
        print(x)

