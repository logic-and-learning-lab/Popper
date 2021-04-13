import operator
from collections import namedtuple
from itertools import chain, product, combinations
from . core import ConstVar, ConstOpt, Constraint, Literal

OUTCOME_TO_CONSTRAINTS = {
    ('all', 'none')  : ('banish',),
    ('all', 'some')  : ('generalisation',),
    ('some', 'none') : ('specialisation',),
    ('some', 'some') : ('specialisation', 'generalisation'),
    ('none', 'none') : ('specialisation', 'redundancy'),
    ('none', 'some') : ('specialisation', 'redundancy', 'generalisation')
}

def lt(args):
    assert len(args) == 2
    return ConstOpt(operator.lt, args, '<')

def gt(args):
    assert len(args) == 2
    return ConstOpt(operator.gt, args, '>')

def eq(args):
    assert len(args) == 2
    return ConstOpt(operator.eq, args, '==')

def neq(args):
    assert len(args) == 2
    return ConstOpt(operator.ne, args, '!=') 

def gteq(args):
    assert len(args) == 2
    return ConstOpt(operator.ge, args, '>=')

def lteq(args):
    assert len(args) == 2
    return ConstOpt(operator.le, args, '<=')

def voclause(variable):
    """Returns variable over a clause"""
    return ConstVar(f'C{variable}', 'Clause')

def vovariable(variable):
    """Returns variable over a variable"""
    return ConstVar(f'{variable}', 'Variable')

def make_clause_handle(clause):
    body_literals = sorted(clause.body, key = operator.attrgetter('predicate'))
    clause_handle = ''
    for literal in [clause.head] + body_literals:
        clause_handle += f'{literal.predicate}{"".join(literal.arguments)}'

    return clause_handle 

def make_program_handle(program):
    return f'prog_{"_".join(sorted(make_clause_handle(clause) for clause in program))}'  

class Constrain:
    def __init__(self, no_pruning = False):
        self.no_pruning  = no_pruning
        self.rule_to_cid = {}
        self.cid_counter = 0
        self.included_clause_handles = set()
    
    def build_constraints(self, program_outcomes):
        for program, (positive_outcome, negative_outcome) in program_outcomes.items():
            constraint_types = self.derive_constraint_types(positive_outcome, negative_outcome)
            for constraint in self.derive_constraints(program, constraint_types):
                yield constraint
            for inclusion_rule in self.derive_inclusion_rules(program, constraint_types):
                yield inclusion_rule

    def derive_constraint_types(self, positive_outcome, negative_outcome):
        if self.no_pruning:
            positive_outcome, negative_outcome = 'all', 'none'
        if negative_outcome == 'all':
            negative_outcome = 'some'
        return OUTCOME_TO_CONSTRAINTS[(positive_outcome, negative_outcome)]
    
    def derive_constraints(self, program, constraint_types):
        for constraint_type in constraint_types:
            if constraint_type == 'generalisation':
                return self.generalisation_constraint(program)
            
            elif constraint_type == 'specialisation':
                return self.specialisation_constraint(program)

            elif constraint_type == 'banish':
                return self.banish_constraint(program)

            elif constraint_type == 'redundancy':
                return self.redundancy_constraint(program)   

    def derive_inclusion_rules(self, program, constraint_types):
        if 'specialisation' in constraint_types or 'generalisation' in constraint_types:
            for clause in program:
                clause_handle, rule = self.make_clause_inclusion_rule(clause)
                if clause_handle not in self.included_clause_handles:
                    self.included_clause_handles.add(clause_handle)
                    yield rule

        if 'redundancy' in constraint_types or 'specialisation' in constraint_types:
            yield self.make_program_inclusion_rule(program)       

    def make_clause_inclusion_rule(self, clause):
        clause_handle = make_clause_handle(clause)
        clause_number = voclause('l')

        constraints = []
        for metapred, lit in chain([('head_literal', clause.head)], 
                                   product(('body_literal',), clause.body)):
            args = (clause_number, lit.predicate, lit.arity, 
                    tuple(vovariable(v) for v in lit.arguments))
            constraints.append(Literal(metapred, args))
        constraints.append(gteq((clause_number, clause.min_num)))

        for var1, var2 in combinations(clause.all_vars(), 2):
            constraints.append(neq((vovariable(var1), vovariable(var2))))
        
        for idx, var in enumerate(clause.head.arguments):
            constraints.append(eq((vovariable(var), idx)))

        head = Literal('included_clause', (clause_handle, clause_number))

        return clause_handle, Constraint('inclusion rule', head, tuple(constraints))

    def make_program_inclusion_rule(self, program):
        program_handle = make_program_handle(program)

        constraints = []
        for clause_number, clause in enumerate(program):
            clause_handle = make_clause_handle(clause)
            clause_variable = voclause(clause_number)
            constraints.append(Literal('included_clause', (clause_handle, clause_variable)))

        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                args = (voclause(clause_number1), voclause(clause_number2))
                constraints.append(lt(args))

        #AC: replace with an AllDiff constraint
        for clause_number1, clause_number2 in combinations(range(len(program)), 2):
            args = voclause(clause_number1), voclause(clause_number2)
            constraints.append(neq(args))
        
        head = Literal('included_program', (program_handle,))
        
        return Constraint('inclusion rule', head, tuple(constraints))

    def generalisation_constraint(self, program):
        constraints = []
        for clause_number, clause in enumerate(program):
            clause_handle = make_clause_handle(clause)
            
            ic_args = (clause_handle, voclause(clause_number))
            constraints.append(Literal('included_clause', ic_args))
                       
            cs_args = (voclause(clause_number), len(clause.body))
            constraints.append(Literal('clause_size', cs_args))
        
        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                args = (voclause(clause_number1), voclause(clause_number2))
                constraints.append(lt(args))

        for clause_number, clause in enumerate(program):
            args = (voclause(clause_number), clause.min_num)
            constraints.append(gteq(args))

        # AC: replace with AllDiff
        for clause_number1, clause_number2 in combinations(range(len(program)), 2):
            args = (voclause(clause_number1), voclause(clause_number2))
            constraints.append(neq(args))
        
        return [Constraint('generalisation', None, tuple(constraints))]

    def specialisation_constraint(self, program):
        program_handle = make_program_handle(program)
        pos_body = Literal('included_program', (program_handle, ))
        neg_body = Literal('clause', (len(program), ), polarity = False)

        return [Constraint('specialisation', None, (pos_body, neg_body))]

    def banish_constraint(self, program):
        constraints = []
        for clause_number, clause in emumerate(program):
            clause_handle = make_clause_handle(clause)
            ic = Literal('included_clause', (clause_handle, clause_number))
            cs = Literal('clause_size', (clause_number, len(clause.body)))
            constraints.append(ic)
            constraints.append(cs)
        constraints.append(Literal('clause', (len(program),)))

        return [Constraint('banish', None, tuple(constraints))]

    # Jk: AC, I cleaned this up a bit, but this reorg is for you. Godspeed!
    def redundancy_constraint(self, program):
        lits_num_clauses = defaultdict(int)
        lits_num_recursive_clauses = defaultdict(int)
        for clause in program:
            lits_num_clauses[clause.head] += 1
            if clause.is_recursive():
                lits_num_recursive_clauses[clause.head] += 1

        recursively_called = set()
        while True:
            something_added = False
            for clause in program:
                for body_literal in clause.body:
                    if body_literal not in lits_num_clauses:
                        continue
                    if (body_literal != clause.head and clause.is_recursive()) or \
                       (clause.head in recursively_called):
                        something_added |= not body_literal in recursively_called
                        recursively_called.add(body_literal)
            if not something_added: break
        
        all_constraints = []
        program_handle = self.make_program_handle(program)
        for lit in lits_num_clauses.keys() - recursively_called:
            constraints = [Literal('included_program', (program_handle,))]
            for other_lit, num_clauses in lits_num_clauses.items():
                if other_lit == lit: continue
                constraints.append(Literal('num_clauses', (other_lit, num_clauses)))
            num_recursive = lits_num_recursive_clauses[pred]
            constraints.append(Literal('num_recursive', arguments(lit.predicate, num_recursive)))
            
            all_constraints.append(Constraint('redundancy', None, tuple(constraints)))

        return all_constraints