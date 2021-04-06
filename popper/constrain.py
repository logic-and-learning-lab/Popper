from . import core
from itertools import combinations

RULE_TO_CID = {}
CID_COUNTER = 0

# -----------------------------------------------------------------------------
# Specific constraint functions
# @NOTE: This function has a "ground" attribute in the orginal. Ask about it. 
def make_clause_handle(clause):
    atoms = [clause.head[0]] + sorted(clause.body)
    def atom_handle(atom):
        variables = (f'{variable}' for variable in atom.arguments)
        return f'{atom.predicate.name}' + ''.join(variables)

    return ''.join(map(atom_handle, atoms))

# Contraints implementation
def generalisation_constraint(program):
    for clause_number, clause in enumerate(program):
        #print(clause_number, clause)
        clause_handle = make_clause_handle(clause)
        ic_arguments = (clause_handle, core.Variable(f'C{clause_number}'))
        ic_literal = core.Literal(predicate = 'included_clause', 
                                  arguments = ic_arguments,
                                  polarity = True)
        cs_arguments = (core.Variable(f'C{clause_number}'), len(clause.body))
        cs_literal = core.Literal(predicate = 'clause_size',
                                  arguments = cs_arguments,
                                  polarity = True)
        
        yield ic_literal
        yield cs_literal
    
    # Use clauses' metadata concerning clause ordering to restrict groundings
    for clause_number1, clause_numbers in program.before.items():
        for clause_number2 in clause_numbers:
            yield core.LT.pos(core.Variable(f'C{cl_num1}'), 
                              core.Variable(f'C{cl_num2}'))
    # Use clauses' metadata concerning minimal clause index to restrict 
    # groundings.
    for clause_number, clause in enumerate(program):
        yield core.GTEQ.pos(core.Variable(f'C{clause_number}'), clause.min_num)    
        
    # Ensure only groundings for distinct clauses are generated    
    for clause_number1, clause_number2 in combinations(range(len(program)), 2):
        yield core.NEQ.pos(core.Variable(f'C{clause_number1}'), 
                           core.Variable(f'C{clause_number2}'))

# -----------------------------------------------------------------------------
def derive_constraint_types(program, positive_outcome, negative_outcome, no_pruning):
    outcome_to_constraints = {
        ('all', 'none')  : ('banish',),
        ('all', 'some')  : ('generalisation',),
        ('some', 'none') : ('specialisation',),
        ('some', 'some') : ('specialisation', 'generalisation'),
        ('none', 'none') : ('specialisation', 'redundancy'),
        ('none', 'some') : ('specialisation', 'redundancy', 'generalisation')
    }
    
    if no_pruning:
        positive_outcome, negative_outcome = 'all', 'none'
    if negative_outcome == 'all':
        negative_outcome = 'some'
    
    return outcome_to_constraints[(positive_outcome, negative_outcome)]

def constraints_from_type(program, constraint_type):
    #if constraint_type == 'banish': return 
    #if constraint_type == 'redundancy': return 
    #if constraint_type == 'specialisation': return 
    if constraint_type == 'generalisation': 
        cc = core.Clause(head = (), 
                         body = tuple(generalisation_constraint(program)))
        return [cc]
    
    assert False, f'Unrecognized constraint type: {constraint_type}.'
    
def derive_constraints(program, constraint_types):
    global CID_COUNTER
    global RULE_TO_CID
    named_constraints = []
    for constraint_type in constraint_types:
        for constraint in constraints_from_type(program, constraint_type):
            if constraint in RULE_TO_CID:
                name = RULE_TO_CID[constraint]
            else:
                name = f'{constraint_type}{CID_COUNTER}'
                CID_COUNTER += 1
                RULE_TO_CID[constraint] = name
            named_constraints.append((constraint_type, name, constraint))

    return named_constraints

def map_ctypes_and_inclusion_rules(constraint_types):
    constraints = []
    inclusion_rules = []
    for program, program_constraint_types in constraint_types.items():
        constraints += derive_constraints(program, program_constraint_types)
        # @NOTE: Continue here

def constrain(solver, program_outcomes, no_pruning = False):
    constraint_types = {}
    for program, (positive_outcome, negative_outcome) in program_outcomes.items():
        constraint_types[program] = \
        derive_constraint_types(program, positive_outcome, negative_outcome, 
                                no_pruning)
    
    map_ctypes_and_inclusion_rules(constraint_types)