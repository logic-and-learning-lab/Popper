from . import core
from collections import defaultdict
from itertools import chain, product

class Constrain:
    def __init__(self, no_pruning = False):
        self.no_pruning  = no_pruning
        self.rule_to_cid = {}
        self.cid_counter = 0
        self.included_clause_handles = set()

    # Specific constraint helper functions
    # @NOTE: The two functions below have a "ground" attribute in the orginal.
    # Ask about it.
    def make_clause_handle(self, clause):
        atoms = [clause.head[0]] + sorted(clause.body)
        def atom_handle(atom):
            variables = (f'{variable}' for variable in atom.arguments)
            return f'{atom.predicate.name}' + ''.join(variables)

        return ''.join(map(atom_handle, atoms))

    def make_program_handle(self, program):
        return ('prog_' +
                '_'.join(sorted(self.make_clause_handle(clause) for clause in program)))

    # Specific constraints
    def generalisation_constraint(self, program):
        for clause_number, clause in enumerate(program):
            clause_handle = self.make_clause_handle(clause)
            ic_arguments = (clause_handle, core.ClauseVariable(f'C{clause_number}'))
            ic_literal = core.Literal(predicate = 'included_clause',
                                      arguments = ic_arguments,
                                      polarity = True)
            yield ic_literal

            cs_arguments = (core.ClauseVariable(f'C{clause_number}'), len(clause.body))
            cs_literal = core.Literal(predicate = 'clause_size',
                                      arguments = cs_arguments,
                                      polarity = True)
            yield cs_literal

        # Use clauses' metadata concerning clause ordering to restrict groundings
        for clause_number1, clause_numbers in program.before.items():
            for clause_number2 in clause_numbers:
                yield core.LT.pos(core.ClauseVariable(f'C{clause_number1}'),
                                  core.ClauseVariable(f'C{clause_number2}'))

        # Use clauses' metadata concerning minimal clause index to restrict groundings.
        for clause_number, clause in enumerate(program):
            yield core.GTEQ.pos(core.ClauseVariable(f'C{clause_number}'), clause.min_num)

        # Ensure only groundings for distinct clauses are generated
        # AC: this is now covered by an ASP constraint
        # for clause_number1, clause_number2 in combinations(range(len(program)), 2):
        #     yield core.NEQ.pos(core.ClauseVariable(f'C{clause_number1}'),
        #                        core.ClauseVariable(f'C{clause_number2}'))

    def banish_constraint(self, program):
        for clause_number, clause in enumerate(program):
            clause_handle = self.make_clause_handle(clause)
            yield core.Literal(predicate = 'included_clause',
                               arguments = (clause_handle, clause_number),
                               polarity  = True)
            yield core.Literal(predicate = 'clause_size',
                               arguments = (clause_number, len(clause.body)),
                               polarity  = True)
        yield core.Literal(predicate = 'clause', arguments = (len(program),),
                           polarity  = False)

    def specialisation_constraint(self, program):
        program_handle = self.make_program_handle(program)
        pos_body = core.Literal(predicate = 'included_program',
                                arguments = (program_handle,),
                                polarity  = True)
        neg_body = core.Literal(predicate = 'clause',
                                arguments = (len(program),),
                                polarity  = False)

        return core.Clause(head = (), body = (pos_body, neg_body))

    # @NOTE: The computational complexity of this function isn't great. Jk: Discuss
    # possible improvements after getting a more detailed specification.
    def redundancy_constraint(self, program):
        preds_num_clauses = defaultdict(int)
        preds_num_recursive_clauses = defaultdict(int)
        for clause in program:
            head, body = clause.head[0], clause.body
            preds_num_clauses[head.predicate] += 1
            if clause.is_recursive():
                preds_num_recursive_clauses[head.predicate] += 1

        recursively_called = set()
        while True:
            something_added = False
            for clause in program:
                head, body = clause.head[0], clause.body
                recursive = clause.is_recursive()
                for body_pred in (body_literal.predicate for body_literal in body):
                    if body_pred not in preds_num_clauses:
                        continue
                    if (body_pred != head.predicate and recursive) or \
                    (head.predicate in recursively_called):
                        something_added |= not body_pred in recursively_called
                        recursively_called.add(body_pred)
            if not something_added: break

        program_handle = self.make_program_handle(program)
        for pred in preds_num_clauses.keys() - recursively_called:
            lits = [core.Literal(predicate = 'included_program',
                                 arguments = (program_handle,),
                                 polarity  = True)]
            for other_pred, num_clauses in preds_num_clauses.items():
                if other_pred == pred: continue
                lits.append(core.Literal(predicate = 'num_clauses',
                                         arguments = (other_pred, num_clauses),
                                         polarity  = True))
            num_recursive = preds_num_recursive_clauses[pred]
            lits.append(core.Literal(predicate = 'num_recursive',
                                     arguments = (pred.name, num_recursive),
                                     polarity  = True))

            yield core.Clause(head = (), body = tuple(lits))

    def derive_constraint_types(self, program, positive_outcome, negative_outcome):
        outcome_to_constraints = {
            ('all', 'none')  : ('banish',),
            ('all', 'some')  : ('generalisation',),
            ('some', 'none') : ('specialisation',),
            ('some', 'some') : ('specialisation', 'generalisation'),
            ('none', 'none') : ('specialisation', 'redundancy'),
            ('none', 'some') : ('specialisation', 'redundancy', 'generalisation')
        }

        if self.no_pruning:
            positive_outcome, negative_outcome = 'all', 'none'
        if negative_outcome == 'all':
            negative_outcome = 'some'

        return outcome_to_constraints[(positive_outcome, negative_outcome)]

    def constraints_from_type(self, program, constraint_type):
        if constraint_type == 'banish':
            cc = core.Clause(head = (),
                             body = tuple(self.banish_constraint(program)))
            return [cc]

        if constraint_type == 'redundancy':
            return list(self.redundancy_constraint(program))

        if constraint_type == 'specialisation':
            return [self.specialisation_constraint(program)]

        if constraint_type == 'generalisation':
            cc = core.Clause(head = (),
                             body = tuple(self.generalisation_constraint(program)))
            return [cc]

        assert False, f'Unrecognized constraint type: {constraint_type}.'

    # @NOTE: Jk: Refactor with better variable names after sorting note below.
    # The way VarVariable and ClauseVariable are used needs to be urgently fixed.
    # Adds unneeded complexity.
    def make_clause_inclusion_rule(self, clause):
        clause_handle = self.make_clause_handle(clause)
        clause_number = core.ClauseVariable('Cl')

        # @NOTE: This function is not necessary. Jk, refactor after getting a
        # clearer specification from Rolf.
        def literals_generator():
            for metapred, lit in chain(product(('head_literal',), clause.head),
                                       product(('body_literal',), clause.body)):
                assert all(isinstance(arg, core.Variable) for arg in lit.arguments)
                yield core.Literal(predicate = metapred,
                                   arguments = (clause_number, lit.predicate.name,
                                                lit.arity,
                                                tuple(core.VarVariable(v.name)
                                                      for v in lit.arguments)),
                                   polarity  = True)

            yield core.GTEQ.pos(clause_number, clause.min_num)

            # Ensure only groundings for distinct variables are used
            # AC: now handled by an ASP constraint
            # for var1, var2 in combinations(clause.all_vars(), 2):
            #     yield core.NEQ.pos(core.VarVariable(var1.name), core.VarVariable(var2.name))

            for idx, var in enumerate(clause.head[0].arguments):
                yield core.EQ.pos(core.VarVariable(var.name), idx)

        head = (core.Literal(predicate = 'included_clause',
                             arguments = (clause_handle, clause_number),
                             polarity  = True),)

        return clause_handle, core.Clause(head = head, body = tuple(literals_generator()))

    def make_program_inclusion_rule(self, program):
        program_handle = self.make_program_handle(program)

        def literals_generator():
            for clause_number, clause in enumerate(program):
                clause_handle = self.make_clause_handle(clause)
                clause_variable = core.ClauseVariable(f'C{clause_number}')
                yield core.Literal(predicate = 'included_clause',
                                   arguments = (clause_handle, clause_variable),
                                   polarity  = True)

            for clause_number1, clause_numbers in program.before.items():
                for clause_number2 in clause_numbers:
                    yield core.LT.pos(core.ClauseVariable(f'C{clause_number1}'),
                                      core.ClauseVariable(f'C{clause_number2}'))

            # Ensure only groundings for distinct clauses are used
            # AC: now handled by an ASP constraint
            # for clause_number1, clause_number2 in combinations(range(len(program)), 2):
            #     yield core.NEQ.pos(core.ClauseVariable(f'C{clause_number1}'),
            #                        core.ClauseVariable(f'C{clause_number2}'))

        head = (core.Literal(predicate = 'included_program',
                             arguments = (program_handle,),
                             polarity  = True),)

        return program_handle, core.Clause(head = head, body = tuple(literals_generator()))

    def derive_inclusion_rules(self, program, constraint_types):
        if 'specialisation' in constraint_types or 'generalisation' in constraint_types:
            for clause in program:
                clause_handle, rule = self.make_clause_inclusion_rule(clause)
                if clause_handle not in self.included_clause_handles:
                    self.included_clause_handles.add(clause_handle)
                    yield ('inclusion_rule', clause_handle, rule)

        if 'redundancy' in constraint_types or 'specialisation' in constraint_types:
            program_handle, rule = self.make_program_inclusion_rule(program)
            yield ('inclusion_rule', program_handle, rule)

    def derive_constraints(self, program, constraint_types):
        named_constraints = []
        for constraint_type in constraint_types:
            for constraint in self.constraints_from_type(program, constraint_type):
                if constraint in self.rule_to_cid:
                    name = self.rule_to_cid[constraint]
                else:
                    name = f'{constraint_type}{self.cid_counter}'
                    self.cid_counter += 1
                    self.rule_to_cid[constraint] = name
                named_constraints.append((constraint_type, name, constraint))

        return named_constraints

    def map_ctypes_and_inclusion_rules(self, constraint_types):
        constraints = []
        inclusion_rules = []
        for program, program_constraint_types in constraint_types.items():
            # @NOTE: Why "+=" instead of append()?
            constraints += self.derive_constraints(program, program_constraint_types)
            inclusion_rules += self.derive_inclusion_rules(program, program_constraint_types)

        return ((name, rule) for _, name, rule in chain(inclusion_rules, constraints))

    def constrain_solver(self, solver, program_outcomes):
        constraint_types = {}
        for program, (positive_outcome, negative_outcome) in program_outcomes.items():
            constraint_types[program] = self.derive_constraint_types(program, positive_outcome, negative_outcome)

        named_constraints = self.map_ctypes_and_inclusion_rules(constraint_types)
        self.impose(solver, named_constraints)

    def impose(self, solver, named_constraints):
        for name, constraint in named_constraints:
            if name not in solver.added:
                solver.ground(constraint, name)