from . import core
from collections import defaultdict

def generate_unordered_program(clingo_model):
    before     = defaultdict(set)
    min_clause = defaultdict(lambda: 0)
    directions = defaultdict(lambda: defaultdict(lambda: core.ArgumentMode.Unknown))
    clause_id_to_body = defaultdict(set)
    clause_id_to_head = {}
    
    for atom in clingo_model:
        if atom.name == 'before':
            clause1 = atom.arguments[0].number
            clause2 = atom.arguments[1].number
            before[clause1].add(clause2)

        if atom.name == 'min_clause':
            clause = atom.arguments[0].number
            min_clause_num = atom.arguments[1].number
            min_clause[clause] = max(min_clause[clause], min_clause_num)

        if atom.name == 'direction':
            pred_name = atom.arguments[0].name
            arg_index = atom.arguments[1].number
            arg_dir_str = atom.arguments[2].name

            if arg_dir_str == 'in':
                arg_dir = core.ArgumentMode.Input
            elif arg_dir_str == 'out':
                arg_dir = core.ArgumentMode.Output
            else:
                raise Exception(f'Unrecognised argument direction "{arg_dir_str}"')
            directions[pred_name][arg_index] = arg_dir
        
        if atom.name == 'head_literal':
            clause_id = atom.arguments[0].number
            predicate = atom.arguments[1].name
            arguments = tuple(core.Variable(chr(ord('A') + arg.number))
                              for arg in atom.arguments[3].arguments)

            head_atom = core.Atom(predicate, arguments)
            clause_id_to_head[clause_id] = head_atom

        if atom.name == 'body_literal':
            clause_id = atom.arguments[0].number
            predicate = atom.arguments[1].name
            arguments = tuple(core.Variable(chr(ord('A') + arg.number))
                              for arg in atom.arguments[3].arguments)
            
            body_atom = core.Atom(predicate, arguments)
            clause_id_to_body[clause_id].add(body_atom)

    # Set modes
    for clause_id, atom in clause_id_to_head.items():
        dirs = tuple(directions[atom.predicate.name][i] 
                     for i in range(atom.arity))
        mode = core.ModeDeclaration(atom.predicate, dirs)
        clause_id_to_head[clause_id] = core.ProgramLiteral(
                                            atom.predicate, atom.arguments,
                                            mode, True)
    
    for clause_id, body in clause_id_to_body.items():
        body_with_dirs = set()
        for atom in body:
            dirs = (directions[atom.predicate.name][i] 
                    for i in range(atom.arity))
            mode = core.ModeDeclaration(atom.predicate, tuple(dirs))
            body_with_dirs.add(core.ProgramLiteral(
                                    atom.predicate, atom.arguments, mode, True))
        clause_id_to_body[clause_id] = body_with_dirs

    clauses = []
    for clause_key in sorted(clause_id_to_head.keys()):
        head = (clause_id_to_head[clause_key],)
        body = tuple(clause_id_to_body[clause_key])
        min_num = min_clause[clause_key]
        clauses.append(core.UnorderedClause(head, body, min_num))

    return core.UnorderedProgram(clauses, before)

def generate_program(solver, number_of_literals):
    solver.update_number_of_literals(number_of_literals)
    clingo_model = solver.get_model()
    
    if clingo_model:
        return generate_unordered_program(clingo_model)
    else:
        return None