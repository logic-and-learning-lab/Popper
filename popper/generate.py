from . import core
from collections import defaultdict, namedtuple

def generate_unordered_program(clingo_model):
    before     = defaultdict(set)
    min_clause = defaultdict(lambda: 0)
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    clause_id_to_body = defaultdict(set)
    clause_id_to_head = {}
    TempLiteral = namedtuple('TempLiteral', ['predicate', 'arguments', 'arity'])

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
                arg_dir = '+'
            elif arg_dir_str == 'out':
                arg_dir = '-'
            else:
                raise Exception(f'Unrecognised argument direction "{arg_dir_str}"')
            directions[pred_name][arg_index] = arg_dir

        if atom.name == 'head_literal':
            clause_id = atom.arguments[0].number
            predicate = atom.arguments[1].name
            arguments = tuple(chr(ord('A') + arg.number) for arg in atom.arguments[3].arguments)
            head_literal = TempLiteral(predicate, arguments, len(arguments))            
            clause_id_to_head[clause_id] = head_literal

        if atom.name == 'body_literal':
            clause_id = atom.arguments[0].number
            predicate = atom.arguments[1].name
            arguments = tuple(chr(ord('A') + arg.number) for arg in atom.arguments[3].arguments)
            body_literal = TempLiteral(predicate, arguments, len(arguments))
            clause_id_to_body[clause_id].add(body_literal)

    # Set modes
    for clause_id in clause_id_to_head.keys():
        literal = clause_id_to_head[clause_id]
        mode = tuple(directions[literal.predicate][i] for i in range(literal.arity))
        program_literal = core.Literal(literal.predicate, literal.arguments, mode)
        clause_id_to_head[clause_id] = program_literal

    for clause_id, body_literals in clause_id_to_body.items():
        body_with_modes = set()
        for literal in body_literals:
            mode = tuple(directions[literal.predicate][i] for i in range(literal.arity))
            program_literal = core.Literal(literal.predicate, literal.arguments, mode)
            body_with_modes.add(program_literal)
        clause_id_to_body[clause_id] = body_with_modes

    unordered_clauses = []
    for clause_id in sorted(clause_id_to_head.keys()):
        head_literal  = clause_id_to_head[clause_id]
        body_literals = tuple(clause_id_to_body[clause_id])
        min_num = min_clause[clause_id]
        unordered_clauses.append(core.Clause(head_literal, body_literals, min_num))

    return core.Program(unordered_clauses, before)

def generate_program(solver):
    clingo_model = solver.get_model()
    if clingo_model:
        return generate_unordered_program(clingo_model)
    return None