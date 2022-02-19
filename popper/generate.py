from . core import Literal
from collections import defaultdict

def gen_args(args):
    return tuple(chr(ord('A') + arg.number) for arg in args)

def generate_program(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    body_atoms = []
    for atom in model:
        if atom.name == 'body_literal':
            pred = atom.arguments[1].name
            args = gen_args(atom.arguments[3].arguments)
            arity = len(args)
            body_modes = tuple(directions[pred][i] for i in range(arity))
            body_atoms.append(Literal(pred, args, body_modes))

        elif atom.name == 'head_literal':
            pred = atom.arguments[1].name
            args = gen_args(atom.arguments[3].arguments)
            arity = len(args)
            head_modes = tuple(directions[pred][i] for i in range(arity))
            head = Literal(pred, args, head_modes)

    return [(head, frozenset(body_atoms))]