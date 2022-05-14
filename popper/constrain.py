from . core import Literal, ConstVar

class Constrainer:
    def __init__(self, settings):
        self.elim_cons = set()
        self.gen_cons = set()
        self.spec_cons = {x: set() for x in settings.pos}

    def add_elimination(self, rule):
        self.elim_cons.add(elimination_constraint(rule))

    def add_specialisation(self, rule, e):
        self.spec_cons[e].add(specialisation_constraint(rule))

def find_all_vars(body):
    all_vars = set()
    for literal in body:
        # print('literal',literal)
        for arg in literal.arguments:
            all_vars.add(arg)
            if isinstance(arg, ConstVar):
                all_vars.add(arg)
            if isinstance(arg, tuple):
                for t_arg in arg:
                    # if isinstance(t_arg, ConstVar):
                    all_vars.add(t_arg)
    return sorted(all_vars)

def specialisation_constraint(rule):
    literals = []
    head, body = rule
    literals.append(Literal('head_literal', (head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
    for body_literal in body:
        literals.append(Literal('body_literal', (body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
    # literals.append(Literal('body_size', (len(body),)))
    # for v1 in body_literal.arguments:
        # for v2 in body_literal
    # print(find_all_vars(body))
    import itertools
    xs = find_all_vars(body)
    for v1,v2 in itertools.combinations(xs, 2):
        vo_variable(v1),
        vo_variable(v2),
        literals.append(Literal('!=', (v1,v2), meta=True))
    # print(format_constraint((None, tuple(literals))))
    # return format_constraint((None, tuple(literals)))
    return None, tuple(literals)

def elimination_constraint(rule):
    literals = []
    head, body = rule
    literals.append(Literal('head_literal', (head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
    for body_literal in body:
        literals.append(Literal('body_literal', (body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
    # k = len(body)
    literals.append(Literal('body_size', (len(body),)))
    # print(format_constraint((None, tuple(literals))))
    import itertools
    xs = find_all_vars(body)
    # print('xs', xs)

    for v1,v2 in itertools.combinations(xs, 2):
        vo_variable(v1),
        vo_variable(v2),
        literals.append(Literal('!=', (v1,v2), meta=True))
        # print('HERE')
    # print(format_constraint((None, tuple(literals))))
    # return format_constraint((None, tuple(literals)))
    return None, tuple(literals)

def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

# def format_constraint(con):
#     head, body = con
#     constraint_literals = []
#     for constobj in body:
#         if not constobj.meta:
#             constraint_literals.append(str(constobj))
#             continue
#         arga, argb = constobj.arguments
#         if isinstance(arga, ConstVar):
#             arga = arga.name
#         else:
#             arga = str(arga)
#         if isinstance(argb, ConstVar):
#             argb = argb.name
#         else:
#             argb = str(argb)
#         constraint_literals.append(f'{arga}{constobj.predicate}{argb}')

#     x = f':- {", ".join(constraint_literals)}.'
#     if head:
#         x = f'{head} {x}'
#     # print(x)
#     return x

def format_constraint(con):
    # print(con)
    head, body = con
    # print(str(head), list(str(x) for x in body))
    constraint_literals = []
    for constobj in body:
        if not constobj.meta:
            constraint_literals.append(str(constobj))
            continue
        # else:
            # print(constobj)
        arga, argb = constobj.arguments
        if isinstance(arga, ConstVar):
            arga = arga.name
        else:
            arga = str(arga)
        if isinstance(argb, ConstVar):
            argb = argb.name
        else:
            argb = str(argb)
        constraint_literals.append(f'{arga}{constobj.predicate}{argb}')

    x = f':- {", ".join(constraint_literals)}.'
    if head:
        x = f'{head} {x}'
    # print(x)
    return x
