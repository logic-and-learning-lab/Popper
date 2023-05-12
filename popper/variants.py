from itertools import permutations
from itertools import chain, combinations
from . util import format_rule

GROUND_VARIANTS_ENCODING = """\
#show bind_var/2.
1 {bind_var(Var,Value): Value=0..max_vars-1}1:- var(Var), Var >= head_vars.
value_type(Value,Type):- bind_var(Var,Value), var_type(Var,Type).
:- value_type(Value,T1), value_type(Value,T2), T1 != T2.
:- Value=0..max_vars-1, #count{Var : bind_var(Var,Value)} > 1.
"""

GROUND_VARIANTS_ENCODING2 = """\
#show bind_var/2.
1 {bind_var(Var,Value): Value=0..max_vars-1, not type_mismatch(Var,Value)}1:- var(Var), Var >= head_vars.
:- Value=0..max_vars-1, #count{Var : bind_var(Var,Value)} > 1.
type_mismatch(Var,Value):- var_type(Var,T1), known_value_type(Value,T2), T1 != T2.
"""

# def var_to_int(var):
    # return ord(var) - ord('A')

MAX_VARS=100
int_to_var = {i:chr(ord('A') + i) for i in range(0,MAX_VARS)}
var_to_int = {v:i for i,v in int_to_var.items()}

import clingo

cached_find_variants = {}
# @profile
def find_variants_aux(settings, rule):
    head, body = rule

    if head:
        head_vars = frozenset(head.arguments)
    else:
        head_vars = frozenset()
    body_vars = frozenset(x for literal in body for x in literal.arguments)

    # print('find_variants_aux',format_rule(rule))

    k = hash((head_vars, body_vars))
    if k in cached_find_variants:
        return cached_find_variants[k]

    head_types = settings.head_types
    body_types = settings.body_types

    encoding = set()
    encoding.add(GROUND_VARIANTS_ENCODING)
    encoding.add(f'#const max_vars={settings.max_vars}.')
    encoding.add(f'#const head_vars={len(head_vars)}.')

    if head_types:
        for i, head_type in enumerate(head_types):
            encoding.add(f'var_type({i},{head_type}).')
            encoding.add(f'value_type({i},{head_type}).')

    var_lookup = {}
    if head:
        for x in head.arguments:
            k = var_to_int[x]
            encoding.add(f'bind_var({k},{k}).')
            encoding.add(f'var({k}).')
            var_lookup[k] = x

    for literal in body:
        # map the letter to an int
        for i, x in enumerate(literal.arguments):

            if x in head_vars:
                continue

            k = var_to_int[x]
            var_lookup[k] = x
            encoding.add(f'var({k}).')
            if literal.predicate in body_types:
                var_type = body_types[literal.predicate][i]
                encoding.add(f'var_type({k},{var_type}).')


    encoding = '\n'.join(encoding)

    solver = clingo.Control(['-Wnone'])
    solver.configuration.solve.models = 0
    solver.add('base', [], encoding)
    solver.ground([("base", [])])

    out = []

    def on_model(m):
        xs = m.symbols(shown = True)
        assignment = {}
        for x in xs:
            args = x.arguments
            var_var_int = args[0].number
            value = args[1].number
            var_var = var_lookup[var_var_int]
            assignment[var_var] = value
        out.append((var_lookup, assignment))
    solver.solve(on_model=on_model)

    cached_find_variants[k] = out
    return out

# def find_variants_aux2(settings, rule, max_vars=6):
#     head, body = rule

#     if head:
#         head_vars = frozenset(head.arguments)
#     else:
#         head_vars = frozenset()
#     body_vars = frozenset(x for literal in body for x in literal.arguments)

#     k = hash((head_vars, body_vars))
#     # if k in cached_find_variants:
#         # return cached_find_variants[k]

#     head_types = settings.head_types
#     body_types = settings.body_types

#     encoding = set()
#     encoding.add(GROUND_VARIANTS_ENCODING2)
#     encoding.add(f'#const max_vars={max_vars}.')
#     encoding.add(f'#const head_vars={len(head_vars)}.')

#     if head_types:
#         for i, head_type in enumerate(head_types):
#             encoding.add(f'var_type({i},{head_type}).')
#             # encoding.add(f'value_type({i},{head_type}).')
#             encoding.add(f'known_value_type({i},{head_type}).')


#     var_lookup = {}
#     if head:
#         for x in head.arguments:
#             k = var_to_int[x]
#             encoding.add(f'bind_var({k},{k}).')
#             encoding.add(f'var({k}).')
#             var_lookup[k] = x

#     for literal in body:
#         # map the letter to an int
#         for i, x in enumerate(literal.arguments):

#             if x in head_vars:
#                 continue

#             k = var_to_int[x]
#             var_lookup[k] = x
#             encoding.add(f'var({k}).')
#             if literal.predicate in body_types:
#                 var_type = body_types[literal.predicate][i]
#                 encoding.add(f'var_type({k},{var_type}).')


#     encoding = '\n'.join(encoding)

#     # print(format_rule(rule))
#     # print(encoding)
#     # exit()

#     t1 = time.time()
#     solver = clingo.Control(['-Wnone'])
#     solver.configuration.solve.models = 0
#     solver.add('base', [], encoding)
#     solver.ground([("base", [])])

#     out = []

#     def on_model(m):
#         xs = m.symbols(shown = True)
#         assignment = {}
#         for x in xs:
#             args = x.arguments
#             var_var_int = args[0].number
#             value = args[1].number
#             var_var = var_lookup[var_var_int]
#             assignment[var_var] = value
#         out.append((var_lookup, assignment))
#     solver.solve(on_model=on_model)
#     # t2 = time.time()
#     # d1 = t2-t1

#     # global max_time
#     # if d1 > max_time:
#     #     print('*'*30)
#     #     print(d1)
#     #     print(format_rule(rule))
#     #     print('============')
#     #     print(encoding)
#     #     print('*'*30)
#     #     max_time = d1
#     cached_find_variants[k] = out
#     return out

# @profile
def find_variants2(settings, rule):
    head, body = rule
    assignments = find_variants_aux(settings, rule)

    new_rules = []

    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None

    for indexes, assignment in assignments:
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in assignment:
                    new_arg = int_to_var[assignment[arg]]
                    new_args.append(new_arg)
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)

    return new_rules


# @profile
def find_variants(rule, max_vars=6):
    all_vars = 'ABCDEFGHIJKLM'
    all_vars = all_vars[:max_vars]

    head, body = rule
    if head:
        head_arity = head.arity
        head_vars = set(head.arguments)
    else:
        head_arity = 0
        head_vars = set()

    body_vars = frozenset({x for literal in body for x in literal.arguments if x not in head_vars})
    num_body_vars = len(body_vars)
    if head:
        # subset = all_vars[head_arity:head_arity+num_body_vars+1]
        subset = all_vars[head_arity:]
        # subset = all_vars
    else:
        subset = all_vars
    indexes = {x:i for i, x in enumerate(body_vars)}
    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None
    new_rules = []
    # print(body_vars, subset, indexes)
    perms = list(permutations(subset, num_body_vars))
    for xs in perms:
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in indexes:
                    new_args.append(xs[indexes[arg]])
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)


    # print(format_rule(rule))
    # for h, b in new_rules:
    #     print('\t', h, sorted(b))

    # print(len(new_rules), len(set(new_rules)))
    return new_rules



def find_variants3(settings, rule, max_vars=6):
    all_vars = 'ABCDEFGHIJKLM'
    all_vars = all_vars[:max_vars]

    head_types = settings.head_types
    body_types = settings.body_types

    val_to_type = {}
    if head_types:
        val_to_type = {k:head_type for i, head_type in enumerate(head_types)}

    head, body = rule
    if head:
        head_arity = head.arity
        head_vars = set(head.arguments)
    else:
        head_arity = 0
        head_vars = set()


    for literal in body:
        for i, x in enumerate(literal.arguments):
            if x in head_vars:
                continue
            if literal.predicate not in body_types:
                continue
            var_type = body_types[literal.predicate][i]
            encoding.add(f'var_type({k},{var_type}).')

    body_vars = frozenset({x for literal in body for x in literal.arguments if x not in head_vars})
    num_body_vars = len(body_vars)
    if head:
        # subset = all_vars[head_arity:head_arity+num_body_vars+1]
        subset = all_vars[head_arity:]
        # subset = all_vars
    else:
        subset = all_vars
    indexes = {x:i for i, x in enumerate(body_vars)}
    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None
    new_rules = []
    # print(body_vars, subset, indexes)
    for xs in permutations(subset, num_body_vars):
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in indexes:
                    new_args.append(xs[indexes[arg]])
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)
    return new_rules
