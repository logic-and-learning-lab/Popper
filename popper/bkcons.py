import clingo
import clingo.script
from . core import Literal
from clingo import Function, Number, Tuple_

# tmp_map = {1:'A', 2:'A,B',3:'A,B,C', 4:'A,B,C,D',5:'A,B,C,D,E', 6:'A,B,C,D,E,F'}
tmp_map = {}
for i in range(1,20):
    tmp_map[i] = ','.join(f'V{j}' for j in range(i))


# print(tmp_map)
arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}

TIDY_OUTPUT = """
#defined body_literal/4.
#defined clause_var/2.
#defined var_type/3.
#defined clause/1.
"""

def get_body_preds(settings):
    solver = clingo.Control()
    with open(settings.bias_file) as f:
        solver.add('bias', [], f.read())
    solver.add('bias', [], TIDY_OUTPUT)
    solver.ground([('bias', [])])

    for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
        args = x.symbol.arguments
        symbol = args[0].name
        arity = args[1].number
        head_pred = symbol, arity

    head_pred, head_arity=  head_pred
    head_literal = Literal(head_pred, tuple(arg_lookup[clingo.Number(arg)] for arg in range(head_arity)))
    head_str =  f'{head_pred}({tmp_map[head_arity]})'

    body_preds = set()
    for x in solver.symbolic_atoms.by_signature('body_pred', arity=2):
        args = x.symbol.arguments
        symbol = args[0]
        arity = args[1].number
        body_preds.add((symbol, arity))
    return body_preds

def deduce_bk_cons(settings):
    prog = []
    lookup2 = {k: f'({v})' for k,v in tmp_map.items()}
    lookup1 = {k:v for k,v in lookup2.items()}
    lookup1[1] = '(V0,)'
    body_preds = get_body_preds(settings)
    for p,a in body_preds:
        arg_str = lookup1[a]
        arg_str2 = lookup2[a]
        rule = f'holds({p},{arg_str}):- {p}{arg_str2}.'
        prog.append(rule)
    prog = '\n'.join(prog)

    with open(settings.bias_file) as f:
        bias = f.read()
    with open(settings.bk_file) as f:
        bk = f.read()
    with open('popper/lp/cons.pl') as f:
        cons = f.read()

    bk = bk.replace('\+','not')

    xs = deduce_bk_cons_aux(cons, prog, bias, bk)
    # for x in xs:
        # x += '.'
        # print(x)

    settings.bkcons = '\n'.join(x + '.' for x in xs)

    # print('intersection')
    # for x in sorted(list(all_props)):
    #     print(str(x) +  '.')

    # print('counts')
    # for k, vs in all_counts.items():
    #     if len(vs) == 1:
    #         continue
    #     print(f'prop(countk,{k},{max(vs)}).')
    #     # print(str(x) +  '.')

    # exit()

def deduce_bk_cons_aux(cons, prog, bias, bk):
    encoding = [cons, prog, bias, bk, TIDY_OUTPUT]
    encoding = '\n'.join(encoding)
    solver = clingo.Control()
    solver.add('base', [], encoding)
    solver.ground([('base', [])])
    out = set()
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                if atom.name == 'prop':
                    # print(str(atom) + '.')
                    out.add(str(atom))
    return out