import clingo
import clingo.script
from . core import Literal
from clingo import Function, Number, Tuple_
import pkg_resources

# tmp_map = {1:'A', 2:'A,B',3:'A,B,C', 4:'A,B,C,D',5:'A,B,C,D,E', 6:'A,B,C,D,E,F'}
# myvars = ['A','B','C','D','E','F','G','H']
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
    solver = clingo.Control(['-Wnone'])
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


from itertools import permutations, combinations
myvars = ['A','B','C','D','E','F','G','H']

def connected(xs,ys):
    for x in xs:
        for y in ys:
            if x==y:
                return True
    return False

def uses_in_order(xs, ys):
    zs = set(x for x in xs+ys)
    for i in myvars[:len(zs)]:
        if i not in zs:
            return False
    return True

def build_props(arities):
    pairs = []
    for a1 in arities:
        xs = tuple(myvars[:a1])
        xs_set = set(xs)
        for a2 in arities:
            for ys in permutations(myvars,a2):
                if not connected(xs, ys):
                    continue
                if not uses_in_order(xs, ys):
                    continue
                pairs.append((xs, ys))

    props = []
    cons = []
    for xs, ys in pairs:
        xs_set = set(xs)
        ys_set = set(ys)

        left = ''.join(x.lower() for x in xs)
        right = ''.join(y.lower() for y in ys)

        t_left = ','.join(f'T{x}' for x in xs)
        t_right = ','.join(f'T{y}' for y in ys)

        zs = []
        for y in ys:
            if y not in xs_set:
                zs.append('_')
            else:
                zs.append(y)

        atom_left = ','.join(xs)
        atom_right = ','.join(zs)

        if len(xs) == 1:
            t_left += ','
            atom_left += ','
        if len(ys) == 1:
            t_right += ','
            atom_right += ','

        # # IMPLIES NOT
        key = f'{left}_implies_not_{right}'
        l1 = f'prop({key},(P,Q)):- body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not {key}_aux((P,Q)).'
        l2 = f'{key}_aux((P,Q)):- body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})).'
        props.extend([l1, l2])


        con1 = f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})).'
        cons.append(con1)

        if not ys_set.issubset(xs_set):
            continue

        # IMPLIES
        key = f'{left}_implies_{right}'

        if xs == ys:
            l1 = f'prop({key},(P,Q)):- P!=Q, body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})), not {key}_aux((P,Q)).'
            l2 = f'{key}_aux((P,Q)):- P!=Q, body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not holds(Q,({atom_right})).'
        else:
            l1 = f'prop({key},(P,Q)):- body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})), not {key}_aux((P,Q)).'
            l2 = f'{key}_aux((P,Q)):- body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not holds(Q,({atom_right})).'
        props.extend([l1, l2])


        # rule_vars = xs_set | ys_set
        rule_vars = ys_set
        checker = ','.join(f'var_appears_more_than_twice(Rule,{v})' for v in rule_vars)
        con1 = f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})), {checker}.'
        # con1 = f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})).'
        # print(con1)
        cons.append(con1)



    return props, cons




def deduce_bk_cons(settings):
    prog = []
    lookup2 = {k: f'({v})' for k,v in tmp_map.items()}
    lookup1 = {k:v for k,v in lookup2.items()}
    lookup1[1] = '(V0,)'
    body_preds = get_body_preds(settings)

    arities = set()
    for p, a in body_preds:
        arities.add(a)
        arg_str = lookup1[a]
        arg_str2 = lookup2[a]
        rule = f'holds({p},{arg_str}):- {p}{arg_str2}.'
        prog.append(rule)
    prog = '\n'.join(prog)

    with open(settings.bias_file) as f:
        bias = f.read()
    with open(settings.bk_file) as f:
        bk = f.read()

    cons = pkg_resources.resource_string(__name__, "lp/cons.pl").decode()
    bk = bk.replace('\+','not')

    new_props, new_cons = build_props(arities)
    new_props = '\n'.join(new_props)
    new_cons = '\n'.join(new_cons)
    encoding = [cons, prog, bias, bk, TIDY_OUTPUT, new_props]
    encoding = '\n'.join(encoding)
    # print(encoding)
    # with open('bkcons-encoding.pl', 'w') as f:
        # f.write(encoding)
    # exit()
    solver = clingo.Control(['-Wnone'])
    solver.add('base', [], encoding)
    solver.ground([('base', [])])
    out = set()
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                if atom.name == 'prop':
                    out.add(str(atom))
    xs = out
    print('\n'.join(sorted(xs)))
    # print(new_cons)
    settings.deduced_bkcons = '\n'.join(x + '.' for x in xs) + '\n' + new_cons + '\n'
    # settings.deduced_bkcons = '\n'.join(x + '.' for x in xs)
    # print(new_cons)
    # exit()
    # settings.deduced_bkcons = new_cons

# def deduce_bk_cons_aux(cons, prog, bias, bk):

def generate_binary_strings(bit_count):
    binary_strings = []
    def genbin(n, bs=''):
        if len(bs) == n:
            binary_strings.append(bs)
        else:
            genbin(n, bs + '0')
            genbin(n, bs + '1')
    genbin(bit_count)
    return binary_strings


def deduce_recalls(settings):
    # Jan Struyf, Hendrik Blockeel: Query Optimization in Inductive Logic Programming by Reordering Literals. ILP 2003: 329-346

    # recall for a subset of arguments, e.g. when A and C are ground in a call to add(A,B,C)
    counts = {}
    # maximum recall for a predicate symbol
    counts_all = {}

    with open(settings.bk_file) as f:
        bk = f.read()
    solver = clingo.Control(['-Wnone'])
    solver.add('base', [], bk)
    solver.ground([('base', [])])

    for pred, arity in settings.body_preds:
        counts_all[pred] = 0
        counts[pred] = {}
        # we find all facts for a given predicate symbol

        for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
            args = []
            for i in range(arity):
                arg = atom.symbol.arguments[i]
                t = arg.type
                if t == clingo.SymbolType.Number:
                    x = arg.number
                else:
                    x = arg.name
                args.append(x)

            # print('X', pred, args)
            counts_all[pred] +=1
            # x_args = [x[arg] for arg in args]
            # we now enumerate all subsets of possible input/ground arguments
            # for instance, for a predicate symbol p/2 we consider p(10) and p(01), where 1 denotes input
            # note that p(00) is the max recall and p(11) is 1 since it is a boolean check
            binary_strings = generate_binary_strings(arity)[1:-1]

            for var_subset in binary_strings:
                # print('var_subset', var_subset)
                if var_subset not in counts[pred]:
                    counts[pred][var_subset] = {}
                key = []
                value = []
                for i in range(arity):
                    if var_subset[i] == '1':
                        key.append(args[i])
                    else:
                        value.append(args[i])
                key = tuple(key)
                value = tuple(value)
                # print('\t', key, value)
                if key not in counts[pred][var_subset]:
                    counts[pred][var_subset][key] = set()
                counts[pred][var_subset][key].add(value)

    # we now calculate the maximum recall
    all_recalls = {}
    for pred, arity in settings.body_preds:
        d1 = counts[pred]
        all_recalls[(pred, '0'*arity)] = counts_all[pred]
        for args, d2 in d1.items():
            recall = max(len(xs) for xs in d2.values())
            all_recalls[(pred, args)] = recall

    settings.recall = all_recalls

    out = []

    for (pred, key), recall in all_recalls.items():
        if recall > 4:
            continue
        if '1' not in key:
            continue
        arity = len(key)
        args = [f'V{i}' for i in range(arity)]
        args_str = ','.join(args)
        subset = []
        fixer = []

        for x, y in zip(key, args):
            if x == '0':
                subset.append(y)
                fixer.append('_')
            else:
                fixer.append(y)


        subset_str = ','.join(subset)
        fixer_str = ','.join(fixer)
        if len(fixer) == 1:
            fixer_str+= ','

        con2 = f':- body_literal(Rule,{pred},_,({fixer_str})), #count{{{subset_str}: body_literal(Rule,{pred},_,({args_str}))}} > {recall}.'
        out.append(con2)

    # print('hi')
    settings.deduced_bkcons += '\n' + '\n'.join(out)
