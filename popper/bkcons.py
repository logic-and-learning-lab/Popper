def deduce_bk_cons(settings):
    prog = []
    lookup1 = {1:'(A,)', 2:'(A,B)', 3:'(A,B,C)', 4:'(A,B,C,D)'}
    lookup2 = {1:'(A)', 2:'(A,B)', 3:'(A,B,C)', 4:'(A,B,C,D)'}
    for p,a in settings.body_preds:
        arg_str = lookup1[a]
        arg_str2 = lookup2[a]
        rule = f'holds({p},{arg_str}):- {p}{arg_str2}.'
        prog.append(rule)
    prog = '\n'.join(prog)

    with open(settings.bias_file) as f:
        bias = f.read()
    with open(settings.bk_file.replace('bk', 'bk-all')) as f:
        bk_all = f.read()
    with open('popper/lp/cons.pl') as f:
        cons = f.read()

    task_bk = parse_bk(settings, bk_all)

    all_counts = defaultdict(set)
    all_props = None
    for task, bk in task_bk.items():
        xs = deduce_bk_cons_aux(cons, prog, bias, bk)
        for x in xs:
            if 'countk' in str(x):
                k = x.arguments[1]
                v = x.arguments[2]
                all_counts[k].add(v)
        if all_props == None:
            all_props = xs
        else:
            all_props = all_props.intersection(xs)

    print('intersection')
    for x in sorted(list(all_props)):
        print(str(x) +  '.')

    print('counts')
    for k, vs in all_counts.items():
        if len(vs) == 1:
            continue
        print(f'prop(countk,{k},{max(vs)}).')
        # print(str(x) +  '.')

    exit()


def deduce_bk_cons_aux(cons, prog, bias, bk):
    solver = clingo.Control()
    cons_prog = cons + '\n' + prog + '\n' + bias + '\n' + bk
    solver.add('base', [], cons_prog)
    solver.ground([('base', [])])
    out = set()
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                if atom.name == 'prop':
                    # print(str(atom) + '.')
                    out.add(atom)
    return out