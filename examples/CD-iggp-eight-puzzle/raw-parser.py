def parse():
    fname = 'raw.pl'
    pos = set()
    neg = set()
    bk = set()
    bk_all = set()
    with open(fname, 'r') as f:
        out = {}
        for line in f:
            line = line.strip()
            if line.startswith('pos'):
                pos.add(line)
            elif line.startswith('neg'):
                neg.add(line)
            elif any(line.startswith(x) for x in ('agent', 'pos', 'cell_type', 'time_step', 'score_int', 'my_index', 'my_input_move', 'role', 'scoremap', 'my_succ', 'my_successor', 'tile')):
                xs = line.split('(')
                pred = xs[0]
                args = xs[1].split(',')
                task = args[0]
                args = ','.join(args[1:])
                x = f'{pred}({args}'
                bk_all.add(x)
            else:
                bk.add(line)
    with open('exs1.pl', 'w') as f:
        f.write('\n'.join(pos | neg))
    with open('bk1.pl', 'w') as f:
        f.write('\n'.join(bk))
    with open('bk-all.pl', 'w') as f:
        f.write('\n'.join(bk_all))




# agent(robot).
# pos(1).
# cell_type(4).
# time_step(0).
# score_int(60).
# index(1).
# input_move(robot, 1, 1).
# role(robot).
# scoremap(26, 100).
# succ(1, 2).
# successor(0, 1).
# tile(8).
# tile(b).


# prim(controls/3).
# prim(does/3).
# prim(my_input/3).
# prim(my_true_at/4).
# prim(my_true_target/3).

parse()