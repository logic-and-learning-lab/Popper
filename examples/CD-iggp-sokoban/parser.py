# def parse_bk():
#     fname = 'bk.pl'
#     with open(fname, 'r') as f:
#         out = {}
#         for line in f:
#             line = line.strip()
#             if line == '':
#                 continue
#             xs = line.split('(')
#             pred = xs[0]
#             args = xs[1].split(',')
#             task = args[0]
#             args = ','.join(args[1:])
#             x = f'{pred}({args}'
#             if task not in out:
#                 out[task] = set()
#             out[task].add(x)


#     for k, xs in out.items():
#         for x in xs:
#             print(k, x)



def parse():
    fname = 'bk-raw.pl'
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
            elif any(line.startswith(x) for x in ('player_obj', 'bounds', 'dir', 'is_box', 'is_down', 'is_left', 'is_noop', 'is_right', 'is_up', 'my_succ', 'role', 'object')):
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







# prim(controls/3).
# prim(does/3).
# prim(my_input/3).
# prim(my_true_at/4).
# prim(my_true_target/3).

parse()