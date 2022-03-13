def parse_bk():
    fname = 'bk1.pl'
    with open(fname, 'r') as f:
        out = {}
        for line in f:
            line = line.strip()
            if line == '':
                continue
            xs = line.split('(')
            pred = xs[0]
            args = xs[1].split(',')
            task = args[0]
            args = ','.join(args[1:])
            x = f'{pred}({args}'
            if task not in out:
                out[task] = set()
            out[task].add(x)

    with open('bk.pl', 'w') as f:
        for k, xs in out.items():

            f.write(f'#T{k}\n')
            f.write('\n'.join(xs) + '\n\n')

def parse_examples():
    fname = 'exs1.pl'
    with open(fname, 'r') as f:
        out = {}
        for line in f:
            line = line.strip()
            if line == '':
                continue
            xs = line.split('(')
            if line.startswith('pos') or line.startswith('neg'):
                sign, sym, args = line.split('(')
                xs = args.split(',')
                task = xs[0]
                args = xs[1:]
                args = ','.join(args)
                x = '('.join([sign, sym, args])
                if task not in out:
                    out[task] = set()
                out[task].add(x)
                # pred = xs[0]
                # args = xs[1].split(',')
                # task = args[0]
                # args = ','.join(args[1:])
                # x = f'{pred}({args}'
                # if task not in out:
                    # out[task] = set()
                # out[task].add(x)

    with open('exs.pl', 'w') as f:
        for k, xs in out.items():
            f.write(f'#T{k}\n')
            f.write('\n'.join(xs) + '\n\n')




parse_bk()
parse_examples()