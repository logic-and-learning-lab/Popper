def parse_bk():
    fname = 'bk.pl'
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


    for k, xs in out.items():
        print(f'#T{k}')
        for x in xs:
            print(x)
        print('')



def parse_examples():
    fname = 'exs.pl'
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

    for k, xs in out.items():
        print(f'#T{k}')
        for x in xs:
            print(x)
        print('')




# parse_bk()
parse_examples()