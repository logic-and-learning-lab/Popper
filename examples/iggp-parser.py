game = 'leafy'
exs, bk = [], []

with open(f'iggp-{game}/all.pl') as f:
    for line in f:
        line = line.strip()
        if line.startswith('pos(') or line.startswith('neg('):
            exs.append(line)
        else:
            bk.append(line)
with open(f'iggp-{game}/bk.pl', 'w') as f:
    f.write('\n'.join(bk))

with open(f'iggp-{game}/exs.pl', 'w') as f:
    f.write('\n'.join(exs))