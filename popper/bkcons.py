# Code and idea from the papers:
# Andrew Cropper, Céline Hocquette:
# Learning Logic Programs by Discovering Where Not to Search. AAAI 2023: 6289-6296

# Andrew Cropper, Filipe Gouveia, David M. Cerna:
# Honey, I shrunk the hypothesis space (through logical preprocessing). CoRR abs/2506.06739 (2025)

import time
import os
import clingo
import clingo.script
from collections import defaultdict
from itertools import permutations
from multiprocessing import Queue, Manager
from pebble import ProcessPool
from concurrent.futures import TimeoutError

from . util import generate_binary_strings
from clingo import Function, Number, Tuple_

clingo.script.enable_python()
from . import logger
from . import stats
from . recalls import recalls

class suppress_stdout_stderr(object):
    '''
    A context manager for doing a "deep suppression" of stdout and stderr in
    Python, i.e. will suppress all print, even if the print originates in a
    compiled C/Fortran sub-function.
       This will not suppress raised exceptions, since exceptions are printed
    to stderr just before a script exits, and after the context manager has
    exited (at least, I think that is why it lets exceptions through).
    '''
    def __init__(self):
        # Open a pair of null files
        self.null_fds =  [os.open(os.devnull,os.O_RDWR) for x in range(2)]
        # Save the actual stdout (1) and stderr (2) file descriptors.
        self.save_fds = [os.dup(1), os.dup(2)]

    def __enter__(self):
        # Assign the null pointers to stdout and stderr.
        os.dup2(self.null_fds[0],1)
        os.dup2(self.null_fds[1],2)

    def __exit__(self, *_):
        # Re-assign the real stdout/stderr back to (1) and (2)
        os.dup2(self.save_fds[0],1)
        os.dup2(self.save_fds[1],2)
        # Close all file descriptors
        for fd in self.null_fds + self.save_fds:
            os.close(fd)

tmp_map = {}
for i in range(1,20):
    tmp_map[i] = ','.join(f'V{j}' for j in range(i))

TIDY_OUTPUT = """
#defined body_literal/4.
#defined clause_var/2.
#defined var_type/3.
#defined clause/1.
"""

all_myvars = ['A','B','C','D','E','F','G','H']

def canonicalize_vars(*var_seqs):
    """Rename variables to A, B, C... in first-appearance order across all sequences."""
    lookup = {}
    next_var = 0
    result = []
    for vs in var_seqs:
        out = []
        for v in vs:
            if v not in lookup:
                lookup[v] = next_var
                next_var += 1
            out.append(chr(ord('A') + lookup[v]))
        result.append(tuple(out))
    return result

def connected(xs, ys):
    xs = set(xs)
    for y in ys:
        if y in xs:
            return True
    return False

def uses_in_order(xs, ys):
    zs = set(xs) | set(ys)
    for i in all_myvars[:len(zs)]:
        if i not in zs:
            return False
    return True

def build_props(settings, arities):
    myvars = all_myvars[:settings.max_vars]

    def prolog_args(items):
        """Join items as Prolog tuple args; adds trailing comma for arity 1."""
        items = list(items)
        s = ','.join(items)
        return s + ',' if len(items) == 1 else s

    # Build canonical pairs in one pass.
    # xs is always the first a1 vars (a fixed prefix), so ys can introduce at most
    # a2 new consecutive variables — no need to permute the full variable pool.
    pairs = set()
    for a1 in arities:
        xs = tuple(myvars[:a1])
        for a2 in arities:
            candidate_vars = myvars[:min(a1 + a2, settings.max_vars)]
            for ys in permutations(candidate_vars, a2):
                if not connected(xs, ys):
                    continue
                if not uses_in_order(xs, ys):
                    continue
                longer, shorter = sorted([xs, ys], key=len, reverse=True)
                canon = canonicalize_vars(longer, shorter)
                pairs.add((canon[0], canon[1]))

    props = []
    cons = []
    for xs, ys in sorted(pairs):
        xs_set = set(xs)
        ys_set = set(ys)

        left  = ''.join(x.lower() for x in xs)
        right = ''.join(y.lower() for y in ys)

        t_left    = prolog_args(f'T{x}' for x in xs)
        t_right   = prolog_args(f'T{y}' for y in ys)
        atom_left  = prolog_args(xs)
        ys_masked  = [y if y in xs_set else '_' for y in ys]
        atom_right = prolog_args(ys_masked)

        # IMPLIES NOT: P(xs) => not Q(ys)
        key = f'not_{left}_{right}'
        sym_con = 'P<Q,' if xs == ys else ''

        l1 = f'prop({key},(P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not {key}_aux((P,Q)).'
        l2 = f'{key}_aux((P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})).'
        props.extend([l1, l2])
        cons.append(f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})).')

        if not ys_set.issubset(xs_set):
            continue

        # IMPLIES: P(xs) => Q(ys)  (ys vars are a subset of xs vars)
        key = f'{left}_{right}'
        sym_con = 'P!=Q,' if xs == ys else ''

        l1 = f'prop({key},(P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})), not {key}_aux((P,Q)).'
        l2 = f'{key}_aux((P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not holds(Q,({atom_right})).'
        props.extend([l1, l2])

        checker = ','.join(f'valid_var(Rule,{v})' for v in ys_set)
        cons.append(f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})), {checker}.')

    return props, cons

def deduce_recalls(settings, solver):
    # Jan Struyf, Hendrik Blockeel: Query Optimization in Inductive Logic Programming by Reordering Literals. ILP 2003: 329-346
    
    if solver is None:
        return None

    counts = {}
    counts_all = {}

    for pred, arity in settings.body_preds:
        counts_all[pred] = 0
        counts[pred] = {}
        d = counts[pred]
        binary_strings = generate_binary_strings(arity)

        for var_subset in binary_strings:
            d[var_subset] = defaultdict(set)

        for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
            counts_all[pred] +=1

            args = list(map(str, atom.symbol.arguments))

            for var_subset in binary_strings:
                key = []
                value = []
                for i in range(arity):
                    if var_subset[i]:
                        key.append(args[i])
                    else:
                        value.append(args[i])
                key = tuple(key)
                value = tuple(value)
                d[var_subset][key].add(value)

    # we now calculate the maximum recall
    all_recalls = {}
    for pred, arity in settings.body_preds:
        d1 = counts[pred]
        all_recalls[(pred, (0,)*arity)] = counts_all[pred]
        for args, d2 in d1.items():
            recall = max([len(xs) for xs in d2.values()] + [0])
            all_recalls[(pred, args)] = recall

    recalls.update(all_recalls)

    out = []

    for (pred, key), recall in all_recalls.items():
        if recall > 4:
            continue
        arity = len(key)
        args = [f'V{i}' for i in range(arity)]
        args_str = ','.join(args)

        if len(args) == 1:
            args_str +=  ','
        subset = []
        fixer = []

        for x, y in zip(key, args):
            if x == 0:
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

    return out

def deduce_type_cons(settings, solver):
    body_types = settings.body_types

    if len(body_types) == 0 or solver is None:
        return

    type_objects = defaultdict(set)

    for pred, arity in settings.body_preds:
        for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
            args = []
            for i in range(arity):
                arg = atom.symbol.arguments[i]
                x = str(arg)
                arg_type = body_types[pred][i]
                type_objects[arg_type].add(x)

    for k, vs in type_objects.items():
        n = len(vs)
        if n > settings.max_vars:
            continue
        logger.debug(f'max_vars {k} {len(vs)}')
        con = ":- clause(C), #count{V : var_type(C, V," + k + ")} > " + str(n) + "."
        yield con

SINGLETON_ENCODING="""
#show total/2.
#show total2/3.
#show total3/4.
#show total4/5.
not_total(P, I):-
    type(P, I, T),
    domain(T, X), not holds(P,I,X),
    pred(P,_).

total(P, I):-
    type(P, I, _),
    not not_total(P, I),
    pred(P,_).

not_total2(P, I, J):-
    pred(P,A),
    A>2,
    I < J,
    type(P, I, T1),
    type(P, J, T2),
    domain(T1, X),
    domain(T2, Y),
    not holds2(P, I, J, X, Y).

total2(P, I, J):-
    pred(P, A),
    A>2,
    type(P, I, _),
    type(P, J, _),
    I < J,
    not not_total2(P, I, J).

not_total3(P, I, J, K):-
    pred(P,A),
    A>3,
    I < J,
    J < K,
    type(P, I, T1),
    type(P, J, T2),
    type(P, K, T3),
    domain(T1, X),
    domain(T2, Y),
    domain(T3, Z),
    not holds3(P, I, J, K, X, Y, Z).

total3(P, I, J, K):-
    pred(P, A),
    A>3,
    type(P, I, _),
    type(P, J, _),
    type(P, K, _),
    I < J,
    J < K,
    not not_total3(P, I, J, K).

not_total4(P, I1, I2, I3, I4):-
    pred(P,A),
    A>4,
    I1 < I2,
    I2 < I3,
    I3 < I4,
    type(P, I1, T1),
    type(P, I2, T2),
    type(P, I3, T3),
    type(P, I4, T4),
    domain(T1, X1),
    domain(T2, X2),
    domain(T3, X3),
    domain(T4, X4),
    not holds4(P, I1, I2, I3, I4, X1, X2, X3, X4).

total4(P, I1, I2, I3, I4):-
    pred(P,A),
    A>4,
    I1 < I2,
    I2 < I3,
    I3 < I4,
    type(P, I1, T1),
    type(P, I2, T2),
    type(P, I3, T3),
    type(P, I4, T4),
    not not_total4(P, I1, I2, I3, I4).
"""

def _process_total_atoms(solver, signature, arity, pred_lookup, seen, cons):
    n_indices = arity - 1  # number of index arguments after the predicate name
    for atom in solver.symbolic_atoms.by_signature(signature, arity=arity):
        p = str(atom.symbol.arguments[0])
        indices = [int(atom.symbol.arguments[i+1].number) for i in range(n_indices)]
        a = pred_lookup[p]
        args = [f'V{i}' for i in range(a)]
        arg_str = ','.join(args)
        total_vars = {args[idx] for idx in indices}
        singletons_checked = frozenset(v for v in args if v not in total_vars)
        if any(x.issubset(singletons_checked) for x in seen[p]):
            continue
        non_singletons = ','.join(f'singleton({v})' for v in args if v not in total_vars)
        con = f':- body_literal(_, {p}, _, ({arg_str})), {non_singletons}.'
        cons.append(con)
        seen[p].add(singletons_checked)

def deduce_non_singletons(settings):
    encoding = []

    if len(settings.body_types) == 0:
        return []

    for p, a in settings.body_preds:

        if a > 1:
            encoding.append(f'pred({p},{a}).')

        types = settings.body_types[p]
        for i, t in enumerate(types):
            rule = f'domain({t},X):- holds({p},{i},X).'
            encoding.append(rule)
            rule = f'type({p},{i},{t}).'
            encoding.append(rule)

        args = [f'V{i}' for i in range(a)]
        arg_str = ','.join(args)

        for i in range(a):

            rule = f'holds({p},{i},{args[i]}):- {p}({arg_str}).'
            encoding.append(rule)

            x = args[i]
            for j in range(i+1, a):
                y = args[j]
                rule = f'holds2({p},{i},{j},{x},{y}):- {p}({arg_str}).'
                encoding.append(rule)

                for k in range(j+1, a):
                    z = args[k]
                    rule = f'holds3({p},{i},{j},{k},{x},{y},{z}):- {p}({arg_str}).'
                    encoding.append(rule)

                    for i_4 in range(k+1, a):
                        x_4 = args[i_4]
                        rule = f'holds4({p},{i},{j},{k},{i_4},{x},{y},{z},{x_4}):- {p}({arg_str}).'
                        encoding.append(rule)

    encoding = sorted(encoding)
    encoding.append(SINGLETON_ENCODING)

    bk = settings.bk_string

    encoding.append(bk)
    encoding = '\n'.join(encoding)

    solver = clingo.Control(['-Wnone'])
    solver.add('base', [], encoding)
    solver.ground([('base', [])])

    pred_lookup = {p: a for p, a in settings.body_preds}
    seen = defaultdict(set)
    cons = []
    _process_total_atoms(solver, 'total4', 5, pred_lookup, seen, cons)
    _process_total_atoms(solver, 'total3', 4, pred_lookup, seen, cons)
    _process_total_atoms(solver, 'total2', 3, pred_lookup, seen, cons)
    _process_total_atoms(solver, 'total', 2, pred_lookup, seen, cons)
    return cons


def run_deduce_bk_cons(settings, tester):
    timeout = min(settings.timeout, settings.bkcons_timeout)

    manager = Manager()
    shared_results = manager.list()
    with ProcessPool(max_workers=1) as pool:
        future = pool.schedule(deduce_bk_cons_stream, args=(settings, shared_results), timeout=timeout)

        try:
            # 2. YOU MUST WAIT HERE.
            # This blocks the parent until the child is done or timeout hits.
            future.result()
        except TimeoutError:
            logger.info(f"BK cons worker killed by timeout ({timeout}s)")
        except Exception as e:
            logger.error(f"Worker failed before timeout: {e}")

        return list(shared_results)

def deduce_bk_cons_stream(settings, shared_results):
    prog = []

    lookup2 = {k: f'({v})' for k,v in tmp_map.items()}
    lookup1 = {k:v for k,v in lookup2.items()}
    lookup1[1] = '(V0,)'

    head_pred, head_args = settings.head_literal
    head_arity = len(head_args)

    arities = set()

    for p, a in settings.body_preds:
        arities.add(a)
        arg_str = lookup1[a]
        arg_str2 = lookup2[a]

        prog.append(f'holds({p},{arg_str}):- {p}{arg_str2}.')
        prog.append(f'body_pred({p},{a}).')

    # ---- types ----
    if settings.head_types:
        prog.append(f'type({settings.head_literal[0]},{tuple(settings.head_types)}).')

        for pred, types in settings.body_types.items():
            prog.append(f'type({pred},{tuple(types)}).')

    prog = '\n'.join(prog)

    bk = settings.bk_string
    bk = bk.replace('\\+','not')

    # ---- generate props ----
    new_props1, new_cons1 = build_props(settings, arities)

    # ---- solver ----
    solver = clingo.Control(['-Wnone'])

    base_encoding = '\n'.join([prog, bk, TIDY_OUTPUT, '\n'.join(new_props1)])
    solver.add('base', [], base_encoding)
    solver.ground([('base', [])])
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown=True):
                if atom.name == 'prop':
                    s = str(atom)
                    shared_results.append(s + '.')

    shared_results.extend(new_cons1)

def get_bk_cons(settings, tester):
    bkcons = []

    # Read the BK file into memory exactly once
    with open(settings.bk_file) as f:
        settings.bk_string = f.read()

    logger.info(f'Finding pointless rules')
    pointless = settings.pointless = tester.find_pointless_relations()

    for p,a in pointless:
        logger.info(f'Pointless relation: {p}/{a}')
        settings.body_preds.remove((p,a))

    # Create a shared pure-BK solver for symbolic_atoms inspection
    base_solver = clingo.Control(['-Wnone'])
    try:
        with suppress_stdout_stderr():
            base_solver.add('base', [], settings.bk_string)
            base_solver.ground([('base', [])])
    except Exception as Err:
        logger.info('Loading recalls FAILURE')
        settings.datalog = False
        return []

    settings.datalog = True

    logger.info(f'Loading recalls')
    with stats.duration('recalls'):
        recalls = deduce_recalls(settings, base_solver)

    for x in recalls:
        logger.debug(f'recall: {x}')
    bkcons.extend(recalls)


    xs = deduce_non_singletons(settings)
    for x in xs:
        logger.debug(f'singletons {x}')
    bkcons.extend(xs)

    type_cons = tuple(deduce_type_cons(settings, base_solver))
    for x in type_cons:
        logger.debug(f'type_con {x}')
    bkcons.extend(type_cons)

    timeout = min(settings.timeout, settings.bkcons_timeout)
    logger.info(f'Loading BK cons')
    with stats.duration('bkcons'):
        xs = run_deduce_bk_cons(settings, tester)
        for x in xs:
            if x.startswith('prop'):
                logger.debug(f'BKCON {x}')
        bkcons.extend(xs)

    return bkcons