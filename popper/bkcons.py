# Code and idea from the papers:
# Andrew Cropper, Céline Hocquette:
# Learning Logic Programs by Discovering Where Not to Search. AAAI 2023: 6289-6296

# Andrew Cropper, Filipe Gouveia, David M. Cerna:
# Honey, I shrunk the hypothesis space (through logical preprocessing). CoRR abs/2506.06739 (2025)

import os
import string
import clingo
import clingo.script
from collections import defaultdict
from itertools import permutations, combinations, product
from multiprocessing import Manager
from pebble import ProcessPool
from concurrent.futures import TimeoutError

from . util import generate_binary_strings
from . import logger
from . import stats
from . recalls import recalls

clingo.script.enable_python()

class suppress_stdout_stderr:
    """A context manager for deep suppression of stdout and stderr (including C/Fortran)."""
    def __init__(self):
        self.null_fds = [os.open(os.devnull, os.O_RDWR) for _ in range(2)]
        self.save_fds = [os.dup(1), os.dup(2)]

    def __enter__(self):
        os.dup2(self.null_fds[0], 1)
        os.dup2(self.null_fds[1], 2)

    def __exit__(self, *_):
        os.dup2(self.save_fds[0], 1)
        os.dup2(self.save_fds[1], 2)
        for fd in self.null_fds + self.save_fds:
            os.close(fd)

def format_tuple(items):
    """Format items as a Clingo/Prolog tuple, e.g., (V0,V1) or (V0,)."""
    items = list(items)
    s = ','.join(map(str, items))
    return f'({s},)' if len(items) == 1 else f'({s})'

def format_args(items):
    """Format items as a Clingo/Prolog argument list, e.g., (V0,V1)."""
    return f"({','.join(map(str, items))})"

def canonicalize_vars(*var_seqs):
    """Rename variables to A, B, C... in first-appearance order across all sequences."""
    lookup = {}
    def get_var(v):
        if v not in lookup:
            lookup[v] = string.ascii_uppercase[len(lookup)]
        return lookup[v]
    return [tuple(get_var(v) for v in vs) for vs in var_seqs]

def connected(xs, ys):
    """Check if two variable sequences share at least one variable."""
    return bool(set(xs) & set(ys))

def uses_in_order(xs, ys):
    """Check if variables are used in order (A, B, C...) without gaps."""
    zs = set(xs) | set(ys)
    return all(string.ascii_uppercase[i] in zs for i in range(len(zs)))

TIDY_OUTPUT = """
#defined body_literal/4.
#defined clause_var/2.
#defined var_type/3.
#defined clause/1.
"""

def build_props(settings, arities):
    """Generate potential properties (functional dependencies, etc.) between predicates."""
    myvars = tuple(string.ascii_uppercase[:settings.max_vars])

    pairs = set()
    for a1 in arities:
        xs = tuple(myvars[:a1])
        for a2 in arities:
            candidate_vars = myvars[:min(a1 + a2, settings.max_vars)]
            for ys in permutations(candidate_vars, a2):
                if not connected(xs, ys) or not uses_in_order(xs, ys):
                    continue
                
                # Canonicalize to handle symmetries
                longer, shorter = sorted([xs, ys], key=len, reverse=True)
                canon = canonicalize_vars(longer, shorter)
                pairs.add((canon[0], canon[1]))

    props, cons = [], []
    for xs, ys in sorted(pairs):
        xs_set, ys_set = set(xs), set(ys)
        left, right = ''.join(x.lower() for x in xs), ''.join(y.lower() for y in ys)

        t_left = format_tuple(f'T{x}' for x in xs)[1:-1]
        t_right = format_tuple(f'T{y}' for y in ys)[1:-1]
        atom_left = format_tuple(xs)[1:-1]
        ys_masked = [y if y in xs_set else '_' for y in ys]
        atom_right = format_tuple(ys_masked)[1:-1]

        # IMPLIES NOT: P(xs) => not Q(ys)
        key = f'not_{left}_{right}'
        sym_con = 'P<Q,' if xs == ys else ''

        l1 = f'prop({key},(P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not {key}_aux((P,Q)).'
        l2 = f'{key}_aux((P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})).'
        props.extend([l1, l2])
        cons.append(f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})).')

        if not ys_set.issubset(xs_set):
            continue

        # IMPLIES: P(xs) => Q(ys) (only when ys variables are a subset of xs variables)
        key = f'{left}_{right}'
        sym_con = 'P!=Q,' if xs == ys else ''

        l1 = f'prop({key},(P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), holds(Q,({atom_right})), not {key}_aux((P,Q)).'
        l2 = f'{key}_aux((P,Q)):- {sym_con} body_pred(P,{len(xs)}), body_pred(Q,{len(ys)}), type(P,({t_left})), type(Q,({t_right})), holds(P,({atom_left})), not holds(Q,({atom_right})).'
        props.extend([l1, l2])

        checker = ','.join(f'valid_var(Rule,{v})' for v in ys_set)
        cons.append(f':- prop({key},(P,Q)), body_literal(Rule,P,_,({atom_left})), body_literal(Rule,Q,_,({atom_right})), {checker}.')

    return props, cons

def deduce_recalls(settings, solver):
    """Deduce maximum recalls for body predicates to prune search space."""
    if solver is None:
        return []

    counts = defaultdict(lambda: defaultdict(lambda: defaultdict(set)))
    counts_all = defaultdict(int)

    for pred, arity in settings.body_preds:
        binary_strings = generate_binary_strings(arity)
        for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
            counts_all[pred] += 1
            args = [str(arg) for arg in atom.symbol.arguments]
            for var_subset in binary_strings:
                key = tuple(args[i] for i, bit in enumerate(var_subset) if bit)
                val = tuple(args[i] for i, bit in enumerate(var_subset) if not bit)
                counts[pred][var_subset][key].add(val)

    all_recalls = {}
    for pred, arity in settings.body_preds:
        all_recalls[(pred, (0,) * arity)] = counts_all[pred]
        for var_subset, key_map in counts[pred].items():
            recall = max((len(vals) for vals in key_map.values()), default=0)
            all_recalls[(pred, var_subset)] = recall

    recalls.update(all_recalls)

    out = []
    for (pred, key), recall in all_recalls.items():
        if recall > 4:
            continue
        arity = len(key)
        args = [f'V{i}' for i in range(arity)]
        subset_str = ','.join(v for v, k in zip(args, key) if k == 0)
        fixer_str = format_tuple(v if k else '_' for v, k in zip(args, key))[1:-1]
        args_str = format_tuple(args)[1:-1]
        
        con = f':- body_literal(Rule,{pred},_,({fixer_str})), #count{{{subset_str}: body_literal(Rule,{pred},_,({args_str}))}} > {recall}.'
        out.append(con)
    return out

def deduce_type_cons(settings, solver):
    """Deduce constraints based on the number of objects of each type."""
    if not settings.body_types or solver is None:
        return []

    type_objects = defaultdict(set)
    for pred, arity in settings.body_preds:
        for atom in solver.symbolic_atoms.by_signature(pred, arity=arity):
            for i, arg in enumerate(atom.symbol.arguments):
                arg_type = settings.body_types[pred][i]
                type_objects[arg_type].add(str(arg))

    cons = []
    for arg_type, objects in type_objects.items():
        n = len(objects)
        if n <= settings.max_vars:
            logger.debug(f'max_vars {arg_type} {n}')
            cons.append(f":- clause(C), #count{{V : var_type(C, V, {arg_type})}} > {n}.")
    return cons

def generate_singleton_encoding(max_size=4):
    """Programmatically generate the ASP encoding for finding total argument combinations."""
    lines = []
    for n in range(1, max_size + 1):
        suffix = str(n) if n > 1 else ""
        lines.append(f"#show total{suffix}/{n+1}.")

    for n in range(1, max_size + 1):
        suffix = str(n) if n > 1 else ""
        indices = [f"I{i}" for i in range(1, n + 1)]
        types = [f"T{i}" for i in range(1, n + 1)]
        vars = [f"X{i}" for i in range(1, n + 1)]
        
        idx_str = ", ".join(indices)
        type_str = ", ".join(f"type(P, {idx}, {t})" for idx, t in zip(indices, types))
        domain_str = ", ".join(f"domain({t}, {v})" for t, v in zip(types, vars))
        order_str = ", ".join(f"{indices[i]} < {indices[i+1]}" for i in range(n-1))
        if order_str: order_str += ", "
        
        pred_cond = f"pred(P, A), A > {n}, " if n > 1 else "pred(P, _), "
        
        # not_totalN: There exists a combination of domain values not present in the predicate
        not_total_rule = (f"not_total{suffix}(P, {idx_str}) :- {pred_cond}{order_str}{type_str}, "
                         f"{domain_str}, not holds{suffix}(P, {idx_str}, {', '.join(vars)}).")
        lines.append(not_total_rule)
        
        # totalN: For all domain values, a matching tuple exists in the predicate
        type_simple_str = ", ".join(f"type(P, {idx}, _)" for idx in indices)
        total_rule = (f"total{suffix}(P, {idx_str}) :- {pred_cond}{type_simple_str}, "
                      f"{order_str}not not_total{suffix}(P, {idx_str}).")
        lines.append(total_rule)
        
    return "\n".join(lines)

def _process_total_atoms(solver, signature, arity, pred_lookup, seen, cons):
    """Extract singleton constraints from grounded 'total' atoms."""
    for atom in solver.symbolic_atoms.by_signature(signature, arity=arity):
        p = str(atom.symbol.arguments[0])
        indices = [int(arg.number) for arg in atom.symbol.arguments[1:]]
        a = pred_lookup[p]
        args = [f'V{i}' for i in range(a)]
        total_vars = {args[idx] for idx in indices}
        singletons_checked = frozenset(v for v in args if v not in total_vars)
        
        if not singletons_checked or any(x.issubset(singletons_checked) for x in seen[p]):
            continue
            
        non_singletons = ','.join(f'singleton({v})' for v in singletons_checked)
        con = f':- body_literal(_, {p}, _, {format_args(args)}), {non_singletons}.'
        cons.append(con)
        seen[p].add(singletons_checked)

def deduce_non_singletons(settings):
    """Find combinations of arguments that cover the whole domain to prune redundant rules."""
    if not settings.body_types:
        return []

    max_arity = max((a for p, a in settings.body_preds), default=0)
    max_size = min(max_arity, 4) # Limit to 4 as in the original code
    
    encoding = []
    for p, a in settings.body_preds:
        if a > 1:
            encoding.append(f'pred({p},{a}).')

        for i, t in enumerate(settings.body_types[p]):
            encoding.append(f'domain({t},X):- holds({p},{i},X).')
            encoding.append(f'type({p},{i},{t}).')

        args = [f'V{i}' for i in range(a)]
        arg_str = ','.join(args)
        
        for size in range(1, max_size + 1):
            pred_name = f"holds{size}" if size > 1 else "holds"
            for indices in combinations(range(a), size):
                idx_str = ','.join(map(str, indices))
                val_str = ','.join(args[i] for i in indices)
                encoding.append(f'{pred_name}({p},{idx_str},{val_str}):- {p}({arg_str}).')

    encoding.extend([generate_singleton_encoding(max_size), settings.bk_string])
    solver = clingo.Control(['-Wnone'])
    solver.add('base', [], '\n'.join(encoding))
    solver.ground([('base', [])])

    pred_lookup = {p: a for p, a in settings.body_preds}
    seen, cons = defaultdict(set), []
    for n in range(max_size, 0, -1):
        suffix = str(n) if n > 1 else ""
        _process_total_atoms(solver, f'total{suffix}', n + 1, pred_lookup, seen, cons)
    return cons


def run_deduce_bk_cons(settings, shared_results):
    """Entry point for parallel BK constraint deduction."""
    timeout = min(settings.timeout, settings.bkcons_timeout)
    with ProcessPool(max_workers=1) as pool:
        future = pool.schedule(deduce_bk_cons_stream, args=(settings, shared_results), timeout=timeout)
        try:
            future.result()
        except TimeoutError:
            logger.info(f"BK cons worker killed by timeout ({timeout}s)")
        except Exception as e:
            logger.error(f"Worker failed before timeout: {e}")

def deduce_bk_cons_stream(settings, shared_results):
    """Deduce properties (props) in a separate process."""
    prog = []
    arities = set()
    for p, a in settings.body_preds:
        arities.add(a)
        prog.append(f'holds({p},{format_tuple([f"V{i}" for i in range(a)])}):- {p}{format_args([f"V{i}" for i in range(a)])}.')
        prog.append(f'body_pred({p},{a}).')

    if settings.head_types:
        prog.append(f'type({settings.head_literal[0]},{format_tuple(settings.head_types)}).')
        for pred, types in settings.body_types.items():
            prog.append(f'type({pred},{format_tuple(types)}).')

    new_props, new_cons = build_props(settings, arities)
    solver = clingo.Control(['-Wnone'])
    base_encoding = '\n'.join(prog + [settings.bk_string, TIDY_OUTPUT] + new_props)
    solver.add('base', [], base_encoding)
    solver.ground([('base', [])])
    
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown=True):
                if atom.name == 'prop':
                    shared_results.append(str(atom) + '.')
    shared_results.extend(new_cons)

def get_bk_cons(settings, tester):
    """Main function to gather all BK-derived constraints."""
    with open(settings.bk_file) as f:
        settings.bk_string = f.read()

    logger.info(f'Finding pointless rules')
    pointless = settings.pointless = tester.find_pointless_relations()
    for p, a in pointless:
        logger.info(f'Pointless relation: {p}/{a}')
        settings.body_preds.remove((p, a))

    base_solver = clingo.Control(['-Wnone'])
    try:
        with suppress_stdout_stderr():
            base_solver.add('base', [], settings.bk_string)
            base_solver.ground([('base', [])])
    except Exception:
        logger.info('Loading recalls FAILURE')
        settings.datalog = False
        return []

    settings.datalog = True
    bkcons = []

    logger.info(f'Loading recalls')
    with stats.duration('recalls'):
        recalls_cons = deduce_recalls(settings, base_solver)
    for x in recalls_cons:
        logger.debug(f'recall: {x}')
    bkcons.extend(recalls_cons)

    singletons = deduce_non_singletons(settings)
    for x in singletons:
        logger.debug(f'singletons {x}')
    bkcons.extend(singletons)

    type_cons = deduce_type_cons(settings, base_solver)
    for x in type_cons:
        logger.debug(f'type_con {x}')
    bkcons.extend(type_cons)


    logger.info(f'Loading BK cons')
    with stats.duration('bkcons'):
        manager = Manager()
        shared_results = manager.list()
        run_deduce_bk_cons(settings, shared_results)
        results = tuple(shared_results)
        for x in results:
            if x.startswith('prop'): logger.debug(f'BKCON {x}')
        bkcons.extend(results)

    return bkcons
