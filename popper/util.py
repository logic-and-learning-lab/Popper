from functools import cache
import clingo
import clingo.script
import signal
import argparse
import os
from . import logger
from itertools import permutations, chain, combinations
from collections import defaultdict
from typing import NamedTuple
from . recalls import recalls
import logging

class Literal(NamedTuple):
    predicate: str
    arguments: tuple

TIMEOUT=3600
MAX_VARS=6
MAX_BODY=10
ANYTIME_TIMEOUT=10
BKCONS_TIMEOUT=10
BATCH_SIZE=1000

class Constraint:
    GENERALISATION = 1
    SPECIALISATION = 2
    UNSAT = 3
    REDUNDANCY_CONSTRAINT1 = 4
    REDUNDANCY_CONSTRAINT2 = 5
    TMP_ANDY = 6
    BANISH = 7

def parse_args():
    parser = argparse.ArgumentParser(description='Popper is an ILP system based on learning from failures')
    parser.add_argument('kbpath', help='Path to files to learn from')
    parser.add_argument('--noisy', '-n', default=False, action='store_true', help='Use a noisy (MDL) cost function (default: False)')
    parser.add_argument('--timeout', type=float, default=TIMEOUT, help=f'Overall timeout in seconds (default: {TIMEOUT})')
    parser.add_argument('--max-body', type=int, default=MAX_BODY, help=f'Maximum number of body literals allowed in rule (default: {MAX_BODY})')
    parser.add_argument('--max-vars', type=int, default=MAX_VARS, help=f'Maximum number of variables allowed in rule (default: {MAX_VARS})')
    parser.add_argument('--nuwls', default=False, action='store_true', help='Use nuwls solver (default: False)')
    # parser.add_argument('-v', action='count', default=1, help='Increase verbosity (-v, -vv, or -vvv)')
    parser.add_argument('-v', action='count', default=1, dest='verbosity', help='Increase verbosity (-v, -vv, or -vvv)')
    return parser.parse_args()

def timeout(settings, func, args=(), kwargs={}, timeout_duration=1):
    timeout_duration = max(timeout_duration, 1)
    result = None
    class TimeoutError(Exception):
        pass

    def handler(signum, frame):
        raise TimeoutError()

    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout_duration)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as _exc:
        logger.out(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
        return result
    except AttributeError as moo:
        if '_SolveEventHandler' in str(moo):
            logger.out(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
            return result
        raise moo
    finally:
        signal.alarm(0)

    return result

def load_kbpath(kbpath):
    def fix_path(filename):
        full_filename = os.path.join(kbpath, filename)
        return full_filename.replace('\\', '\\\\') if os.name == 'nt' else full_filename
    return fix_path("bk.pl"), fix_path("exs.pl"), fix_path("bias.pl")

def format_literal(literal):
    pred, args = literal
    args = ','.join(f'V{i}' for i in args)
    return f'{pred}({args})'

def format_rule(rule):
    head, body = rule
    head_str = ''
    if head:
        head_str = format_literal(head)
    body_str = ','.join(format_literal(literal) for literal in body)
    return f'{head_str}:- {body_str}.'

def calc_prog_size(prog):
    return sum(calc_rule_size(rule) for rule in prog)

def calc_rule_size(rule):
    head, body = rule
    return 1 + len(body)

def reduce_prog(prog):
    reduced = {}
    for rule in prog:
        head, body = rule
        k = head, frozenset(body)
        reduced[k] = rule
    return reduced.values()

def order_prog(prog):
    return sorted(list(prog), key=lambda rule: (rule_is_recursive(rule), len(rule[1])))

def rule_is_recursive(rule):
    head, body = rule
    head_pred, _head_args = head
    if not head:
        return False
    return any(head_pred == pred for pred, _args in body)

def prog_is_recursive(prog):
    if len(prog) < 2:
        return False
    return any(rule_is_recursive(rule) for rule in prog)

def prog_has_invention(prog):
    if len(prog) < 2:
        return False
    return any(rule_is_invented(rule) for rule in prog)

def rule_is_invented(rule):
    head, body = rule
    if not head:
        return False
    head_pred, _head_arg = head
    return head_pred.startswith('inv')

def mdl_score(fn, fp, size):
    return fn + fp + size

def flatten(xs):
    return [item for sublist in xs for item in sublist]

settings = None

def get_body_preds(solver):
    body_preds_ = set()
    for x in solver.symbolic_atoms.by_signature('body_pred', arity=2):
        pred = x.symbol.arguments[0].name
        arity = x.symbol.arguments[1].number
        body_preds_.add((pred, arity))
    return set(body_preds_)

class Settings:

    @classmethod
    def from_args(cls):
        args = parse_args() #
        bk, ex, bias = load_kbpath(args.kbpath)
        conf = vars(args)
        conf.update({'bk_file': bk, 'ex_file': ex, 'bias_file': bias})
        settings = Settings(**conf)
        return settings

    def __init__(self, cmd_line=False, info=True, timeout=TIMEOUT, max_body=MAX_BODY, max_vars=MAX_VARS, ex_file=None, bk_file=None, bias_file=None, noisy=False, nuwls=None, anytime_timeout=ANYTIME_TIMEOUT, kbpath=None, verbosity=1):

        self.nuwls = nuwls
        self.anytime_timeout = anytime_timeout
        self.bias_file = bias_file
        self.bk_file = bk_file
        self.bkcons_timeout = BKCONS_TIMEOUT
        self.ex_file = ex_file
        self.has_directions = False
        self.info = info
        self.max_body = max_body
        self.max_vars = max_vars
        self.noisy = noisy
        self.non_datalog_flag = False
        self.pi_enabled = False
        self.recursion_enabled = False
        self.timeout = timeout

        if noisy:
            self.batch_size = BATCH_SIZE
        else:
            self.batch_size = 1

        self.verbosity=verbosity
        self.debug=verbosity==3
        self.show_stats = self.verbosity>1

        logger.set_verbosity(verbosity)

        solver = clingo.Control(['-Wnone'])
        with open(self.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], """
            #defined body_literal/4.
            #defined clause/1.
            #defined clause_var/2.
            #defined var_type/3.
            #defined body_size/2.
            #defined recursive/0.
            #defined var_in_literal/4.
        """)
        solver.ground([('bias', [])])

        for x in solver.symbolic_atoms.by_signature('enable_recursion', arity=0):
            self.recursion_enabled = True

        for x in solver.symbolic_atoms.by_signature('enable_pi', arity=0):
            self.pi_enabled = True

        for x in solver.symbolic_atoms.by_signature('non_datalog', arity=0):
            self.non_datalog_flag = True

        self.directions = directions = defaultdict(dict)

        for x in solver.symbolic_atoms.by_signature('direction', arity=2):
            self.has_directions = True
            pred = x.symbol.arguments[0].name
            for i, y in enumerate(x.symbol.arguments[1].arguments):
                y = y.name
                if y == 'in':
                    arg_dir = '+'
                elif y == 'out':
                    arg_dir = '-'
                directions[pred][i] = arg_dir

        max_arity = 0
        for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
            max_arity = max(max_arity, x.symbol.arguments[1].number)
            head_pred = x.symbol.arguments[0].name
            head_arity = x.symbol.arguments[1].number
            head_args = tuple(range(head_arity))
            self.head_literal = Literal(head_pred, head_args)

        if self.max_body == MAX_BODY:
            for x in solver.symbolic_atoms.by_signature('max_body', arity=1):
                self.max_body = x.symbol.arguments[0].number

        if self.max_vars == MAX_VARS:
            for x in solver.symbolic_atoms.by_signature('max_vars', arity=1):
                self.max_vars = x.symbol.arguments[0].number

        if self.recursion_enabled or self.pi_enabled:
            self.max_rules = 2
            for x in solver.symbolic_atoms.by_signature('max_clauses', arity=1):
                self.max_rules = x.symbol.arguments[0].number
        else:
            self.max_rules = 1

        self.body_preds = get_body_preds(solver)
        max_arity = max(max_arity, max(arity for (pred, arity) in self.body_preds))

        # check that directions are all given
        if self.has_directions:
            for pred, arity in self.body_preds:
                if len(directions[pred]) != arity:
                    print(f'ERROR: missing directions for {pred}/{arity}')
                    exit()

        self.literal_inputs = {}
        self.literal_outputs = {}

        if self.has_directions:
            head_pred, head_args = self.head_literal
            # print('head_args', head_args)
            for head_args in permutations(range(self.max_vars), len(head_args)):
                head_inputs = frozenset(arg for i, arg in enumerate(head_args) if directions[head_pred][i] == '+')
                head_outputs = frozenset(arg for i, arg in enumerate(head_args) if directions[head_pred][i] == '-')
                self.literal_inputs[(head_pred, head_args)] = head_inputs
                self.literal_outputs[(head_pred, head_args)] = head_outputs

        self.cached_atom_args = {}
        for i in range(1, max_arity+1):
            for args in permutations(range(0, self.max_vars), i):
                k = tuple(clingo.Number(x) for x in args)
                self.cached_atom_args[k] = args

        self.cached_literals = {}
        for pred, arity in self.body_preds:
            for k, args in self.cached_atom_args.items():
                if len(args) != arity:
                    continue
                literal = Literal(pred, args)
                self.cached_literals[(pred, k)] = literal
                if self.has_directions:
                    self.literal_inputs[(pred, args)] = frozenset(arg for i, arg in enumerate(args) if directions[pred][i] == '+')
                    self.literal_outputs[(pred, args)] = frozenset(arg for i, arg in enumerate(args) if directions[pred][i] == '-')

        pred = self.head_literal.predicate
        arity = len(self.head_literal.arguments)

        for k, args in self.cached_atom_args.items():
            if len(args) != arity:
                continue
            literal = Literal(pred, args)
            self.cached_literals[(pred, k)] = literal

        self.head_types = None
        self.body_types = {}
        for x in solver.symbolic_atoms.by_signature('type', arity=2):
            pred = x.symbol.arguments[0].name
            xs = [y.name for y in x.symbol.arguments[1].arguments]
            if pred == head_pred:
                self.head_types = xs
            else:
                self.body_types[pred] = xs

        if len(self.body_types) > 0 or not self.head_types is None:
            if self.head_types is None:
                print('WARNING: MISSING HEAD TYPE')
                exit()
            for p,a in self.body_preds:
                if p not in self.body_types:
                    print(f'WARNING: MISSING BODY TYPE FOR {p}')
                    exit()

        self.single_solve = not (self.recursion_enabled or self.pi_enabled)

        logger.info(f'Max rules: {self.max_rules}')
        logger.info(f'Max vars: {self.max_vars}')
        logger.info(f'Max body: {self.max_body}')

def init_settings(in_settings=None):
    global settings
    assert(settings is None)
    if in_settings is None:
        settings = Settings.from_args()
    return settings

import os
# AC: I do not know what this code below really does, but it works
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

def rename_variables(rule):
    head, body = rule
    if head:
        head_vars = set(head.arguments)
    else:
        head_vars = set()
    next_var = len(head_vars)
    new_body = []
    lookup = {}
    for pred, args in sorted(body, key=lambda x: x.predicate):
        new_args = []
        for var in args:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_body.append((pred, tuple(new_args)))
    return (head, new_body)

def get_raw_prog(prog):
    xs = set()
    for rule in prog:
        h, b = rename_variables(rule)
        xs.add((h, frozenset(b)))
    return frozenset(xs)

def prog_hash(prog):
    new_prog = get_raw_prog(prog)
    return hash(new_prog)

def remap_variables(rule):
    head, body = rule

    head_vars = frozenset(head.arguments) if head else frozenset()

    next_var = len(head_vars)
    lookup = {i:i for i in head_vars}

    new_body = []
    for pred, args in body:
        new_args = []
        for var in args:
            if var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(pred, tuple(new_args))
        new_body.append(new_atom)

    return head, frozenset(new_body)

def format_prog(prog):
    return '\n'.join(format_rule(rule) for rule in prog)

# TODO: THIS CHECK IS NOT COMPLETE
# IT DOES NOT ACCOUNT FOR VARIABLE RENAMING
# R1 = (None, frozenset({('c3', ('A',)), ('c2', ('A',))}))
# R2 = (None, frozenset({('c3', ('B',)), ('c2', ('B',), true_value(A,B))}))
def rule_subsumes(r1, r2):
    # r1 subsumes r2 if r1 is a subset of r2
    h1, b1 = r1
    h2, b2 = r2
    if h1 != None and h2 == None:
        return False
    return b1.issubset(b2)

# P1 subsumes P2 if for every rule R2 in P2 there is a rule R1 in P1 such that R1 subsumes R2
def theory_subsumes(prog1, prog2):
    return all(any(rule_subsumes(r1, r2) for r1 in prog1) for r2 in prog2)

def head_connected(rule):
    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    while body_literals:
        connected = []
        for literal in body_literals:
            if any (x in head_connected_vars for x in literal.arguments):
                head_connected_vars.update(literal.arguments)
                connected.append(literal)
        if not connected and body_literals:
            return False
        body_literals.difference_update(connected)
    return True

def connected(body):
    if len(body) == 1:
        return True

    body = list(body)
    connected_vars = set(body[0].arguments)
    body_literals = set(body[1:])

    while body_literals:
        connected = []
        for literal in body_literals:
            if any (x in connected_vars for x in literal.arguments):
                connected_vars.update(literal.arguments)
                connected.append(literal)
        if not connected and body_literals:
            return False
        body_literals.difference_update(connected)
    return True

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def generalisations(prog, allow_headless=True, recursive=False):

    if len(prog) == 1:
        rule = list(prog)[0]
        head, body = rule

        if allow_headless:
            if head and len(body) > 0:
                new_rule = (None, body)
                new_prog = [new_rule]
                yield new_prog

        if (recursive and len(body) > 2 and head) or (not recursive and len(body) > 1):
            body = list(body)
            for i in range(len(body)):
                # do not remove recursive literals
                if recursive and body[i].predicate == head.predicate:
                    continue
                new_body = body[:i] + body[i+1:]
                new_rule = (head, frozenset(new_body))
                new_prog = [new_rule]
                yield new_prog

    else:
        prog = list(prog)
        for i in range(len(prog)):
            subrule = prog[i]
            recursive = rule_is_recursive(subrule)
            for new_subrule in generalisations([subrule], allow_headless=False, recursive=recursive):
                new_prog = prog[:i] + new_subrule + prog[i+1:]
                yield new_prog

def order_rule_datalog(head, body):
    ordered_body = []
    seen_vars = set()

    if head:
        seen_vars.update(head.arguments)
        recursive_literals = set(literal for literal in body if literal.predicate == head.predicate)
    else:
        recursive_literals = set()

    body_literals = set(body) - recursive_literals

    while body_literals:
        selected_literal = None
        for literal in body_literals:
            if set(literal.arguments).issubset(seen_vars):
                selected_literal = literal
                break

        if selected_literal == None:
            selected_literal = min(body_literals, key=lambda x: tmp_score_(seen_vars, x))

        ordered_body.append(selected_literal)
        seen_vars.update(selected_literal.arguments)
        body_literals.remove(selected_literal)

    return head, tuple(ordered_body) + tuple(recursive_literals)

def tmp_score_(seen_vars, literal):
    pred, args = literal
    # GLOBAL VARIABLE SHITSHOW
    return recalls[pred, tuple(1 if x in seen_vars else 0 for x in args)]

def order_rule(rule):
    head, body = rule

    if settings.pi_enabled:
        return rule

    if settings.datalog:
        return order_rule_datalog(head, frozenset(body))

    if not settings.has_directions:
        return rule

    ordered_body = []
    grounded_variables = set()

    if head:
        head_pred, head_args = head
        head_inputs = settings.literal_inputs[(head_pred, head_args)]
        if head_inputs == []:
            return rule
        grounded_variables.update(head_inputs)

    body_literals = set(body)

    while body_literals:
        selected_literal = None
        for literal in body_literals:
            pred, args = literal
            literal_outputs = settings.literal_outputs[(pred, args)]

            if len(literal_outputs) == len(args):
                selected_literal = literal
                break

            literal_inputs = settings.literal_inputs[(pred, args)]
            if not literal_inputs.issubset(grounded_variables):
                continue

            if head and pred != head.predicate:
                # find the first ground non-recursive body literal and stop
                selected_literal = literal
                break
            elif selected_literal == None:
                # otherwise use the recursive body literal
                selected_literal = literal

        if selected_literal == None:
            message = f'{selected_literal} in clause {format_rule(rule)} could not be grounded'
            raise ValueError(message)

        ordered_body.append(selected_literal)
        pred, args = selected_literal
        selected_literal_outputs = settings.literal_outputs[(pred, args)]
        grounded_variables = grounded_variables.union(selected_literal_outputs)
        body_literals = body_literals.difference({selected_literal})

    return head, tuple(ordered_body)

def print_incomplete_solution2(prog, size, conf_matrix):
    tp, fn, tn, fp = conf_matrix
    logger.out('*'*20)
    logger.out('New best hypothesis:')
    if settings.noisy:
        logger.out(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size} mdl:{size+fn+fp}')
    else:
        logger.out(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size}')
    for rule in order_prog(prog):
        logger.out(format_rule(order_rule(rule)))
    logger.out('*'*20)

def print_prog_score(prog, score):
    tp, fn, tn, fp = score
    size = calc_prog_size(prog)
    precision = 'n/a'
    if (tp+fp) > 0:
        precision = f'{tp / (tp+fp):0.2f}'
    recall = 'n/a'
    if (tp+fn) > 0:
        recall = f'{tp / (tp+fn):0.2f}'
    print('*'*10 + ' SOLUTION ' + '*'*10)
    if settings.noisy:
        print(f'Precision:{precision} Recall:{recall} TP:{tp} FN:{fn} TN:{tn} FP:{fp} Size:{size} MDL:{size+fn+fp}')
    else:
      print(f'Precision:{precision} Recall:{recall} TP:{tp} FN:{fn} TN:{tn} FP:{fp} Size:{size}')
    for rule in order_prog(prog):
        print(format_rule(order_rule(rule)))
    print('*'*30)

def has_valid_directions(rule):
    if settings.has_directions:
        return has_valid_directions_(rule)
    return True

@cache
def has_valid_directions_(rule):
    head, body = rule
    lit_inputs = settings.literal_inputs
    lit_outputs = settings.literal_outputs

    body_literals = set(body)

    # ---- INITIAL GROUNDED VARS ----
    if head:
        head_pred, head_args = head
        grounded = lit_inputs[(head_pred, head_args)]
        if not grounded:
            return True
    else:
        if all(not lit_inputs[(p, a)] for p, a in body):
            return True
        grounded = set()

    # ---- MAIN LOOP ----
    while body_literals:
        selected = None

        for literal in body_literals:
            pred, args = literal
            inputs = lit_inputs[(pred, args)]
            outputs = lit_outputs[(pred, args)]

            if head:
                if not inputs.issubset(grounded):
                    continue
                if pred != head_pred:
                    selected = literal
                    break
                if selected is None:
                    selected = literal
            else:
                if outputs == set(args):
                    selected = literal
                    break
                if inputs.issubset(grounded):
                    selected = literal
                    break

        if selected is None:
            return False

        pred, args = selected
        if head:
            grounded |= lit_outputs[(pred, args)]
        else:
            grounded |= set(args)

        body_literals.remove(selected)

    return True