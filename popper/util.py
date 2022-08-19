import clingo
import clingo.script
import signal
import argparse
import os
import logging
from time import perf_counter
from contextlib import contextmanager
from .core import Literal

clingo.script.enable_python()

TIMEOUT=600
EVAL_TIMEOUT=0.001
MAX_LITERALS=40
MAX_SOLUTIONS=1
CLINGO_ARGS=''
MAX_RULES=2
MAX_VARS=6
MAX_BODY=6
MAX_EXAMPLES=10000

def parse_args():
    parser = argparse.ArgumentParser(description='Popper is an ILP system based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')
    # parser.add_argument('--info', default=False, action='store_true', help='Print best programs so ')
    parser.add_argument('--quiet', '-q', default=False, action='store_true', help='Hide information during learning')
    parser.add_argument('--debug', default=False, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--stats', default=False, action='store_true', help='Print statistics at end of execution')

    parser.add_argument('--timeout', type=float, default=TIMEOUT, help=f'Overall timeout in seconds (default: {TIMEOUT})')
    parser.add_argument('--eval-timeout', type=float, default=EVAL_TIMEOUT, help=f'Prolog evaluation timeout in seconds (default: {EVAL_TIMEOUT})')
    parser.add_argument('--max-literals', type=int, default=MAX_LITERALS, help=f'Maximum number of literals allowed in program (default: {MAX_LITERALS})')
    parser.add_argument('--max-body', type=int, default=MAX_BODY, help=f'Maximum number of body literals allowed in rule (default: {MAX_BODY})')
    parser.add_argument('--max-vars', type=int, default=MAX_VARS, help=f'Maximum number of variables allowed in rule (default: {MAX_VARS})')
    parser.add_argument('--max-rules', type=int, default=MAX_RULES, help=f'Maximum number of rules allowed in recursive program (default: {MAX_RULES})')
    parser.add_argument('--max-examples', type=int, default=MAX_EXAMPLES, help=f'Maximum number of examples per label (positive or negative) to learn from (default: {MAX_EXAMPLES})')

    # parser.add_argument('--threads', type=int, default=MAX_LITERALS, help=f'Maximum number of threads (default: 1)')

    # parser.add_argument('--cd', default=False, action='store_true', help='context-dependent')
    # parser.add_argument('--hspace', type=int, default=-1, help='Show the full hypothesis space')
    parser.add_argument('--functional-test', default=False, action='store_true', help='Run functional test')
    # parser.add_argument('--clingo-args', type=str, default=CLINGO_ARGS, help='Arguments to pass to Clingo')
    parser.add_argument('--ex-file', type=str, default='', help='Filename for the examples')
    parser.add_argument('--bk-file', type=str, default='', help='Filename for the background knowledge')
    parser.add_argument('--bias-file', type=str, default='', help='Filename for the bias')
    parser.add_argument('--bkcons', default=False, action='store_true', help='EXPERIMENTAL FEATURE: deduce background constraints from Datalog background')
    return parser.parse_args()

def timeout(settings, func, args=(), kwargs={}, timeout_duration=1):
    result = None
    class TimeoutError(Exception):
        pass

    def handler(signum, frame):
        raise TimeoutError()

    # set the timeout handler
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout_duration)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as _exc:
        settings.logger.warn(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
        return result
    except AttributeError as moo:
        if '_SolveEventHandler' in str(moo):
            settings.logger.warn(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
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

class Stats:
    def __init__(self, info = False, debug = False):
        self.exec_start = perf_counter()
        self.total_programs = 0
        self.durations = {}

    def total_exec_time(self):
        return perf_counter() - self.exec_start

    def show(self):
        message = f'Num. programs: {self.total_programs}\n'
        total_op_time = 0
        for summary in self.duration_summary():
            message += f'{summary.operation}:\n\tCalled: {summary.called} times \t ' + \
                       f'Total: {summary.total:0.2f} \t Mean: {summary.mean:0.3f} \t ' + \
                       f'Max: {summary.maximum:0.3f}\n'
            if summary.operation != 'basic setup':
                total_op_time += summary.total
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {self.total_exec_time():0.2f}s'
        print(message)

    def duration_summary(self):
        summary = []
        stats = sorted(self.durations.items(), key = lambda x: sum(x[1]), reverse=True)
        for operation, durations in stats:
            called = len(durations)
            total = sum(durations)
            mean = sum(durations)/len(durations)
            maximum = max(durations)
            summary.append(DurationSummary(operation.title(), called, total, mean, maximum))
        return summary

    @contextmanager
    def duration(self, operation):
        start = perf_counter()
        try:
            yield
        finally:
            end = perf_counter()
            duration = end - start

            if operation not in self.durations:
                self.durations[operation] = [duration]
            else:
                self.durations[operation].append(duration)

def format_prog(prog):
    return '\n'.join(format_rule(order_rule(rule)) for rule in prog)

def format_literal(literal):
    args = ','.join(literal.arguments)
    return f'{literal.predicate}({args})'

def format_rule(rule):
    head, body = rule
    head_str = ''
    if head:
        head_str = format_literal(head)
    body_str = ','.join(format_literal(literal) for literal in body)
    return f'{head_str}:- {body_str}.'

def print_prog_score(prog, score):
    tp, fn, tn, fp, size = score
    precision = 'n/a'
    if (tp+fp) > 0:
        precision = f'{tp / (tp+fp):0.2f}'
    recall = 'n/a'
    if (tp+fn) > 0:
        recall = f'{tp / (tp+fn):0.2f}'
    print('*'*10 + ' SOLUTION ' + '*'*10)
    print(f'Precision:{precision} Recall:{recall} TP:{tp} FN:{fn} TN:{tn} FP:{fp} Size:{size}')
    print(format_prog(order_prog(prog)))
    print('*'*30)

def prog_size(prog):
    return sum(rule_size(rule) for rule in prog)

def rule_size(rule):
    head, body = rule
    return 1 + len(body)

def reduce_prog(prog):
    def f(literal):
        return literal.predicate, literal.arguments
    reduced = {}
    for rule in prog:
        head, body = rule
        head = f(head)
        body = frozenset(f(literal) for literal in body)
        k = head, body
        reduced[k] = rule
    return reduced.values()

def order_prog(prog):
    return sorted(list(prog), key=lambda rule: (rule_is_recursive(rule), len(rule[1])))

def rule_is_recursive(rule):
    head, body = rule
    if not head:
        return False
    return any(head.predicate  == literal.predicate for literal in body if isinstance(literal, Literal))

def prog_is_recursive(prog):
    return any(rule_is_recursive(rule) for rule in prog)

def rule_is_invented(rule):
    head, body = rule
    if not head:
        return False
    return head.predicate.startswith('inv')

def order_rule(rule):
    head, body = rule
    ordered_body = []
    grounded_variables = head.inputs
    body_literals = set(body)

    if head.inputs == []:
        return clause

    while body_literals:
        selected_literal = None
        for literal in body_literals:
            # AC: could cache for a micro-optimisation
            if literal.inputs.issubset(grounded_variables):
                if literal.predicate != head.predicate:
                    # find the first ground non-recursive body literal and stop
                    selected_literal = literal
                else:
                    # otherwise use the recursive body literal
                    selected_literal = literal

        if selected_literal == None:
            message = f'{selected_literal} in clause {format_rule(rule)} could not be grounded'
            raise ValueError(message)

        ordered_body.append(selected_literal)
        grounded_variables = grounded_variables.union(selected_literal.outputs)
        body_literals = body_literals.difference({selected_literal})

    return head, tuple(ordered_body)

class DurationSummary:
    def __init__(self, operation, called, total, mean, maximum):
        self.operation = operation
        self.called = called
        self.total = total
        self.mean = mean
        self.maximum = maximum

def flatten(xs):
    return [item for sublist in xs for item in sublist]

class Settings:
    def __init__(self, kbpath=False, info=True, debug=False, show_stats=False, bkcons=False, max_literals=MAX_LITERALS, timeout=TIMEOUT, quiet=False, eval_timeout=EVAL_TIMEOUT, max_examples=MAX_EXAMPLES, max_body=MAX_BODY, max_rules=MAX_RULES, max_vars=MAX_VARS, functional_test=False):

        if kbpath == False:
            args = parse_args()
            # info = args.info
            quiet = args.quiet
            debug = args.debug
            kbpath = args.kbpath
            show_stats = args.stats
            bkcons = args.bkcons
            max_literals = args.max_literals
            timeout = args.timeout
            eval_timeout = args.eval_timeout
            max_examples = args.max_examples
            max_body = args.max_body
            max_vars = args.max_vars
            max_rules = args.max_rules
            functional_test = args.functional_test

        self.logger = logging.getLogger("popper")

        if quiet:
            pass
        elif debug:
            log_level = logging.DEBUG
            logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')
        elif info:
            log_level = logging.INFO
            logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')

        self.info = info
        self.debug = debug
        self.stats = Stats(info=info, debug=debug)
        self.stats.logger = self.logger
        self.bk_file, self.ex_file, self.bias_file = load_kbpath(kbpath)
        self.show_stats = show_stats
        self.bkcons = bkcons
        self.max_literals = max_literals
        # self.clingo_args = [] if not args.clingo_args else args.clingo_args.split(' ')
        self.functional_test = functional_test
        self.timeout = timeout
        self.eval_timeout = eval_timeout
        self.max_examples = max_examples
        self.max_body = max_body
        self.max_vars = max_vars
        self.max_rules = max_rules

        self.solution = None
        self.best_prog_score = None
        # self.best_prog = None

        solver = clingo.Control()
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

        for x in solver.symbolic_atoms.by_signature('max_body', arity=1):
            self.max_body = x.symbol.arguments[0].number

        for x in solver.symbolic_atoms.by_signature('max_vars', arity=1):
            self.max_vars = x.symbol.arguments[0].number

        self.max_rules = None
        for x in solver.symbolic_atoms.by_signature('max_clauses', arity=1):
            self.max_rules = x.symbol.arguments[0].number

        self.recursion_enabled = False
        for x in solver.symbolic_atoms.by_signature('enable_recursion', arity=0):
            self.recursion_enabled = True

        self.pi_enabled = False
        for x in solver.symbolic_atoms.by_signature('enable_pi', arity=0):
            self.pi_enabled = True

        if self.max_rules == None:
            if self.recursion_enabled or self.pi_enabled:
                self.max_rules = max_rules
            else:
                self.max_rules = 1

        self.logger.debug(f'Max rules: {self.max_rules}')
        self.logger.debug(f'Max vars: {self.max_vars}')
        self.logger.debug(f'Max body: {self.max_body}')

    def print_incomplete_solution(self, prog, tp, fn, size):
        # self.logger.info(self.hypothesis_output(prog, tp, fn, size))
        self.logger.info('*'*20)
        self.logger.info('New best hypothesis:')
        self.logger.info(f'tp:{tp} fn:{fn} size:{size}')
        for rule in order_prog(prog):
            self.logger.info(format_rule(order_rule(rule)))
        self.logger.info('*'*20)
