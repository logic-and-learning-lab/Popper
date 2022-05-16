import sys
import clingo
import clingo.script
import multiprocessing
import time
import signal
import argparse
import os
import logging
import copy
from time import perf_counter
from contextlib import contextmanager
from .core import Literal
# from .constrain import Constrain

clingo.script.enable_python()

TIMEOUT=1200
EVAL_TIMEOUT=0.001
MAX_LITERALS=100
MAX_SOLUTIONS=1
CLINGO_ARGS=''
MAX_RULES=2
MAX_VARS=6
MAX_BODY=6


def parse_args():
    parser = argparse.ArgumentParser(description='Popper, an ILP engine based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')
    parser.add_argument('--info', default=False, action='store_true', help='Print best programs so far to stderr')
    parser.add_argument('--debug', default=False, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--stats', default=False, action='store_true', help='Print statistics at end of execution')

    parser.add_argument('--timeout', type=float, default=TIMEOUT, help=f'Overall timeout in seconds (default: {timeout})')
    parser.add_argument('--eval-timeout', type=float, default=EVAL_TIMEOUT, help=f'Prolog evaluation timeout in seconds (default: {EVAL_TIMEOUT})')
    parser.add_argument('--max-literals', type=int, default=MAX_LITERALS, help=f'Maximum number of literals allowed in program (default: {MAX_LITERALS})')
    parser.add_argument('--max-body', type=int, default=MAX_BODY, help=f'Maximum number of body literals allowed in rule (default: {MAX_BODY})')
    parser.add_argument('--max-vars', type=int, default=MAX_VARS, help=f'Maximum number of variables allowed in rule (default: {MAX_VARS})')
    parser.add_argument('--max-rules', type=int, default=MAX_RULES, help=f'Maximum number of rules allowed in recursive program (default: {MAX_RULES})')
    # parser.add_argument('--test-all', default=False, action='store_true', help='Test all examples')
    # parser.add_argument('--cd', default=False, action='store_true', help='context-dependent')
    parser.add_argument('--hspace', type=int, default=-1, help='Show the full hypothesis space')
    parser.add_argument('--functional-test', default=False, action='store_true', help='Run custom functional test')
    parser.add_argument('--clingo-args', type=str, default=CLINGO_ARGS, help='Arguments to pass to Clingo')
    parser.add_argument('--ex-file', type=str, default='', help='Filename for the examples')
    parser.add_argument('--bk-file', type=str, default='', help='Filename for the background knowledge')
    parser.add_argument('--bias-file', type=str, default='', help='Filename for the bias')
    parser.add_argument('--bkcons', default=False, action='store_true', help='do bk cons')
    return parser.parse_args()

def timeout(func, args=(), kwargs={}, timeout_duration=1, default=None):
    class TimeoutError(Exception):
        pass

    def handler(signum, frame):
        raise TimeoutError()

    # set the timeout handler
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout_duration)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as exc:
        result = default
    finally:
        signal.alarm(0)

    return result

def load_kbpath(kbpath):
    return fix_path(kbpath, "bk.pl"), fix_path(kbpath, "exs.pl"), fix_path(kbpath, "bias.pl")
    
def fix_path(kbpath, filename):
    full_filename = os.path.join(kbpath, filename)
    return full_filename.replace('\\', '\\\\') if os.name == 'nt' else full_filename

class Stats:
    def __init__(self,
                    log_best_programs=False,
                    num_literals = 0,
                    total_programs = 0,
                    total_rules = 0,
                    total_ground_rules = 0,
                    durations = None,
                    final_exec_time = 0,
                    stages = None,
                    best_programs = None,
                    solution = None,
                    info = False,
                    debug = False
                    ):

        self.exec_start = perf_counter()
        self.logger = logging.getLogger("popper")

        if debug:
            log_level = logging.DEBUG
            logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')
            # logging.basicConfig(level=log_level, stream=sys.stderr, format='%(message)s')
        elif info:
            log_level = logging.INFO
            logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')
            # logging.basicConfig(level=log_level, stream=sys.stderr, format='%(message)s')






        self.log_best_programs = log_best_programs
        self.num_literals = num_literals
        self.total_programs = total_programs
        self.total_rules = total_rules
        self.total_ground_rules = total_ground_rules
        self.durations = {} if not durations else durations
        self.final_exec_time = final_exec_time
        self.stages = [] if not stages else stages
        self.best_programs = [] if not best_programs else best_programs
        self.solution = solution

    def __enter__(self):
        return self

    # def update_num_literals(self, size):
    #     prev_stage = self.stages[-1] if self.stages else None
    #     programs_tried = self.total_programs - prev_stage.total_programs if prev_stage else 0

    #     total_exec_time = self.total_exec_time()
    #     exec_time = total_exec_time - prev_stage.total_exec_time if prev_stage else 0
        
    #     self.stages.append(Stage(size, self.total_programs, programs_tried, total_exec_time, exec_time))

    #     self.logger.debug(f'Programs tried: {programs_tried} Exec Time: {exec_time:0.3f}s Total Exec Time: {total_exec_time:0.3f}s\n')

    #     self.logger.debug(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')

    #     self.num_literals = size
    
    def register_prog(self, prog):
        self.logger.debug(f'Program {self.total_programs}:')
        for rule in prog:
            self.logger.debug(format_rule(rule))

    def register_candidate_prog(self, prog):
        self.logger.info(f'Candidate program:')
        for rule in prog:
            self.logger.info(format_rule(rule))

    def register_best_prog(self, prog, size):
        self.logger.info(f'New best solution of size {size}:')
        for rule in prog:
            self.logger.info(format_rule(rule))

    # def log_final_result(self):
    #     if self.solution:
    #         prog_stats = self.solution
    #     elif self.best_programs:
    #         prog_stats = self.best_programs[-1]
    #     else:
    #         self.logger.info('NO PROGRAMS FOUND')
    #         return

    #     self.logger.info(f'\n% BEST PROG {self.total_programs}:')
    #     self.logger.info(prog_stats.code)
    #     self.logger.info(format_conf_matrix(prog_stats.conf_matrix))

    def make_program_stats(self, program, conf_matrix):
        code = format_program(program)
        return ProgramStats(code, conf_matrix, self.total_exec_time(), self.duration_summary())

    def register_solution(self, program, conf_matrix):
        prog_stats = self.make_program_stats(program, conf_matrix)
        self.solution = prog_stats

    def register_completion(self):
        self.logger.info('NO MORE SOLUTIONS')
        self.final_exec_time = self.total_exec_time()

    def register_program(self, prog):
        self.logger.debug(f'Program {self.total_programs}:')
        self.logger.debug(format_prog(prog))

    @property
    def best_program(self):
        if self.solution:
            return self.solution
        if self.best_programs:
            return self.best_programs[-1]
        return None

    def total_exec_time(self):
        return perf_counter() - self.exec_start

    def show(self):
        message = f'Total programs: {self.total_programs}\n'
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
        for operation, durations in self.durations.items():
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

# def format_program(program):
    # return "\n".join(Clause.to_code(Clause.to_ordered(clause)) + '.' for clause in program)

def format_prog(prog):
    return '\n'.join(format_rule(rule) for rule in prog)

def format_rule(rule):
    head, body = rule
    head_str = ''
    if head:
        head_str = Literal.to_code(head)
    body_str = ','.join(Literal.to_code(literal) for literal in body)
    return f'{head_str}:- {body_str}.'

def print_prog(prog):
    print('*'*10 + ' SOLUTION ' + '*'*10)
    print(format_prog(prog))
    print('*'*30)

def prog_size(prog):
    return sum(1 + len(body) for head, body in prog)

def order_rule(rule):
    head, body = rule
    ordered_body = []
    grounded_variables = head.inputs
    body_literals = set(body)

    if head.inputs == []:
        return clause

    # print('grounded_variables',grounded_variables)
    while body_literals:
        selected_literal = None
        for literal in body_literals:
            # print('literal.inputs',literal.inputs)
            # AC: could cache for a micro-optimisation
            if literal.inputs.issubset(grounded_variables):
                if literal.predicate != head.predicate:
                    # find the first ground non-recursive body literal and stop
                    selected_literal = literal
                    break
                else:
                    # otherwise use the recursive body literal
                    selected_literal = literal

        if selected_literal == None:
            message = f'{selected_literal} in clause {format_rule(rule)} could not be grounded'
            raise ValueError(message)

        ordered_body.append(selected_literal)
        grounded_variables = grounded_variables.union(selected_literal.outputs)
        body_literals = body_literals.difference({selected_literal})

    return (head, tuple(ordered_body))


class Stage:
    def __init__(self, num_literals, total_programs, programs, total_exec_time, exec_time):
        self.num_literals = num_literals
        self.total_programs = total_programs
        self.programs = programs
        self.total_exec_time = total_exec_time
        self.exec_time = exec_time

class ProgramStats:
    def __init__(self, code, conf_matrix, total_exec_time, durations):
        self.code = code
        self.conf_matrix = conf_matrix
        self.total_exec_time = total_exec_time
        self.durations = durations

        _, fn, _, fp = conf_matrix
        
        self.is_solution = fn == fp == 0

class DurationSummary:
    def __init__(self, operation, called, total, mean, maximum):
        self.operation = operation
        self.called = called
        self.total = total
        self.mean = mean
        self.maximum = maximum

def chunk_list(xs, size):
    for i in range(0, len(xs), size):
        yield xs[i:i+size]

def flatten(xs):
    return [item for sublist in xs for item in sublist]

arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}

class Settings:
    def __init__(self):
        args = parse_args()

        self.stats = Stats(info=args.info, debug=args.debug)
        self.bk_file, self.ex_file, self.bias_file = load_kbpath(args.kbpath)
        self.show_stats = args.stats

        self.max_literals = args.max_literals
        self.clingo_args = [] if not args.clingo_args else args.clingo_args.split(' ')

        self.functional_test = args.functional_test
        self.hspace = args.hspace
        self.timeout = args.timeout
        self.eval_timeout = args.eval_timeout

        solver = clingo.Control()
        with open(self.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], """
            #defined body_literal/4.
            #defined clause/1.
            #defined clause_var/2.
            #defined var_type/3.
        """)
        solver.ground([('bias', [])])

        self.max_body = args.max_body
        for x in solver.symbolic_atoms.by_signature('max_body', arity=1):
            self.max_body = x.symbol.arguments[0].number

        self.max_vars = args.max_vars
        for x in solver.symbolic_atoms.by_signature('max_vars', arity=1):
            self.max_vars = x.symbol.arguments[0].number

        self.max_rules = None
        for x in solver.symbolic_atoms.by_signature('max_clauses', arity=1):
            self.max_rules = x.symbol.arguments[0].number

        if self.max_rules == None:
            pi_or_recursion = False
            for x in solver.symbolic_atoms.by_signature('enable_recursion', arity=0):
                pi_or_recursion = True
            for x in solver.symbolic_atoms.by_signature('enable_pi', arity=0):
                pi_or_recursion = True
            if pi_or_recursion:
                self.max_rules = args.max_rules
            else:
                self.max_rules = 1

        self.stats.logger.debug(f'Max rules: {self.max_rules}')
        self.stats.logger.debug(f'Max vars: {self.max_vars}')
        self.stats.logger.debug(f'Max body: {self.max_body}')