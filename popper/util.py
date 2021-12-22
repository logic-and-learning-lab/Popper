import signal
import argparse
import os
import logging
import copy
from time import perf_counter
from contextlib import contextmanager
from .core import Clause
from .constrain import Constrain

TIMEOUT=600
EVAL_TIMEOUT=0.001
MAX_LITERALS=100
MAX_SOLUTIONS=1
CLINGO_ARGS=''

def parse_args():
    parser = argparse.ArgumentParser(description='Popper, an ILP engine based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')
    parser.add_argument('--eval-timeout', type=float, default=EVAL_TIMEOUT, help='Prolog evaluation timeout in seconds')
    parser.add_argument('--timeout', type=float, default=TIMEOUT, help='Overall timeout (in seconds)')
    parser.add_argument('--max-literals', type=int, default=MAX_LITERALS, help='Maximum number of literals allowed in program')
    # parser.add_argument('--max-solutions', type=int, default=MAX_SOLUTIONS, help='Maximum number of solutions to print')
    parser.add_argument('--test-all', default=False, action='store_true', help='Test all examples')
    parser.add_argument('--info', default=False, action='store_true', help='Print best programs so far to stderr')
    parser.add_argument('--debug', default=False, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--stats', default=False, action='store_true', help='Print statistics at end of execution')
    parser.add_argument('--hspace', type=int, default=-1, help='Show the full hypothesis space')
    parser.add_argument('--functional-test', default=False, action='store_true', help='Run custom functional test')
    parser.add_argument('--clingo-args', type=str, default=CLINGO_ARGS, help='Arguments to pass to Clingo')
    parser.add_argument('--ex-file', type=str, default='', help='Filename for the examples')
    parser.add_argument('--bk-file', type=str, default='', help='Filename for the background knowledge')
    parser.add_argument('--bias-file', type=str, default='', help='Filename for the bias')
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

def parse_settings():
    args = parse_args()
    
    (bk_file, ex_file, bias_file) = load_kbpath(args.kbpath)

    return Settings(
        bias_file,
        args.ex_file if args.ex_file else ex_file,
        args.bk_file if args.bk_file else bk_file,
        info = args.info,
        debug = args.debug,
        stats = args.stats,
        eval_timeout = args.eval_timeout,
        test_all = args.test_all,
        timeout = args.timeout,
        max_literals = args.max_literals,
        clingo_args= [] if not args.clingo_args else args.clingo_args.split(' '),
        max_solutions = MAX_SOLUTIONS,
        functional_test = args.functional_test,
        hspace = False if args.hspace == -1 else args.hspace
    )

class Settings:
    def __init__(self,
            bias_file,
            ex_file,
            bk_file,
            info = False,
            debug = False,
            stats = False,
            eval_timeout = EVAL_TIMEOUT,
            test_all = False,
            timeout = TIMEOUT,
            max_literals = MAX_LITERALS,
            clingo_args = CLINGO_ARGS,
            max_solutions = MAX_SOLUTIONS,
            functional_test = False,
            hspace=False):
            
        self.bias_file = bias_file
        self.ex_file = ex_file
        self.bk_file = bk_file
        self.info = info
        self.debug = debug
        self.stats = stats
        self.eval_timeout = eval_timeout
        self.test_all = test_all
        self.timeout = timeout
        self.max_literals = max_literals
        self.clingo_args = clingo_args
        self.max_solutions = max_solutions
        self.functional_test = functional_test
        self.hspace = hspace

def format_program(program):
    return "\n".join(Clause.to_code(Clause.to_ordered(clause)) + '.' for clause in program)

def format_conf_matrix(conf_matrix):
    tp, fn, tn, fp = conf_matrix
    precision = 'n/a'
    if (tp+fp) > 0:
        precision = f'{tp / (tp+fp):0.2f}'
    recall = 'n/a'
    if (tp+fn) > 0:
        recall = f'{tp / (tp+fn):0.2f}'
    return f'% Precision:{precision}, Recall:{recall}, TP:{tp}, FN:{fn}, TN:{tn}, FP:{fp}\n'

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
                    solution = None):
        self.exec_start = perf_counter()
        self.logger = logging.getLogger("popper")

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

    def update_num_literals(self, size):
        prev_stage = self.stages[-1] if self.stages else None
        programs_tried = self.total_programs - prev_stage.total_programs if prev_stage else 0

        total_exec_time = self.total_exec_time()
        exec_time = total_exec_time - prev_stage.total_exec_time if prev_stage else 0
        
        self.stages.append(Stage(size, self.total_programs, programs_tried, total_exec_time, exec_time))

        self.logger.debug(f'Programs tried: {programs_tried} Exec Time: {exec_time:0.3f}s Total Exec Time: {total_exec_time:0.3f}s\n')

        self.logger.debug(f'{"*" * 20} MAX LITERALS: {size} {"*" * 20}')

        self.num_literals = size
    
    def register_program(self, program, conf_matrix):
        self.total_programs +=1
        
        self.logger.debug(f'Program {self.total_programs}:')
        self.logger.debug(format_program(program))
        self.logger.debug(format_conf_matrix(conf_matrix))
    
    def register_best_program(self, program, conf_matrix):
        prog_stats = self.make_program_stats(program, conf_matrix)
        self.best_programs.append(prog_stats)
        if self.log_best_programs:
            self.logger.info(f'% NEW BEST PROG {self.total_programs}:')
            self.logger.info(prog_stats.code)
            self.logger.info(format_conf_matrix(conf_matrix))

    def log_final_result(self):
        if self.solution:
            prog_stats = self.solution
        elif self.best_programs:
            prog_stats = self.best_programs[-1]
        else:
            self.logger.info('NO PROGRAMS FOUND')
            return

        self.logger.info(f'\n% BEST PROG {self.total_programs}:')
        self.logger.info(prog_stats.code)
        self.logger.info(format_conf_matrix(prog_stats.conf_matrix))

    def make_program_stats(self, program, conf_matrix):
        code = format_program(program)
        return ProgramStats(code, conf_matrix, self.total_exec_time(), self.duration_summary())

    def register_solution(self, program, conf_matrix):
        prog_stats = self.make_program_stats(program, conf_matrix)
        self.solution = prog_stats

    def register_completion(self):
        self.logger.info('NO MORE SOLUTIONS')
        self.final_exec_time = self.total_exec_time()

    def register_rules(self, rules):
        self.logger.debug('Rules:')
        for rule in rules:
            self.logger.debug(Constrain.format_constraint(rule))
        self.logger.debug('\n')

        self.total_rules += len(rules)
    
    def register_ground_rules(self, rules):
        self.total_ground_rules += len(rules)

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
        self.logger.info(message)

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