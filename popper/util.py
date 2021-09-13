import signal
import argparse
from time import perf_counter
from contextlib import contextmanager

TIMEOUT=600
EVAL_TIMEOUT=0.001
MAX_LITERALS=100
MAX_SOLUTIONS=1
TEST_ALL=False
DEBUG=False
INFO=False
STATS=False
FUNCTIONAL_TEST=False
CLINGO_ARGS=''

def parse_args():
    parser = argparse.ArgumentParser(description='Popper, an ILP engine based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')
    parser.add_argument('--eval-timeout', type=float, default=EVAL_TIMEOUT, help='Prolog evaluation timeout in seconds')
    parser.add_argument('--timeout', type=float, default=TIMEOUT, help='Overall timeout (in seconds)')
    parser.add_argument('--max-literals', type=int, default=MAX_LITERALS, help='Maximum number of literals allowed in program')
    # parser.add_argument('--max-solutions', type=int, default=MAX_SOLUTIONS, help='Maximum number of solutions to print')
    parser.add_argument('--test-all', default=TEST_ALL, action='store_true', help='Test all examples')
    parser.add_argument('--debug', default=DEBUG, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--info', default=INFO, action='store_true', help='Print useful info information to stderr')
    parser.add_argument('--stats', default= STATS, action='store_true', help='Print statistics at end of execution')
    parser.add_argument('--functional-test', default=FUNCTIONAL_TEST, action='store_true', help='Run custom functional test')
    parser.add_argument('--clingo-args', type=str, default=CLINGO_ARGS, help='Arguments to pass to Clingo')
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

class Stats:
    def __init__(self):
        self.total_programs = 0
        self.durations = {}
        self.exec_start = perf_counter()

    def __enter__(self):
        return self

    def show(self):
        total_exec_time = perf_counter() - self.exec_start
        message = f'Total programs: {self.total_programs}\n'
        total_op_time = 0
        for operation, durations in self.durations.items():
            called = len(durations)
            total = sum(durations)
            mean = sum(durations)/len(durations)
            max_ = max(durations)
            message += f'{operation.title()}:\n\tCalled: {called} times \t Total: {total:0.2f} \t Mean: {mean:0.3f} \t Max: {max_:0.3f}\n'
            if operation != 'basic setup':
                total_op_time += total
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {total_exec_time:0.2f}s'
        print(message)

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

class Settings:
    def __init__(self, cmd_line=True):
        if cmd_line:
            args = parse_args()
            self.debug = args.debug
            self.info = args.info
            self.stats = args.stats
            self.kbpath = args.kbpath
            if self.kbpath[-1] != '/':
                self.kbpath += '/'
            self.bias_file = self.kbpath + 'bias.pl'
            self.ex_file = self.kbpath + 'exs.pl'
            self.bk_file = self.kbpath + 'bk.pl'
            self.eval_timeout = args.eval_timeout
            self.test_all = args.test_all
            self.timeout = args.timeout
            self.max_literals = args.max_literals
            self.clingo_args = args.clingo_args
            # self.max_solutions = args.max_solutions
            self.functional_test = args.functional_test
            self.clingo_args = [] if not args.clingo_args else args.clingo_args.split(' ')