import argparse
from time import perf_counter
from contextlib import contextmanager

def parse_args():
    parser = argparse.ArgumentParser(description='Popper, an ILP engine based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')
    parser.add_argument('--eval-timeout', type=float, default=0.1, help='Prolog evaluation timeout in seconds')
    parser.add_argument('--timeout', type=float, default=600, help='Overall timeout (in seconds)')
    parser.add_argument('--max-literals', type=int, default=100, help='Maximum number of literals allowed in program')
    parser.add_argument('--max-solutions', type=int, default=1, help='Maximum number of solutions to print')
    parser.add_argument('--test-all', default=False, action='store_true', help='Test all examples')
    parser.add_argument('--debug', default=False, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--stats', default= False, action='store_true', help='Print statistics at end of execution')
    parser.add_argument('--functional-test', default= False, action='store_true', help='Run custom functional test')
    parser.add_argument('--clingo-args', type=str, default='', help='Arguments to pass to Clingo')
    return parser.parse_args()

class Experiment:
    def __init__(self):
        self.args = parse_args()
        self.total_programs = 0
        self.durations = {}
        self.exec_start = perf_counter()
        self.debug = self.args.debug
        self.stats = self.args.stats
        self.kbpath = self.args.kbpath
        self.max_solutions = self.args.max_solutions
        self.functional_test = self.args.functional_test
        self.clingo_args = [] if not self.args.clingo_args else self.args.clingo_args.split(' ')

    def __enter__(self):
        return self

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

    def show_stats(self, program):
        total_exec_time = perf_counter() - self.exec_start

        message = f'Total programs: {self.total_programs}\n'
        total_op_time = 0
        for operation, durations in self.durations.items():
            called = len(durations)
            total = sum(durations)
            mean = sum(durations)/len(durations)
            message += f'{operation.title()}: Called: {called} times | Total: {total:0.3f}s | Mean: {mean:0.4f}s\n'
            if operation != 'basic setup':
                total_op_time += total
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {total_exec_time:0.2f}s'
        print(message)