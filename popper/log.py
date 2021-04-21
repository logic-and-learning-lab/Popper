import argparse
from time import perf_counter
from contextlib import contextmanager

def parse_args():
    parser = argparse.ArgumentParser(description='Popper, an ILP engine based on learning from failures')
    parser.add_argument('kbpath', help = 'Path to the knowledge base one wants to learn on')

    parser.add_argument('--eval-timeout', type=float, default=0.1, help='Prolog evaluation timeout in seconds')
    parser.add_argument('--max-literals', type=int, default=100, help='Maximum number of literals allowed in program')
    parser.add_argument('--minimal-testing', type=bool, default=True, help='Use minimal testing during testing')
    parser.add_argument('--no-pruning', type=bool, default=False, help='Use pruning during constrain stage')

    parser.add_argument('--debug', default=False, help='Print debugging information to stderr')
    parser.add_argument('--stats', default= False, help='Print statistics at end of execution')
    
    return parser.parse_args()

class Experiment:
    def __init__(self):
        self.args = parse_args()
        self.total_programs = 0
        self.durations = {}

        self.exec_start = perf_counter()
    
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
            
    def stats(self, program):
        total_exec_time = perf_counter() - self.exec_start

        message = f'Total programs: {self.total_programs}\n\n'
        total_op_time = 0
        for operation, durations in self.durations.items():
            called = len(durations)
            total = sum(durations)
            mean = sum(durations)/len(durations)
            message += f'{operation.title()}\n'
            message += f'Called: {called} times | Total: {total:0.5f}s | Mean: {mean:0.5f}s\n\n'

            if operation != 'basic setup': total_op_time += total
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {total_exec_time:0.2f}s\n'
        
        print(message)