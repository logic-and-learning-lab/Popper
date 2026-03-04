from time import perf_counter
from contextlib import contextmanager
from typing import NamedTuple

class DurationSummary(NamedTuple):
    operation: str
    called: int
    total: float
    mean: float
    maximum: float

class Stats:
    def __init__(self):
        self.exec_start = perf_counter()
        self.total_programs = 0
        self.durations = {}

    def total_exec_time(self):
        return perf_counter() - self.exec_start

    @contextmanager
    def duration(self, operation):
        start = perf_counter()
        try:
            yield
        finally:
            duration = perf_counter() - start
            if operation not in self.durations:
                self.durations[operation] = []
            self.durations[operation].append(duration)

    def show(self):
        message = f'Num. programs: {self.total_programs}\n'
        total_op_time = sum(summary.total for summary in self.duration_summary())

        for summary in self.duration_summary():
            percentage = int((summary.total/total_op_time)*100)
            message += f'{summary.operation}:\n\tCalled: {summary.called} times \t ' + \
                       f'Total: {summary.total:0.2f} \t Mean: {summary.mean:0.4f} \t ' + \
                       f'Max: {summary.maximum:0.3f} \t Percentage: {percentage}%\n'
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {self.total_exec_time():0.2f}s'
        print(message)
        return message

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

stats = Stats()
duration = stats.duration
show = stats.show