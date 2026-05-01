from time import perf_counter

class _Timer:
    __slots__ = ['s', 'start']

    def __init__(self, stat_list):
        self.s = stat_list  # Points directly to the [called, total, max] list

    def __enter__(self):
        self.start = perf_counter()

    def __exit__(self, *_):
        d = perf_counter() - self.start
        self.s[0] += 1             # called
        self.s[1] += d             # total
        if d > self.s[2]:          # max
            self.s[2] = d

class Stats:
    def __init__(self):
        self.exec_start = perf_counter()
        self.total_programs = 0
        self.durations = {}

    def duration(self, operation):
        # Create the [called, total, max] list once, then pass its reference
        if operation not in self.durations:
            self.durations[operation] = [0, 0.0, 0.0]
        return _Timer(self.durations[operation])

    def show(self):
        total_exec = perf_counter() - self.exec_start
        total_op = sum(stat[1] for stat in self.durations.values())

        lines = [f'Num. programs: {self.total_programs}']

        # Sort by total time descending
        for op, (called, total, maximum) in sorted(self.durations.items(), key=lambda x: x[1][1], reverse=True):
            mean = total / called if called else 0
            pct = int((total / total_op) * 100) if total_op else 0

            lines.append(
                f'{op.title()}:\n \t Called: {called} times \t'
                f'Total: {total:.2f} \t Mean: {mean:.4f}\t'
                f'Max: {maximum:.3f} \t Percentage: {pct}%'
            )

        lines.append(f'Total operation time: {total_op:.2f}s')
        lines.append(f'Total execution time: {total_exec:.2f}s')

        message = '\n'.join(lines)
        print(message)
        return message

stats = Stats()
duration = stats.duration
show = stats.show
