#!/usr/bin/env python

from popper.util import Settings, print_prog_score
from popper.loop import learn_solution
from popper import stats

if __name__ == '__main__':
    settings = Settings.from_args()
    prog, score = learn_solution(settings)
    if prog:
        print_prog_score(prog, score, settings, settings.noisy)
    else:
        print('NO SOLUTION')
    if settings.show_stats:
        stats.show()
