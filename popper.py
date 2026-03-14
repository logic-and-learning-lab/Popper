#!/usr/bin/env python

from popper.util import init_settings, print_prog_score
from popper.loop import learn_solution
from popper import stats

if __name__ == '__main__':
    settings = init_settings()
    prog, score = learn_solution(settings)
    if prog:
        print_prog_score(prog, score)
    else:
        print('NO SOLUTION')
    if settings.show_stats:
        stats.show()
