#!/usr/bin/env python

import threading
from popper.util import Settings, print_prog_score
from popper.loop import learn_solution
from popper.state import SearchState
from popper import stats

if __name__ == '__main__':
    settings = Settings.from_args()
    state = SearchState()

    t = threading.Thread(target=learn_solution, args=(settings, state), daemon=True)
    t.start()
    t.join(settings.timeout)

    if t.is_alive():
        print(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')

    prog, score = state.best_hypothesis, state.best_hypothesis_score
    if prog:
        print_prog_score(prog, score, settings, settings.noisy)
    else:
        print('NO SOLUTION')

    if settings.show_stats:
        stats.show()
