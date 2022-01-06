#!/usr/bin/env python3
from popper.loop import show_hspace, learn_solution
from popper.util import parse_settings

if __name__ == '__main__':
    settings = parse_settings()
    if settings.hspace:
        show_hspace(settings)
    else:
        _prog, stats = learn_solution(settings)
        stats.log_final_result()
        if settings.stats:
            stats.show()