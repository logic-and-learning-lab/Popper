#!/usr/bin/env python3

from popper.util import Settings, parse_settings
from popper.loop import learn_solution, show_hspace

if __name__ == '__main__':
    settings = parse_settings()
    if settings.hspace:
        show_hspace(settings)
    else:
        _prog, stats = learn_solution(settings)
        stats.log_final_result()
        if settings.stats:
            stats.show()