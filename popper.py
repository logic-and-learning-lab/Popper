from popper.util import Settings2
from popper.loop import learn_solution

if __name__ == '__main__':
    settings = Settings2()
    if settings.hspace:
        show_hspace(settings)
    else:
        _prog, stats = learn_solution(settings)
        if settings.stats:
            stats.show()