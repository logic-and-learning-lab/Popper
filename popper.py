from popper.util import Settings, print_prog
from popper.loop import learn_solution

if __name__ == '__main__':
    settings = Settings()
    prog, stats = learn_solution(settings)
    if prog != None:
        print_prog(prog)
    if settings.show_stats:
        stats.show()
