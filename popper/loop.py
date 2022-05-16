import logging
import sys
import numpy as np
from datetime import datetime
from . util import Settings2, Stats, timeout, format_prog, chunk_list, flatten
from . pltester import Tester
# from . tester import Tester
# from . asptester import Tester
from . generate import Generator, Constrainer, format_constraint
from . select import Selector
import time

def dbg(*args):
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print(current_time, *args)

def find_progs(settings, tester, cons, prog_coverage, chunk_pos, max_size=20):
    bootstrap_cons = deduce_cons(cons, chunk_pos)
    generator = Generator(settings, bootstrap_cons)

    for size in range(1, max_size+1):
        generator.update_num_literals(size)
        print(f'SEARCHING SIZE: {size}')

        while True:
            with settings.stats.duration('gen'):
                prog = generator.gen_prog()
            if prog == None or prog == []:
                break

            settings.stats.total_programs += 1

            # print('')
            # print(f'prog num: {settings.stats.total_programs}')
            # print(format_prog(prog))


            with settings.stats.duration('test'):
                inconsistent, pos_covered = tester.test_prog(prog)

            chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(chunk_pos_covered) != len(chunk_pos)

            # print(f'inconsistent:{inconsistent}')
            # print(f'incomplete:{incomplete}')
            # print(f'totally incomplete:{len(pos_covered) == 0}')

            add_spec = False
            add_gen = False

            # always add an elimination constraint
            cons.add_elimination(prog)

            # if not inconsistent and len(pos_covered) > 0:
            #     xs = frozenset(pos_covered)
            #     if xs in ss:
            #         add_spec = True
            #         for e in settings.pos:
            #             cons.add_specialisation(spec_con, e)
            #     else:
            #         ss[xs] = rule

            # if inconsistent, then rule all generalisations
            if inconsistent:
                add_gen = True

            if not inconsistent:
                # if consistent, no need to specialise
                add_spec = True
                for e in settings.pos:
                    cons.add_specialisation(prog, e)

            # for any examples uncovered, save a specialisation constraint
            for e in settings.pos.difference(pos_covered):
                cons.add_specialisation(prog, e)

            # if it does not cover any chunk example, then prune specialisations
            if len(chunk_pos_covered) == 0:
                add_spec = True

            # if consistent and covers at least one pos example, yield rule
            if len(pos_covered) > 0 and not inconsistent:
                print('rule')
                print(format_prog(prog))
                prog_coverage[prog] = pos_covered
                yield prog

            # if it covers all examples, add candidate rule and prune specialisations
            if len(chunk_pos_covered) == len(chunk_pos) and not inconsistent:
                return

            with settings.stats.duration('constrain'):
                new_cons = set()
                if add_spec:
                    new_cons.update(generator.build_specialisation_constraint(prog))
                if add_gen:
                    new_cons.update(generator.build_generalisation_constraint(prog))
                if not add_spec and not add_gen:
                    new_cons.update(generator.build_elimination_constraint(prog))
                generator.add_constraints(new_cons)
    # assert(False)
    # exit()

def deduce_cons(cons, chunk_pos):
    return set.intersection(*[cons.spec_cons[x] for x in chunk_pos]), cons.elim_cons

def popper(ignore, stats):
    settings = Settings2()
    # note: tester loads the examples and adds them to settings
    tester = Tester(settings)
    settings.stats=stats
    cons = Constrainer(settings)
    selector = Selector(settings)

    all_chunks = [[x] for x in settings.pos]
    chunk_size = 1
    max_size = 20

    # chunk_size = len(settings.pos)

    while chunk_size <= len(settings.pos):
        print('CHUNK_SIZE', chunk_size)
        chunks = list(chunk_list(all_chunks, chunk_size))

        covered_examples = set()

        for chunk_pos in chunks:
            chunk_pos = set(flatten(chunk_pos))
            # print('chunk_pos', chunk_pos)

            # if all examples are covered, stop
            if len(covered_examples) == len(chunks):
                break

            if chunk_pos.issubset(covered_examples):
                continue

            for prog in find_progs(settings, tester, cons, selector.prog_coverage, chunk_pos, max_size):
                covered_examples.update(selector.prog_coverage[prog])
                with settings.stats.duration('select'):
                    new_solution = selector.update_best_prog(prog)
                    if new_solution and len(chunk_pos) == 1:
                        max_size = selector.max_size - 1

                # TODO: CONSTRAIN PROGRAM SIZE

        # chunk_size += chunk_size
        if chunk_size == 1:
            chunk_size = len(settings.pos)
        elif chunk_size == len(settings.pos):
            chunk_size += 1

    return selector.best_program


def learn_solution(settings):
    stats = Stats(log_best_programs=settings.info)
    stats.solution_found = False
    log_level = logging.DEBUG if settings.debug else logging.INFO
    logging.basicConfig(level=log_level, stream=sys.stderr, format='%(message)s')
    timeout(popper, (settings, stats), timeout_duration=int(settings.timeout))

    return stats.solution_found, stats
