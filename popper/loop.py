import logging
import sys
from datetime import datetime
from . util import Settings2, Stats, timeout, format_prog, chunk_list, flatten
# from . tester import Tester
from . pltester import Tester
# from . asptester import Tester
from . generate import Generator, Constrainer, specialisation_constraint, elimination_constraint, format_constraint
from . select import Selector
import time

def dbg(*args):
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print(current_time, *args)

# ss = {}

def find_progs(settings, tester, cons, prog_coverage, chunk_pos):
    bootstrap_cons = deduce_cons(cons, chunk_pos)
    generator = Generator(settings, bootstrap_cons)

    for num_body_literals in range(1, settings.max_size+1):
        generator.update_num_literals(num_body_literals)

        while True:
            with settings.stats.duration('gen'):
                prog = generator.gen_prog()
            if prog == None:
                break

            # TMP!
            prog = frozenset([prog])
            # print(format_prog(rule))

            settings.stats.total_programs += 1

            with settings.stats.duration('test'):
                inconsistent, pos_covered = tester.test_prog(prog)

            chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(chunk_pos_covered) != len(chunk_pos)

            # dbg(format_prog(rule), f'incomplete:{incomplete}', f'inconsistent:{inconsistent}', len(pos_covered))

            # print('')
            # print(format_prog(rule))
            # print(f'inconsistent:{inconsistent}')
            # print(f'incomplete:{incomplete}')
            # print(f'totally incomplete:{len(pos_covered) == 0}')
            # print('pos_covered',pos_covered)
            # print('chunk_pos_covered',chunk_pos_covered

            add_spec = False
            spec_con = specialisation_constraint(prog)
            elim_con = elimination_constraint(prog)

            # print(format_constraint(elim_con))
            # print(format_constraint(elim_con))

            # always add an elimination constraint
            cons.add_elimination(elim_con)

            # if not inconsistent and len(pos_covered) > 0:
            #     xs = frozenset(pos_covered)
            #     if xs in ss:
            #         add_spec = True
            #         # con = specialisation_constraint(rule)
            #         # cons.add(con)
            #         # print('')
            #         # print('---')
            #         # print('SKIP')
            #         # print('OLD', format_prog(ss[xs]))
            #         # print('NEW', format_prog(rule))
            #         for e in settings.pos:
            #             cons.add_specialisation(rule, e)
            #         # for e in pos:
            #             # spec_cons[e].add(con)
            #         # continue
            #         # skip = True
            #     else:
            #         ss[xs] = rule


            # if inconsistent, then rule all generalisations
            if inconsistent:
                pass
                # cons.add_generalisation(rule)
                # add_gen = True


            if not inconsistent:
                # if consistent, no need to specialise
                add_spec = True
                for e in settings.pos:
                    cons.add_specialisation(spec_con, e)

            # for any examples uncovered, save a specialisation constraint
            for e in settings.pos.difference(pos_covered):
                cons.add_specialisation(spec_con, e)

            # if it does not cover any chunk example, then prune specialisations
            if len(chunk_pos_covered) == 0:
                add_spec = True

            # if consistent and covers at least one pos example, yield rule
            if len(pos_covered) > 0 and not inconsistent:
                dbg(f'yield prog: {format_prog(prog)}')
                prog_coverage[prog] = pos_covered
                yield prog

            # if it covers all examples, add candidate rule and prune specialisations
            if len(chunk_pos_covered) == len(chunk_pos) and not inconsistent:
                return

            with settings.stats.duration('constrain'):
                if add_spec:
                    generator.add_constraint(spec_con)
                else:
                    generator.add_constraint(elim_con)
    # assert(False)
    # exit()

def deduce_cons(cons, chunk_pos):
    return set.intersection(*[cons.spec_cons[x] for x in chunk_pos]) | cons.elim_cons

def popper(ignore, stats):
    settings = Settings2()
    settings.stats=stats
    cons = Constrainer(settings)
    selector = Selector(settings)
    tester = Tester(settings)

    all_chunks = [[x] for x in settings.pos]
    chunk_size = 1
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

            for prog in find_progs(settings, tester, cons, selector.prog_coverage, chunk_pos):
                covered_examples.update(selector.prog_coverage[prog])
                with settings.stats.duration('select'):
                    selector.update_best_prog(prog)
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
