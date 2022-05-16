import logging
import sys
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

def find_progs(settings, tester, cons, prog_coverage, success_sets, chunk_pos, max_size=20):
    bootstrap_cons = deduce_cons(cons, chunk_pos)
    generator = Generator(settings, bootstrap_cons)

    for size in range(1, max_size+1):
        settings.stats.logger.info(f'SEARCHING SIZE: {size}')
        generator.update_num_literals(size)

        while True:
            with settings.stats.duration('gen'):
                prog = generator.gen_prog()
            if prog == None or prog == []:
                break

            settings.stats.total_programs += 1

            with settings.stats.duration('test'):
                pos_covered, neg_covered = tester.test_prog(prog)

            settings.stats.register_prog(prog)

            chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(chunk_pos_covered) != len(chunk_pos)
            inconsistent = len(neg_covered) > 0

            add_spec = False
            add_gen = False

            # always add an elimination constraint
            cons.add_elimination(prog)

            # if inconsistent, prune generalisations
            if inconsistent:
                add_gen = True

            # if consistent, prune specialisations
            if not inconsistent:
                add_spec = True
                for e in settings.pos:
                    cons.add_specialisation(prog, e)

            # if too specific for an example e, save a specialisation constraint for e
            for e in settings.pos.difference(pos_covered):
                cons.add_specialisation(prog, e)

            # if it does not cover any example, prune specialisations
            if len(chunk_pos_covered) == 0:
                add_spec = True

            # if consistent and covers at least one example
            if len(pos_covered) > 0 and not inconsistent:
                # check whether subsumed by an already seen program
                # if so, prune specialisations and do not yield
                if pos_covered in success_sets:
                    add_spec = True
                    for e in settings.pos:
                        cons.add_specialisation(prog, e)
                else:
                    success_sets[pos_covered] = prog
                    settings.stats.register_candidate_prog(prog)
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

def deduce_cons(cons, chunk_pos):
    return set.intersection(*[cons.spec_cons[x] for x in chunk_pos]), cons.elim_cons, cons.gen_cons

def popper(settings):
    tester = Tester(settings)
    cons = Constrainer(settings)
    selector = Selector(settings)

    all_chunks = [[x] for x in settings.pos]
    chunk_size = 1
    # chunk_size = len(settings.pos)
    max_size = 20
    success_sets = {}

    while chunk_size <= len(settings.pos):
        chunks = list(chunk_list(all_chunks, chunk_size))

        covered_examples = set()

        for chunk_pos in chunks:
            chunk_pos = frozenset(flatten(chunk_pos))

            # if all examples are covered, stop
            if len(covered_examples) == len(chunks):
                break

            if chunk_pos.issubset(covered_examples):
                continue

            for prog in find_progs(settings, tester, cons, selector.prog_coverage, success_sets, chunk_pos, max_size):
                covered_examples.update(selector.prog_coverage[prog])
                with settings.stats.duration('select'):
                    new_solution_found = selector.update_best_prog(prog)
                    if new_solution_found and len(chunk_pos) == 1:
                        max_size = selector.max_size - 1
                # TODO: CONSTRAIN PROGRAM SIZE

        # chunk_size += chunk_size
        if chunk_size == 1:
            chunk_size = len(settings.pos)
        elif chunk_size == len(settings.pos):
            chunk_size += 1

def learn_solution(settings):
    timeout(popper, (settings,), timeout_duration=int(settings.timeout))
    return settings.solution, settings.stats
