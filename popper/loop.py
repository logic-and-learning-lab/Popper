import time
from . select import Selector
from . util import timeout, chunk_list, flatten, print_prog, format_rule
from . tester import Tester
# from . asptester import Tester
from . generate import Generator, Constrainer, Grounder

SIMPLE_HACK = True

def find_progs(settings, tester, grounder, cons, prog_coverage, success_sets, chunk_pos, max_size=20):
    bootstrap_cons = deduce_cons(cons, chunk_pos)
    # # TODO: WE CAN TAKE THE UNION OF SPECIALISATIONS WHEN THE BEST SOLUTION ONLY HAS TWO RULES
    # if SIMPLE_HACK and settings.best_prog != None and len(chunk_pos) > 1 and len(settings.best_prog) == 2:
        # bootstrap_cons = set.union(*[cons.spec_cons[x] for x in chunk_pos]), cons.elim_cons, cons.gen_cons

    # a = len(set.union(*[cons.spec_cons[x] for x in chunk_pos]))
    # b = len(set.intersection(*[cons.spec_cons[x] for x in chunk_pos]))
    # counts = {}
    # for x in chunk_pos:
    #     for con in cons.spec_cons[x]:
    #         if con not in counts:
    #             counts[con] += 1
    #         else:
    #             counts[con] = 1
    # c = len(con for con, count in counts.items() if count >= 1)

    with settings.stats.duration('bootstrap'):
        generator = Generator(settings, grounder, bootstrap_cons)

    for size in range(1, max_size+1):

        if size > settings.max_literals:
            continue

        settings.stats.logger.info(f'Searching size: {size}')
        generator.update_num_literals(size)

        while True:
            with settings.stats.duration('gen'):
                prog = generator.gen_prog()
            if prog == None or prog == []:
                break

            settings.stats.total_programs += 1

            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            settings.stats.register_prog(prog)

            chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(chunk_pos_covered) != len(chunk_pos)

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

            # # # # TODO: IF WE ALREADY HAVE A SOLUTION, ANY NEW RULE MUST COVER AT LEAST TWO EXAMPLES
            # if len(chunk_pos) > 1 and len(chunk_pos_covered) < 2:
            #     # print('asda2')
            #     add_spec = True
            #     for e in settings.pos:
            #         cons.add_specialisation(prog, e)
            # if SIMPLE_HACK and len(chunk_pos) > 1 and len(settings.best_prog) == 2 and len(chunk_pos_covered) != len(chunk_pos):
            #     # print('asda3')
            #     add_spec = True
            #     for e in settings.pos:
            #         cons.add_specialisation(prog, e)

            # if too specific for an example e, save a specialisation constraint for e
            for e in settings.pos.difference(pos_covered):
                cons.add_specialisation(prog, e)

            # if it does not cover any example, prune specialisations
            if len(chunk_pos_covered) == 0:
                add_spec = True

            # check whether subsumed by an already seen program
            # if so, prune specialisations
            subsumed = False
            if len(pos_covered) > 0:
                subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets.keys())
                if subsumed:
                    add_spec = True
                    for e in settings.pos:
                        cons.add_specialisation(prog, e)

            # if consistent, covers at least one example, and is not subsumed, yield candidate program
            if len(pos_covered) > 0 and not inconsistent and not subsumed:
                success_sets[pos_covered] = prog
                settings.stats.register_candidate_prog(prog)
                prog_coverage[prog] = pos_covered
                yield prog

            # if it covers all examples, stop
            if len(chunk_pos_covered) == len(chunk_pos) and not inconsistent:
                settings.stats.logger.info('Found complete and consistent program for examples')
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
    grounder = Grounder()
    selector = Selector(settings, tester)
    success_sets = {}
    covered_examples = set()

    def find_solution(examples, max_size):
        settings.stats.logger.info(f'Trying to cover: {examples}')
        for prog in find_progs(settings, tester, grounder, cons, selector.prog_coverage, success_sets, examples, max_size):

            # if we find a program that covers all examples, stop
            if selector.prog_coverage[prog] == settings.pos:
                selector.update_best_prog(prog)
                return True

            # update coverage
            covered_examples.update(selector.prog_coverage[prog])

            with settings.stats.duration('select'):
                new_solution_found = selector.update_best_prog(prog)
                if new_solution_found:
                    settings.best_prog = selector.best_prog
                    max_size = selector.max_size - 1
                    settings.max_literals = max_size
                    continue

    for pos in settings.pos:
        # if all examples are covered, stop
        if len(covered_examples) == len(settings.pos):
            break

        # if example is covered, stop
        if pos in covered_examples:
            continue

        # find a solution
        optimal_found = find_solution(frozenset([pos]), settings.max_literals)
        if optimal_found:
            return

    find_solution(settings.pos, settings.max_literals)

def learn_solution(settings):
    timeout(popper, (settings,), timeout_duration=int(settings.timeout))
    return settings.solution, settings.stats
