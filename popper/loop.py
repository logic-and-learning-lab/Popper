import time
from . select import Selector
from . util import timeout, chunk_list, flatten, print_prog, format_rule, format_prog
from . tester import Tester
# from . asptester import Tester
from . generate import Generator, Grounder, atom_to_symbol
from . bkcons import deduce_bk_cons
from clingo import Function, Number, Tuple_
from . core import Constrainer

SIMPLE_HACK = True

def prog_size(prog):
    return sum(1 + len(body) for head, body in prog)

seen = set()
def find_progs(settings, tester, grounder, cons, success_sets, chunk_pos, max_size=20):

    print('chunk_pos',chunk_pos)

    bootstrap_cons = deduce_cons(cons, chunk_pos)
    with settings.stats.duration('init'):

        # TODO: ADD NOGOOD ON SIZE
        generator = Generator(settings, grounder, bootstrap_cons, max_size)

    with generator.solver.solve(yield_ = True) as handle:
        for model in handle:
            atoms = model.symbols(shown = True)
            prog = generator.parse_model(atoms)

            settings.stats.total_programs += 1

            settings.stats.register_prog(prog)
            assert(format_prog(prog) not in seen)
            seen.add(format_prog(prog))

            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)



            chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(chunk_pos_covered) != len(chunk_pos)

            # print('inconsistent', inconsistent)
            # print('coverage', pos_covered)

            add_spec = False
            add_gen = False

            # always add an elimination constraint
            cons.add_elimination(prog)

            # if inconsistent, prune generalisations
            if inconsistent:
                add_gen = True
                cons.add_generalisation(prog)

            # if consistent, prune specialisations
            else:
                add_spec = True
                for e in settings.pos:
                    cons.add_specialisation(prog, e)

            # HACKY
            # if we already have a solution, any new rule must cover at least two examples
            if len(chunk_pos) > 1 and len(chunk_pos_covered) == 1:
                add_spec = True
                for e in settings.pos:
                    cons.add_specialisation(prog, e)

            # if SIMPLE_HACK and len(chunk_pos) > 1 and len(settings.best_prog) == 2 and len(chunk_pos_covered) != len(chunk_pos):
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
                # settings.stats.register_candidate_prog(prog)
                yield prog, pos_covered

            # if it covers all examples, stop
            if len(chunk_pos_covered) == len(chunk_pos) and not inconsistent:
                settings.logger.debug(f'Found complete and consistent program for examples: {chunk_pos}')
                return

            with settings.stats.duration('constrain.build'):
                new_cons = set()
                if add_spec:
                    new_cons.add(generator.build_specialisation_constraint(prog))
                if add_gen:
                    new_cons.add(generator.build_generalisation_constraint(prog))
                if not add_spec and not add_gen:
                    assert(False)

            with settings.stats.duration('constrain.ground'):
                s = set()
                for con in new_cons:
                    for grule in generator.get_ground_rules([(None, con)]):
                        h, b = grule
                        s.add(b)

            with settings.stats.duration('constrain.transform'):
                nogoods = []
                for b in s:
                    tmp = []
                    for sign, pred, args in b:
                        x = (atom_to_symbol(pred, args), sign)
                        tmp.append(x)
                    nogoods.append(tmp)

            with settings.stats.duration('constrain.add'):
                for tmp in nogoods:
                    model.context.add_nogood(tmp)

def deduce_cons(cons, chunk_pos):
    return set.intersection(*[cons.spec_cons[x] for x in chunk_pos]), cons.elim_cons, cons.gen_cons

def popper(settings):
    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            deduce_bk_cons(settings)

    tester = Tester(settings)
    cons = Constrainer(settings)
    grounder = Grounder()
    prog_coverage = {}
    success_sets = {}
    selector = Selector(settings, tester, prog_coverage)
    covered_examples = set()

    def find_solution(examples, max_size):
        # settings.stats.logger.info(f'Trying to cover:')
        # settings.stats.logger.info(f'{set(examples)}')
        for prog, coverage in find_progs(settings, tester, grounder, cons, success_sets, examples, max_size):

            # update coverage
            prog_coverage[prog] = coverage
            covered_examples.update(coverage)
            success_sets[coverage] = prog

            # if we find a program that covers all examples, stop
            if len(coverage) == len(settings.pos):
                selector.update_best_prog(prog)
                return True

            with settings.stats.duration('select'):
                new_solution_found = selector.update_best_prog(prog)
                if new_solution_found:
                    max_size = selector.max_size - 1
                    settings.max_literals = max_size
                    continue

    # in the first pass we try to cover each example
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

    # return
    # we then try to generalise over all examples
    find_solution(settings.pos, settings.max_literals)

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.stats



    # # # # # TODO: IF WE ALREADY HAVE A SOLUTION, ANY NEW RULE MUST COVER AT LEAST TWO EXAMPLES
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
            # for e in settings.pos.difference(pos_covered):
                # cons.add_specialisation(prog, e)



            # check whether subsumed by an already seen program
            # if so, prune specialisations
