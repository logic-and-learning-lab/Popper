import time
from . select import Selector
from . util import timeout, chunk_list, flatten, print_prog, format_rule, format_prog, rule_is_recursive
from . tester import Tester
# from . asptester import Tester
from . generate import Generator, Grounder, atom_to_symbol
from . bkcons import deduce_bk_cons
from clingo import Function, Number, Tuple_
from . core import Constrainer

def prog_size(prog):
    return sum(1 + len(body) for head, body in prog)

def find_progs(settings):
    tester = Tester(settings)
    cons = Constrainer(settings)
    grounder = Grounder()
    prog_coverage = {}
    success_sets = {}
    selector = Selector(settings, tester, prog_coverage)
    covered_examples = set()
    pos = settings.pos

    print('settings.max_literals', settings.max_literals)
    generator = Generator(settings, grounder, settings.max_literals)

    last_size = None
    seen = set()
    seen_inconsistent = set()

    with generator.solver.solve(yield_ = True) as handle:
        for model in handle:
            new_cons = set()

            atoms = model.symbols(shown = True)
            prog = generator.parse_model(atoms)



            prog_key = format_prog(prog)
            assert(prog_key not in seen)
            seen.add(prog_key)
            k = prog_size(prog)
            if last_size == None or k != last_size:
                print(k)
                last_size = k

            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            settings.stats.total_programs += 1
            settings.stats.register_prog(prog)

            # chunk_pos_covered = set([x for x in chunk_pos if x in pos_covered])
            incomplete = len(pos_covered) != len(pos)

            # print('stats', len(pos_covered), inconsistent)
            # print('coverage', pos_covered)

            add_spec = False
            add_gen = False

            # if inconsistent, prune generalisations
            if inconsistent:
                # print('***')
                # print('INCONSISTENT')
                # for rule in prog:
                    # print(format_rule(rule))
                add_gen = True
                cons.add_generalisation(prog)
                if len(prog) > 1:
                    for rule in prog:
                        if rule_is_recursive(rule):
                            continue
                        subprog = frozenset([rule])
                        if format_prog(subprog) in seen_inconsistent:
                            print('SUBPROG SHOULD NOT BE HERE')
                            print(format_rule(rule))
                            assert(False)
                        _, inconsistent1 = tester.test_prog(subprog)
                        if inconsistent1:
                            new_cons.add(generator.build_generalisation_constraint(subprog))
                            seen_inconsistent.add(format_prog(subprog))
                seen_inconsistent.add(prog_key)

            # if consistent, prune specialisations
            else:
                add_spec = True

            # if it does not cover any example, prune specialisations
            if len(pos_covered) == 0:
                add_spec = True

            # HACKY
            # if we already have a solution, a new rule must cover at least two examples
            if selector.solution_found and len(pos_covered) == 1:
                add_spec = True

            # check whether subsumed by an already seen program
            # if so, prune specialisations
            subsumed = False
            if len(pos_covered) > 0:
                subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets.keys())
                if subsumed:
                    add_spec = True

            if add_spec == False and selector.solution_found and len(selector.best_prog) == 1 and len(chunk_pos_covered) != len(pos):
                print('prune baby prune')


            # if consistent, covers at least one example, and is not subsumed, yield candidate program
            if not inconsistent and not subsumed and len(pos_covered) > 0:
                # update coverage
                prog_coverage[prog] = pos_covered
                covered_examples.update(pos_covered)
                success_sets[pos_covered] = prog

                print('CALLING SELECT')
                for rule in prog:
                    print(format_rule(rule))

                with settings.stats.duration('select'):
                    new_solution_found = selector.update_best_prog(prog)
                    if new_solution_found:
                        k = prog_size(selector.best_prog)
                        # print('FOUND SOMETHING', k)
                        for i in range(k, settings.max_literals+1):
                            tmp = [(atom_to_symbol("size", (i,)), True)]
                            # print(tmp)
                            model.context.add_nogood(tmp)
                        settings.max_literals = k-1

            # if it covers all examples, stop
            if not inconsistent and len(pos_covered) == len(pos):
                print('STOPPING')
                # settings.logger.debug(f'Found complete and consistent program for examples: {chunk_pos}')
                return

            with settings.stats.duration('constrain.build'):
                if add_spec:
                    # print("ADD_SPEC")
                    new_cons.add(generator.build_specialisation_constraint(prog))
                # TODO: IF NO PI OR RECURSION THEN NO NEED TO ADD GEN
                if add_gen:
                    con = generator.build_generalisation_constraint(prog)
                    new_cons.add(con)
                    # for a in generator.con_to_strings(x):
                        # pass
                        # print(a)
                    # for grule in generator.get_ground_rules([(None, x)]):
                        # print(grule)
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
                    # pass
                    model.context.add_nogood(tmp)

def deduce_cons(cons, chunk_pos):
    return set.intersection(*[cons.spec_cons[x] for x in chunk_pos]), cons.elim_cons, cons.gen_cons

def popper(settings):
    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            deduce_bk_cons(settings)


    # def find_solution(examples, max_size):
        # settings.stats.logger.info(f'Trying to cover:')
        # settings.stats.logger.info(f'{set(examples)}')
    find_progs(settings)

            # # update coverage
            # prog_coverage[prog] = coverage
            # covered_examples.update(coverage)
            # success_sets[coverage] = prog

            # # # if we find a program that covers all examples, stop
            # # if len(coverage) == len(settings.pos):
            # #     selector.update_best_prog(prog)
            # #     return True

            # with settings.stats.duration('select'):
            #     new_solution_found = selector.update_best_prog(prog)
            #     if new_solution_found:
            #         max_size = selector.max_size - 1
            #         settings.max_literals = max_size
            #         continue

    # # in the first pass we try to cover each example
    # for pos in settings.pos:
    #     # if all examples are covered, stop
    #     if len(covered_examples) == len(settings.pos):
    #         break

    #     # if example is covered, stop
    #     if pos in covered_examples:
    #         continue

    #     # find a solution
    #     optimal_found = find_solution(frozenset([pos]), settings.max_literals)
    #     if optimal_found:
    #         return

    # return
    # we then try to generalise over all examples
    # find_solution(settings.pos, settings.max_literals)

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
