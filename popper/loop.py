import time
import numbers
from . combine import Combiner
from . explain import Explainer
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, order_rule
from . tester import Tester
from . generate import Generator, Grounder, parse_model
from . bkcons import deduce_bk_cons
from clingo import Function, Number, Tuple_

def prog_size(prog):
    return sum(1 + len(body) for head, body in prog)

def arg_to_symbol(arg):
    if isinstance(arg, numbers.Number):
        return Number(arg)
    if isinstance(arg, tuple):
        return Tuple_(tuple(arg_to_symbol(a) for a in arg))
    if isinstance(arg, str):
        return Function(arg)
    assert False, f'Unhandled argtype({type(arg)}) in aspsolver.py arg_to_symbol()'

def atom_to_symbol(pred, args):
    xs = tuple(arg_to_symbol(arg) for arg in args)
    return Function(name = pred, arguments = xs)



def parse_handles(generator, new_handles):
    for x, rule in new_handles:
        # print(x, rule
        head, body = rule
        # print('X1', x, head, ','.join(map(str,body)))
        # TODO: add cachcing
        for h, b in generator.get_ground_rules(rule):
            _, p, args = h
            # z = f'{p}{args}:-' + ','.join(f'{b_pred}{b_args}' for _, b_pred, b_args in b) + '.'

            out_h = (p, args)
            out_b = frozenset((b_pred, b_args) for _, b_pred, b_args in b)
            yield (x, (out_h, out_b))


tmp_seen = set()

# @profile
def constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model, all_nogoods):
    with settings.stats.duration('constrain'):
        ground_bodies = set()
        for con in new_cons:
            if con in tmp_seen:
                print('shit')
            tmp_seen.add(con)

            ground_rules = generator.get_ground_rules((None, con))
            # if any(x.predicate == 'seen_rule' for x in con):
                # print('A',', '.join(str(x) for x in sorted(con, key=lambda x: x.predicate)))
            for ground_rule in ground_rules:
                _ground_head, ground_body = ground_rule
                # if any(x.predicate == 'seen_rule' for x in con):
                    # print('B',ground_body)
                ground_bodies.add(ground_body)
                ground_con = []
                for sign, pred, args in ground_body:
                    x = ''
                    if len(args) == 1:
                        x = f'{pred}({args[0]})'
                    else:
                        x = f'{pred}{args}'
                    x = x.replace("'","")
                    if sign == False:
                        x = 'not ' + x
                    ground_con.append(x)
                y = ':-' + ', '.join(sorted(ground_con)) + '.'
                # print('B', y)
                # all_ground_cons.add(y)
                all_ground_cons.add(frozenset(ground_body))

        nogoods = []
        for ground_body in ground_bodies:
            nogood = []
            for sign, pred, args in ground_body:
                k = hash((sign, pred, args))
                if k in cached_clingo_atoms:
                    nogood.append(cached_clingo_atoms[k])
                else:
                    x = (atom_to_symbol(pred, args), sign)
                    nogood.append(x)
                    cached_clingo_atoms[k] = x
            nogoods.append(nogood)

        for nogood in nogoods:
            settings.nogoods += 1
            model.context.add_nogood(nogood)
            all_nogoods.append(nogood)


# @profile
def popper(settings):
    if settings.bkcons:
        deduce_bk_cons(settings)

    settings.nogoods = 0

    tester = Tester(settings)
    explainer = Explainer(settings, tester)
    grounder = Grounder()
    combiner = Combiner(settings, tester)

    pos = settings.pos

    success_sets = {}
    last_size = None

    # caching
    cached_clingo_atoms = {}

    # TMP SETS
    seen_covers_only_one_gen = set()
    seen_covers_only_one_spec = set()
    seen_incomplete_gen = set()
    seen_incomplete_spec = set()

    all_cons = set()
    all_nogoods = []
    all_handles = set()
    bad_handles = set()
    bad_progs = set()
    all_ground_cons = set()

    num_gen=0
    num_red1=0
    num_red2=0
    num_specs=0

    generator = Generator(settings, grounder)

    max_size = (1 + settings.max_body) * settings.max_rules
    # max_size = 11
    # max_size = 9
    # max_size = 3
    for size in range(1, max_size+1):

        print('SIZE', size, 'MAX_SIZE', settings.max_literals)
        if size > settings.max_literals:
            break

        # with settings.stats.duration('init'):
            # generator = Generator(settings, grounder, size, all_handles, bad_handles, all_ground_cons)
        generator.update_number_of_literals(size)

        # print('SIZE', size, max_size)
        with settings.stats.duration('init'):
            generator.update_solver(size, all_handles, bad_handles, all_ground_cons, bad_progs)

        all_ground_cons = set()
        all_handles = set()
        bad_handles = set()
        bad_progs = set()

        nogoods_added = False
        # for h,b in set(all_handles):
            # print(h)

        with generator.solver.solve(yield_ = True) as handle:
            handle = iter(handle)

            while True:
                # print(num_gen, num_specs, num_red1, num_red2)
                model = None

                # GENERATE A PROGRAM
                with settings.stats.duration('generate'):
                    # get the next model from the solver
                    model = next(handle, None)
                    if model is None:
                        break
                    atoms = model.symbols(shown = True)
                    prog, rule_ordering, directions = parse_model(atoms)

                # with settings.stats.duration('bootstrap'):
                #     if not nogoods_added:
                #         for x in all_nogoods:
                #             model.context.add_nogood(x)
                #         nogoods_added = True

                settings.stats.total_programs += 1
                settings.logger.debug(f'Program {settings.stats.total_programs}:')
                for rule in order_prog(prog):
                    settings.logger.debug(format_rule(order_rule(rule)))
                    # if rule_is_recursive(rule):
                        # r1 = order_rule(rule)
                        # r2 = order_rule2(rule)
                        # if r1 != r2:
                        #     print('--')
                        #     print('R1', format_rule(r1))
                        #     print('R2', format_rule(r2))

                # TEST A PROGRAM
                with settings.stats.duration('test'):
                    pos_covered, inconsistent = tester.test_prog(prog)

                new_cons = set()

                pruned_subprog = False

                if settings.explain:
                    with settings.stats.duration('explain'):
                        explainer.add_seen_prog(prog)
                        if len(pos_covered) == 0:
                            for subprog in explainer.explain_totally_incomplete2(prog, directions):
                                pruned_subprog = True
                                # print('\tTOTALLY INCOMPLETE')
                                # for rule in order_prog(subprog):
                                #     print('\t',format_rule(order_rule(rule)))
                                # TODO: ADD RULE ORDERING
                                new_handles, con = generator.build_specialisation_constraint(subprog)
                                new_cons.add(con)
                                # print(','.join(str(x) for x in con))
                                all_handles.update(parse_handles(generator, new_handles))

                                if not settings.pi_enabled and settings.recursion_enabled:
                                    if len(subprog) == 1:
                                        bad_handle, new_handles, con = generator.redundancy_constraint1(subprog)
                                        bad_handles.add(bad_handle)
                                        new_cons.add(con)
                                        all_handles.update(parse_handles(generator, new_handles))
                                    else:
                                        new_cons.add(generator.redundancy_constraint2(subprog))

                                        new_handles, con = generator.redundancy_constraint3(prog)
                                        all_handles.update(parse_handles(generator, new_handles))
                                        bad_progs.add(con)

                if inconsistent and prog_is_recursive(prog):
                    combiner.add_inconsistent(prog)

                # messy way to track program size
                k = prog_size(prog)
                if last_size == None or k != last_size:
                    last_size = k
                    settings.logger.info(f'Searching programs of size: {k}')

                add_spec = False
                add_gen = False
                add_redund1 = False
                add_redund2 = False

                if inconsistent:
                    # if inconsistent, prune generalisations
                    add_gen = True
                    # if the program has multiple rules, test the consistency of each non-recursive rule as we might not have seen it before
                    if len(prog) > 1:
                        for rule in prog:
                            if rule_is_recursive(rule):
                                continue
                            subprog = frozenset([rule])
                            # TODO: ADD CACHING IF THIS STEP BECOMES TOO EXPENSIVE
                            if tester.is_inconsistent(subprog):
                                new_handles, con = generator.build_generalisation_constraint(subprog)
                                new_cons.add(con)
                                all_handles.update(parse_handles(generator, new_handles))
                else:
                    # if consistent, prune specialisations
                    add_spec = True

                # if consistent and partially complete test whether functional
                if not inconsistent and settings.functional_test and len(pos_covered) > 0 and tester.is_non_functional(prog):
                    # if not functional, rule out generalisations and set as inconsistent
                    add_gen = True
                    # v.important: do not prune specialisations!
                    add_spec = False
                    inconsistent = True

                # if it does not cover any example, prune specialisations
                if len(pos_covered) == 0:
                    # print('\tTOTALLY INCOMPLETE')
                    # for rule in order_prog(prog):
                        # print('\t',format_rule(order_rule(rule)))
                    add_spec = True
                    # if recursion and no PI, then apply redundancy constraints
                    if settings.recursion_enabled and not settings.pi_enabled and settings.test:
                        if len(prog) == 1:
                            add_redund1 = True
                        else:
                            add_redund2 = True

                # check whether subsumed by an already seen program
                subsumed = False
                if len(pos_covered) > 0 and not prog_is_recursive(prog):
                    subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
                    # if so, prune specialisations
                    if subsumed:
                        add_spec = True

                # HACKY TMP IDEAS
                if not settings.recursion_enabled:

                    # if we already have a solution, a new rule must cover at least two examples
                    if not add_spec and combiner.solution_found and len(pos_covered) == 1:
                        # print('HACKY1')
                        add_spec = True

                    # backtracking idea
                    # keep track of programs that only cover one example
                    # once we find a solution, we apply specialisation/generalisation constraints
                    if len(pos_covered) == 1:
                        if not add_gen:
                            seen_covers_only_one_gen.add(prog)
                        if not add_spec:
                            seen_covers_only_one_spec.add(prog)
                    # keep track of programs that do not cover all the examples
                    if len(pos_covered) != len(pos):
                        if not add_gen:
                            seen_incomplete_gen.add(prog)
                        if not add_spec:
                            seen_incomplete_spec.add(prog)

                    # if we found a solution, then prune programs that only cover one example
                    # reset the sets to avoid adding duplicate constraints
                    if combiner.solution_found:
                        for x in seen_covers_only_one_gen:
                            new_handles, con = generator.build_generalisation_constraint(x)
                            new_cons.add(con)
                            all_handles.update(parse_handles(generator, new_handles))

                        seen_covers_only_one_gen = set()
                        for x in seen_covers_only_one_spec:
                            new_handles, con = generator.build_specialisation_constraint(x)
                            new_cons.add(con)
                            all_handles.update(parse_handles(generator, new_handles))
                        seen_covers_only_one_spec = set()

                        if len(combiner.best_prog) <= 2:
                            for x in seen_incomplete_gen:
                                new_handles, con = generator.build_generalisation_constraint(x)
                                new_cons.add(con)
                                all_handles.update(parse_handles(generator, new_handles))
                            for x in seen_incomplete_spec:
                                new_handles, con = generator.build_specialisation_constraint(x)
                                new_cons.add(con)
                                all_handles.update(parse_handles(generator, new_handles))
                            seen_incomplete_gen = set()
                            seen_incomplete_spec = set()


                # if consistent, covers at least one example, and is not subsumed, try to find a solution
                if not inconsistent and not subsumed and len(pos_covered) > 0:
                    # update success sets
                    success_sets[pos_covered] = prog

                    with settings.stats.duration('combine'):
                        new_solution_found = combiner.update_best_prog(prog, pos_covered)

                    # if we find a new solution, update the maximum program size
                    if new_solution_found:
                        # exit()
                        for i in range(combiner.max_size, settings.max_literals+1):
                            size_con = [(atom_to_symbol("size", (i,)), True)]
                            model.context.add_nogood(size_con)
                        settings.max_literals = combiner.max_size-1

                # if it covers all examples, stop
                if not inconsistent and len(pos_covered) == len(pos):
                    return


                # print(f'add_spec: {add_spec} add_gen:{add_gen} add_redund1:{add_redund1} add_redund2:{add_redund2} skipping:{pruned_subprog}')


                if add_spec and not pruned_subprog:
                    # pass
                    # print('ADD_SPEC')
                    new_handles, con = generator.build_specialisation_constraint(prog, rule_ordering)
                    new_cons.add(con)
                    all_handles.update(parse_handles(generator, new_handles))
                    num_specs += len(generator.get_ground_rules((None, con)))
                    # for _, x in generator.get_ground_rules((None, con)):
                        # print(','.join(sorted(map(str,x))))
                # if add_gen and settings.recursion_enabled:
                if add_gen:
                    # print('MOO')
                    # pass
                    new_handles, con = generator.build_generalisation_constraint(prog, rule_ordering)
                    new_cons.add(con)
                    all_handles.update(parse_handles(generator, new_handles))
                    # try:
                    num_gen += len(generator.get_ground_rules((None, con)))
                    # except:
                        # print(','.join(map(str,con)))
                        # exit()
                if add_redund1 and not pruned_subprog and settings.test:
                    # pass
                    bad_handle, new_handles, con = generator.redundancy_constraint1(prog)
                    new_cons.add(con)
                    bad_handles.add(bad_handle)
                    all_handles.update(parse_handles(generator, new_handles))
                    num_red1 += len(generator.get_ground_rules((None, con)))
                if add_redund2 and not pruned_subprog and settings.test:
                    # pass
                    con = generator.redundancy_constraint2(prog)
                    num_red2 += len(generator.get_ground_rules((None, con)))
                    new_cons.add(con)
                    new_handles, con = generator.redundancy_constraint3(prog)
                    all_handles.update(parse_handles(generator, new_handles))
                    bad_progs.add(con)


                all_cons.update(new_cons)

                constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model, all_nogoods)



def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    print('SETTINGS.NOGOODS',settings.nogoods)
    return settings.solution, settings.best_prog_score, settings.stats
