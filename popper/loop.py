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
        head, body = rule
        # TODO: add caching
        for h, b in generator.get_ground_rules(rule):
            _, p, args = h
            out_h = (p, args)
            out_b = frozenset((b_pred, b_args) for _, b_pred, b_args in b)
            yield (x, (out_h, out_b))

def explain_failure(settings, generator, explainer, prog, directions, pos_covered, new_cons, all_handles, bad_handles):
    pruned_subprog = False
    explainer.add_seen_prog(prog)

    if len(pos_covered) > 0:
        return

    for subprog in explainer.explain_totally_incomplete2(prog, directions, settings.stats.total_programs):
        pruned_subprog = True
        new_rule_handles, con = generator.build_specialisation_constraint(subprog)
        new_cons.add(con)
        all_handles.update(parse_handles(generator, new_rule_handles))

        # if not settings.recursion_enabled or settings.pi_enabled:
        #     continue

        if len(subprog) == 1:
            bad_handle, new_rule_handles, con = generator.redundancy_constraint1(subprog)
            bad_handles.add(bad_handle)
            new_cons.add(con)
            all_handles.update(parse_handles(generator, new_rule_handles))
        # else:
        handles, cons = generator.redundancy_constraint2(prog)
        new_cons.update(cons)
        # new_cons.add(generator.redundancy_constraint2(subprog))
        # new_handles, con = generator.redundancy_constraint3(prog)
        all_handles.update(parse_handles(generator, handles))

    return pruned_subprog

def constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model):
    with settings.stats.duration('constrain'):
        ground_bodies = set()
        for con in new_cons:

            ground_rules = generator.get_ground_rules((None, con))
            for ground_rule in ground_rules:
                _ground_head, ground_body = ground_rule
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
            model.context.add_nogood(nogood)

def popper(settings):
    if settings.bkcons:
        deduce_bk_cons(settings)

    tester = Tester(settings)
    explainer = Explainer(settings, tester)
    grounder = Grounder()
    combiner = Combiner(settings, tester)

    pi_or_rec = settings.recursion_enabled or settings.pi_enabled

    num_pos = settings.pos

    # track the success sets of tested hypotheses
    success_sets = {}
    last_size = None

    # caching
    cached_clingo_atoms = {}

    # for micro-optimisations
    seen_covers_only_one_gen = set()
    seen_covers_only_one_spec = set()
    seen_incomplete_gen = set()
    seen_incomplete_spec = set()

    # constraints generated
    all_ground_cons = set()
    # rule handles, such as seen(id):- head_literal(...), body_literal(...)
    all_handles = set()
    # handles for rules that are minimal and unsatisfiable
    bad_handles = set()

    # generator that builds programs
    generator = Generator(settings, grounder)

    max_size = (1 + settings.max_body) * settings.max_rules

    for size in range(1, max_size+1):
        if size > settings.max_literals:
            break

        # code is odd/crap:
        # if there is no PI or recursion, we only add nogoods
        # otherwise we build constraints and add them as nogoods and then again as constraints to the solver
        if pi_or_rec:
            settings.logger.info(f'SIZE: {size} MAX_SIZE: {settings.max_literals}')
            generator.update_number_of_literals(size)

            with settings.stats.duration('init'):
                generator.update_solver(size, all_handles, bad_handles, all_ground_cons)

        all_ground_cons = set()
        all_handles = set()
        bad_handles = set()

        with generator.solver.solve(yield_ = True) as handle:
            handle = iter(handle)

            while True:
                new_cons = set()
                new_rule_handles = set()
                pruned_subprog = False

                # GENERATE A PROGRAM
                with settings.stats.duration('generate'):
                    # get the next model from the solver
                    model = next(handle, None)
                    if model is None:
                        break
                    atoms = model.symbols(shown = True)
                    prog, rule_ordering, directions = parse_model(atoms)

                settings.stats.total_programs += 1
                if settings.debug:
                    settings.logger.debug(f'Program {settings.stats.total_programs}:')
                    for rule in order_prog(prog):
                        settings.logger.debug(format_rule(order_rule(rule)))

                # TEST A PROGRAM
                with settings.stats.duration('test'):
                    pos_covered, inconsistent = tester.test_prog(prog)
                    num_pos_covered = len(pos_covered)

                # EXPLAIN A FAILURE
                if settings.explain:
                    with settings.stats.duration('explain'):
                        pruned_subprog = explain_failure(settings, generator, explainer, prog, directions, pos_covered, new_cons, all_handles, bad_handles)

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
                    with settings.stats.duration('subcheck'):
                        if len(prog) > 1:
                            for rule in prog:
                                if rule_is_recursive(rule):
                                    continue
                                subprog = frozenset([rule])
                                # TODO: ADD CACHING BEFORE THE CALL TO TESTER
                                if tester.is_inconsistent(subprog):
                                    handles, con = generator.build_generalisation_constraint(subprog)
                                    new_cons.add(con)
                                    new_rule_handles.update(handles)
                else:
                    # if consistent, prune specialisations
                    add_spec = True

                # if consistent and partially complete test whether functional
                if not inconsistent and settings.functional_test and num_pos_covered > 0 and tester.is_non_functional(prog):
                    # if not functional, rule out generalisations and set as inconsistent
                    add_gen = True
                    # v.important: do not prune specialisations!
                    add_spec = False
                    inconsistent = True

                # if it does not cover any example, prune specialisations
                if num_pos_covered == 0:
                    add_spec = True
                    # if recursion and no PI, apply redundancy constraints
                    # if settings.recursion_enabled and not settings.pi_enabled:
                    # if settings.recursion_enabled and not settings.pi_enabled:
                    if len(prog) == 1:
                        add_redund1 = True
                    add_redund2 = True

                # check whether subsumed by a seen program
                subsumed = False
                if num_pos_covered > 0 and not prog_is_recursive(prog):
                    subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
                    # if so, prune specialisations
                    if subsumed:
                        add_spec = True

                # micro-optimisiations
                if not settings.recursion_enabled:

                    # if we already have a solution, a new rule must cover at least two examples
                    if not add_spec and combiner.solution_found and num_pos_covered == 1:
                        add_spec = True

                    # keep track of programs that only cover one example
                    # once we find a solution, we apply specialisation/generalisation constraints
                    if num_pos_covered == 1:
                        if not add_gen:
                            seen_covers_only_one_gen.add(prog)
                        if not add_spec:
                            seen_covers_only_one_spec.add(prog)

                    # keep track of programs that do not cover all the examples
                    if num_pos_covered != num_pos:
                        if not add_gen:
                            seen_incomplete_gen.add(prog)
                        if not add_spec:
                            seen_incomplete_spec.add(prog)

                    # if we find a solution, prune programs that only cover one example
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
                if not inconsistent and not subsumed and num_pos_covered > 0:
                    # update success sets
                    success_sets[pos_covered] = prog

                    # COMBINE
                    with settings.stats.duration('combine'):
                        new_solution_found = combiner.update_best_prog(prog, pos_covered)

                    # if we find a new solution, update the maximum program size
                    if new_solution_found:

                        # no only adding nogoods, eliminate larger programs
                        if not pi_or_rec:
                            for i in range(combiner.max_size, settings.max_literals+1):
                                size_con = [(atom_to_symbol("size", (i,)), True)]
                                model.context.add_nogood(size_con)

                        settings.max_literals = combiner.max_size-1
                        if size >= settings.max_literals:
                            return

                # if program covers all examples, stop
                if not inconsistent and num_pos_covered == num_pos:
                    return

                if add_spec and not pruned_subprog:
                    handles, con = generator.build_specialisation_constraint(prog, rule_ordering)
                    new_rule_handles.update(handles)
                    new_cons.add(con)

                if add_gen:
                    handles, con = generator.build_generalisation_constraint(prog, rule_ordering)
                    new_rule_handles.update(handles)
                    new_cons.add(con)

                if add_redund1 and not pruned_subprog:
                    bad_handle, handles, con = generator.redundancy_constraint1(prog)
                    bad_handles.add(bad_handle)
                    new_rule_handles.update(handles)
                    new_cons.add(con)

                if add_redund2 and not pruned_subprog:
                    handles, cons = generator.redundancy_constraint2(prog)
                    new_rule_handles.update(handles)
                    new_cons.update(cons)

                    generator.redundancy_constraint4(prog)

                # if pi or rec, we need to save the constraints and handles for the next program size
                if pi_or_rec:
                    all_handles.update(parse_handles(generator, new_rule_handles))

                # Q. For recursive programs, can we skip adding them?

                # CONSTRAIN
                constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model)

        if not pi_or_rec:
            break

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
