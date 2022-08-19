import time
import numbers
from . combine import Combiner
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive
from . tester import Tester
from . generate import Generator, Grounder
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

cached_clingo_atoms = {}
def constrain(settings, generator, cons, model):
    with settings.stats.duration('constrain'):
        ground_bodies = set()
        for con in cons:
            for ground_rule in generator.get_ground_rules((None, con)):
                _ground_head, ground_body = ground_rule
                ground_bodies.add(ground_body)

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
    grounder = Grounder()
    combiner = Combiner(settings, tester)
    generator = Generator(settings, grounder)
    pos = settings.pos

    success_sets = {}
    last_size = None

    # TMP SETS
    seen_covers_only_one_gen = set()
    seen_covers_only_one_spec = set()
    seen_incomplete_gen = set()
    seen_incomplete_spec = set()

    with generator.solver.solve(yield_ = True) as handle:
        handle = iter(handle)

        while True:
            model = None

            with settings.stats.duration('generate'):
                model = next(handle, None)
                if model is None:
                    break
                atoms = model.symbols(shown = True)
                prog, rule_ordering = generator.parse_model(atoms)

            new_cons = set()

            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            settings.stats.total_programs += 1
            settings.logger.debug(f'Program {settings.stats.total_programs}:')
            for rule in order_prog(prog):
                settings.logger.debug(format_rule(rule))

            if inconsistent and prog_is_recursive(prog):
                combiner.add_inconsistent(prog)

            k = prog_size(prog)
            if last_size == None or k != last_size:
                last_size = k
                settings.logger.info(f'Searching programs of size: {k}')

            add_spec = False
            add_gen = False

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
                            new_cons.add(generator.build_generalisation_constraint(subprog))
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
                add_spec = True

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
                    add_spec = True

                # backtracking idea
                # keep track of programs that only cover one example
                # once we find a solution, we apply specialisation/generalisation constraints
                if len(pos_covered) == 1:
                    if not add_gen:
                        seen_covers_only_one_gen.add(prog)
                    if not add_spec:
                        seen_covers_only_one_spec.add(prog)
                if len(pos_covered) != len(pos):
                    if not add_gen:
                        seen_incomplete_gen.add(prog)
                    if not add_spec:
                        seen_incomplete_spec.add(prog)

                if combiner.solution_found:
                    for x in seen_covers_only_one_gen:
                        new_cons.add(generator.build_generalisation_constraint(x))
                    seen_covers_only_one_gen = set()
                    for x in seen_covers_only_one_spec:
                        new_cons.add(generator.build_specialisation_constraint(x))
                    seen_covers_only_one_spec = set()

                    if len(combiner.best_prog) <= 2:
                        for x in seen_incomplete_gen:
                            new_cons.add(generator.build_generalisation_constraint(x))
                        for x in seen_incomplete_spec:
                            new_cons.add(generator.build_specialisation_constraint(x))
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
                    for i in range(combiner.max_size, settings.max_literals+1):
                        size_con = [(atom_to_symbol("size", (i,)), True)]
                        model.context.add_nogood(size_con)
                    settings.max_literals = combiner.max_size-1

            # if it covers all examples, stop
            if not inconsistent and len(pos_covered) == len(pos):
                return

            if add_spec:
                new_cons.add(generator.build_specialisation_constraint(prog, rule_ordering))
            if add_gen:
                new_cons.add(generator.build_generalisation_constraint(prog, rule_ordering))

            constrain(settings, generator, new_cons, model)

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
