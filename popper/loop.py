import time
from . select import Selector
from . util import timeout, format_rule, format_prog, rule_is_recursive, order_prog
from . tester import Tester
# from . asptester import Tester
from . generate import Generator, Grounder
from . bkcons import deduce_bk_cons
from clingo import Function, Number, Tuple_
from . core import Constrainer
import numbers

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

cached = {}
# seen_cons = set()
def f(settings, generator, new_cons, model):
    with settings.stats.duration('constrain.ground'):
        s = set()
        for con in new_cons:
            # assert(con not in seen_cons)
            # seen_cons.add(con)
            for grule in generator.get_ground_rules([(None, con)]):
                h, b = grule
                s.add(b)

    with settings.stats.duration('constrain.transform'):
        nogoods = []
        for b in s:
            tmp = []
            for sign, pred, args in b:
                k = hash((sign, pred, args))
                if k in cached:
                    tmp.append(cached[k])
                else:
                    x = (atom_to_symbol(pred, args), sign)
                    tmp.append(x)
                    cached[k] = x
            nogoods.append(tmp)

    with settings.stats.duration('constrain.add'):
        for tmp in nogoods:
            # pass
            model.context.add_nogood(tmp)

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
    pos = settings.pos

    generator = Generator(settings, grounder, settings.max_literals)

    last_size = None
    seen = set()
    seen_inconsistent = set()
    seen_covers_only_one_gen = set()
    seen_covers_only_one_spec = set()
    seen_incomplete_gen = set()
    seen_incomplete_spec = set()

    inconsistent_count = 0

    with generator.solver.solve(yield_ = True) as handle:
        for model in handle:
            new_cons = set()

            atoms = model.symbols(shown = True)
            prog = generator.parse_model(atoms)

            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            settings.stats.total_programs += 1
            settings.logger.debug(f'Program {settings.stats.total_programs}:')
            for rule in order_prog(prog):
                settings.logger.debug(format_rule(rule))

            k = prog_size(prog)
            if last_size == None or k != last_size:
                last_size = k
                settings.logger.info(f'Searching programs of size: {k}')


            incomplete = len(pos_covered) != len(pos)

            add_spec = False
            add_gen = False

            # if inconsistent, prune generalisations
            if inconsistent:
                # inconsistent_count +=1
                # print('INCONSISTENT', inconsistent_count, settings.stats.total_programs)
                # for rule in order_prog(prog):
                    # print(format_rule(rule))
                add_gen = True
                cons.add_generalisation(prog)
                if len(prog) > 1:
                    for rule in prog:
                        if rule_is_recursive(rule):
                            continue
                        subprog = frozenset([rule])
                        if tester.is_inconsistent(subprog):
                            new_cons.add(generator.build_generalisation_constraint(subprog))

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

            # # TMP!!!!!
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

            if selector.solution_found:
                # TMP MORE PRUNE TMP
                for x in seen_covers_only_one_gen:
                    new_cons.add(generator.build_generalisation_constraint(x))
                seen_covers_only_one_gen = set()
                for x in seen_covers_only_one_spec:
                    new_cons.add(generator.build_specialisation_constraint(x))
                seen_covers_only_one_spec = set()

                if len(selector.best_prog) <= 2:
                    for x in seen_incomplete_gen:
                        new_cons.add(generator.build_generalisation_constraint(x))
                    for x in seen_incomplete_spec:
                        new_cons.add(generator.build_specialisation_constraint(x))
                    seen_incomplete_gen = set()
                    seen_incomplete_spec = set()

            # if consistent, covers at least one example, and is not subsumed, yield candidate program
            if not inconsistent and not subsumed and len(pos_covered) > 0:
                # update coverage
                prog_coverage[prog] = pos_covered
                covered_examples.update(pos_covered)
                success_sets[pos_covered] = prog

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
                return

            if add_spec:
                new_cons.add(generator.build_specialisation_constraint(prog))
            if add_gen:
                new_cons.add(generator.build_generalisation_constraint(prog))
            if not add_spec and not add_gen:
                assert(False)

            f(settings, generator, new_cons, model)

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats