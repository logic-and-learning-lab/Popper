import time
import numbers
from itertools import permutations
from itertools import chain, combinations
# from chain import from_iterable
from collections import deque
from . explain import get_raw_prog as get_raw_prog2
from . combine import Combiner
from . explain import Explainer, head_connected, get_raw_prog, seen_more_general_unsat, prog_hash, has_valid_directions, order_body, connected

# from . explain2 import find_most_gen_unsat
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, prog_has_invention, order_rule, prog_size, format_literal, theory_subsumes, rule_subsumes, format_prog, format_prog2, order_rule2, Constraint
from . core import Literal
from . tester import Tester
from . generate import Generator, Grounder, parse_model, atom_to_symbol, arg_to_symbol
from . bkcons import deduce_bk_cons, deduce_recalls, get_bkcons
from . variants import find_variants

AGGRESSIVE = True
AGGRESSIVE = False

SHOW_PRUNED = True
# SHOW_PRUNED = False

WITH_OPTIMISATIONS = True
# WITH_OPTIMISATIONS = False

WITH_MOST_GEN_OPTIMISATIONS = True
# WITH_MOST_GEN_OPTIMISATIONS = False

pruned = set()
pruned2 = set()


def explain_incomplete(settings, explainer, tester, prog, directions):
    unsat_cores = list(explainer.explain_totally_incomplete(prog, directions))

    out_cons = []
    for subprog, unsat_body in unsat_cores:
        pruned_subprog = True

        if SHOW_PRUNED:
            for i, rule in enumerate(order_prog(subprog)):
                print('\t', format_rule(order_rule(rule)), '\t', f'unsat{i}')

        if unsat_body:
            _, body = list(subprog)[0]
            out_cons.append((Constraint.UNSAT, body, None))
            continue

        if not (settings.recursion_enabled or settings.pi_enabled):
            out_cons.append((Constraint.SPECIALISATION, subprog, None))
            continue

        if len(subprog) == 1:
            out_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, subprog, None))

        out_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, subprog, None))

    return out_cons

def explain_inconsistent(settings, tester, prog):
    out_cons = []

    if len(prog) == 1 or not settings.recursion_enabled:
        return out_cons

    base = []
    rec = []
    for rule in prog:
        if rule_is_recursive(rule):
            rec.append(rule)
        else:
            base.append(rule)

    pruned_subprog = False
    for rule in base:
        subprog = frozenset([rule])
        if tester.is_inconsistent(subprog):
            out_cons.append((Constraint.GENERALISATION, subprog, None))
            pruned_subprog = True

    if pruned_subprog:
        return out_cons

    if len(rec) == 1:
        return out_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_inconsistent(subprog):
                out_cons.append((Constraint.GENERALISATION, subprog, None))
                pruned_subprog = True

    return out_cons

# seen_covers_any = set()
# seen_get_neg_covered = set()
def check_redundant_literal2(prog, tester, settings):
    if len(prog) > 1:
        return False

    rule = list(prog)[0]
    head, body = rule

    if len(body) == 1:
        return False

    body = tuple(body)

    head_vars = set(head.arguments)
    rule_vars = set()
    rule_vars.update(head_vars)

    for lit in body:
        rule_vars.update(lit.arguments)

    fetched_thing = False

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_rule = (head, frozenset(new_body))

        new_vars = set()
        new_body_vars = set()

        for lit in new_body:
            new_body_vars.update(lit.arguments)

        new_vars.update(head_vars)
        new_vars.update(new_body_vars)

        if not head_connected(new_rule):
            continue

        if not new_vars.issubset(rule_vars):
            continue

        if not head_vars.issubset(new_body_vars):
            continue

        # if len(new_body) < 2:
            # continue

        var_count = {x:1 for x in head_vars}

        for lit in new_body:
            for x in lit.arguments:
                if x in var_count:
                    var_count[x] += 1
                else:
                    var_count[x] = 1

        if any(v == 1 for v in var_count.values()):
            continue

        if not has_valid_directions(new_rule):
            continue


        if not fetched_thing:
            fetched_thing = True
            neg_covered = tester.get_neg_covered2(prog)

        new_prog = [new_rule]

        uncovered = [x for x in tester.neg_index if x not in neg_covered]
        tmp = tester.covers_any3(new_prog, uncovered)

        if not tmp:
            print('has redundant literal')
            for rule in prog:
                print(format_rule(rule))
            print('\t\tredundant',format_literal(body[i]))
            return True
    return False

seen_shit_subprog = set()

def find_most_general_shit_subrule(prog, tester, settings, min_coverage, d=0, seen_poo=set(), seen_ok={}, all_seen_crap=set()):
    rule = list(prog)[0]
    head, body = rule

    if len(body) == 0:
        return []

    body = tuple(body)

    pruned_subprog = None

    head_vars = set(head.arguments)
    rule_vars = set()
    rule_vars.update(head_vars)


    out_progs = []

    for lit in body:
        rule_vars.update(lit.arguments)

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_rule = (head, new_body)
        subprog = frozenset([(head, frozenset(new_body))])
        raw_subprog = get_raw_prog2(subprog)

        if len(new_body) == 0:
            continue

        k = prog_hash(subprog)

        if k in seen_shit_subprog:
            continue
        seen_shit_subprog.add(k)


        if not head_connected(new_rule):
            continue

        if not has_valid_directions(new_rule):
            continue

        if has_messed_up_vars([new_rule]):
            new_rule2 = functional_rename_vars(new_rule)
            k2 = prog_hash([new_rule2])
            if k2 in seen_shit_subprog:
                seen_shit_subprog.add(k)
                continue

        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned for x in non_empty_powerset(new_body)):
            continue

        head2, body2 = functional_rename_vars((head, new_body))
        if any(frozenset((y.predicate, y.arguments) for y in z) in pruned for z in non_empty_powerset(body2)):
            continue

        # if any(frozenset((y.predicate, y.arguments) for y in z) in pruned2 for z in non_empty_powerset(new_body)):
        #     print('PRUNED_H', format_prog2(subprog))

        # if any(frozenset((y.predicate, y.arguments) for y in z) in pruned2 for z in non_empty_powerset(body2)):
        #     print('PRUNED_I', format_prog2(subprog))

        if tester.has_redundant_literal(subprog):
            xs = find_most_general_shit_subrule(subprog, tester, settings, min_coverage, d+1, seen_poo, seen_ok, all_seen_crap)
            out_progs.extend(xs)
            continue

        t1 = time.time()
        pos_covered = tester.get_pos_covered(subprog)

        if len(pos_covered) <= min_coverage:
            seen_poo.add(raw_subprog)
            xs = find_most_general_shit_subrule(subprog, tester, settings, min_coverage,  d+1, seen_poo, seen_ok, all_seen_crap)
            if len(xs) != 0:
                out_progs.extend(xs)
            else:
                out_progs.append(subprog)
                with settings.stats.duration('variants'):
                    for _, x in find_variants(new_rule, settings.max_vars):
                        pruned2.add(x)
        else:
            seen_ok[raw_subprog] = min_coverage
    return out_progs


def get_min_pos_coverage(best_prog, cached_pos_covered):
    if prog_is_recursive(best_prog):
        return 1
    return min(len(cached_pos_covered[x]) for x in best_prog if x in cached_pos_covered)

def functional_rename_vars(rule):
    head, body = rule
    seen_args = set()
    for body_literal in body:
        seen_args.update(body_literal.arguments)

    if head:
        head_vars = set(head.arguments)
    else:
        head_vars = set()
    next_var = len(head_vars)
    new_body = []
    lookup = {}

    new_body = set()
    for body_literal in sorted(body, key=lambda x: x.predicate):
        new_args = []
        for var in body_literal.arguments:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = chr(ord('A') + next_var)
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(body_literal.predicate, tuple(new_args), body_literal.directions)
        new_body.add(new_atom)

    return head, frozenset(new_body)

def has_messed_up_vars(prog):
    for head, body in prog:
        seen_args = set()
        seen_args.update(head.arguments)
        for body_literal in body:
            seen_args.update(body_literal.arguments)
        # print(seen_args, any(chr(ord('A') + i) not in seen_args for i in range(len(seen_args))))
        if any(chr(ord('A') + i) not in seen_args for i in range(len(seen_args))):
            return True
    return False


def prune_smaller_backtrack4(min_coverage, cached_pos_covered, could_prune_later, settings, tester):

    if not AGGRESSIVE:
        min_coverage = 1

    to_dump = set()
    to_prune = set()

    zs = sorted(could_prune_later.items(), key=lambda x: len(list(x[0])[0][1]), reverse=False)
    for prog2, pos_covered2 in zs:

        if len(pos_covered2) >= min_coverage:
            continue

        to_dump.add(prog2)

        head, body = list(prog2)[0]

        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(body)):
            continue

        skip = False

        for x in non_empty_powerset(body):
            head2, body2 = functional_rename_vars((head, x))
            if frozenset((y.predicate, y.arguments) for y in body2) in pruned2:
                skip = True
                break
            # continue

        if skip:
            continue

        head2, body2 = functional_rename_vars((head, body))
        for z in non_empty_powerset(body2):
            moo = frozenset((y.predicate, y.arguments) for y in z)
            if moo not in pruned2:
                continue


            asda = set()
            renamed_ok = False
            for x in non_empty_powerset(body):
                asda.add(frozenset((y.predicate, y.arguments) for y in x))
                head3, body3 = functional_rename_vars((head, x))
                p1 = frozenset((y.predicate, y.arguments) for y in body3)
                if p1 in pruned2:
                    renamed_ok = True


            print('PISS1', sorted(moo))
            print(format_prog2(prog2))
            print('moo in asda', moo in asda)
            print('renamed ok', renamed_ok)
            # for p in asda:
                # print('p', sorted(p))
            print('\t',format_rule(order_rule2((head, body))))
            for k in non_empty_powerset(body):
                if frozenset((y.predicate, y.arguments) for y in k) in pruned2:
                    print('SHIT MY PANTS')
            print('\t',format_rule(order_rule2((head2, body2))))
            xs = find_variants((head, body), settings.max_vars)
            ys = find_variants((head2, body2), settings.max_vars)
            print(set(xs) == set(ys))
            if set(xs) != set(ys):
                for x in xs:
                    if x not in ys:
                        print('x', x)
                for y in ys:
                    if y not in xs:
                        print('y', y)
            assert(False)

        for z in non_empty_powerset(body2):
            moo = frozenset((y.predicate, y.arguments) for y in z)
            if moo not in pruned2:
                continue
            print('PISS2', moo)
            print('\t',format_rule(order_rule2((head, body))))
            print('\t',format_rule(order_rule2((head2, body2))))
            xs = find_variants((head, body), settings.max_vars)
            ys = find_variants((head2, body2), settings.max_vars)
            print(set(xs) == set(ys))
            if set(xs) != set(ys):
                for x in xs:
                    if x not in ys:
                        print('x', x)
                for y in ys:
                    if y not in xs:
                        print('y', y)
            assert(False)

        z = frozenset((y.predicate, y.arguments) for y in body)
        pruned.add(z)

        with settings.stats.duration('variants'):
            for _, x in find_variants((head, body), settings.max_vars):
                pruned2.add(x)

        # with settings.stats.duration('most gen2'):

        if WITH_MOST_GEN_OPTIMISATIONS and min_coverage > 1:
            with settings.stats.duration('find most gen incomplete'):
                pruned_smaller = find_most_general_shit_subrule(prog2, tester, settings, min_coverage)
                pruned.update(pruned_smaller)
                # TODO1!!!!!
                # assert(False)
        else:
            pruned_smaller = set()

        if len(pruned_smaller) == 0:
            to_prune.add(prog2)
            if SHOW_PRUNED:
                print('\t', format_prog2(prog2), 'smaller_1', len(pos_covered2))
        else:
            for pruned_pruned_smaller_prog in pruned_smaller:
                to_prune.add(pruned_pruned_smaller_prog)

                if SHOW_PRUNED:
                    print('\t', format_prog2(pruned_pruned_smaller_prog), '\t', 'smaller_2', len(pos_covered2))

                head, body = list(pruned_pruned_smaller_prog)[0]
                z = frozenset((y.predicate, y.arguments) for y in body)
                pruned.add(z)
                with settings.stats.duration('variants'):
                    for _, x in find_variants((head, body), settings.max_vars):
                        print('\t\t\tMOO3', sorted(x))
                        pruned2.add(x)

    for x in to_dump:
        del could_prune_later[x]

    return to_prune


def prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester):
    to_prune = set()
    to_delete = set()
    seen = set()

    zs = sorted(could_prune_later.items(), key=lambda x: len(list(x[0])[0][1]), reverse=False)
    for prog2, pos_covered2 in zs:

        if not pos_covered2.issubset(pos_covered):
            continue

        head, body = list(prog2)[0]

        if body in seen:
            continue
        seen.add(body)

        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(body)):
            to_delete.add(prog2)
            continue

        pruned_subprog = False

        for new_body in non_empty_powerset(body):

            if len(new_body) == 0:
                continue

            if len(new_body) == len(body):
                continue

            new_rule = (head, new_body)

            if not head_connected(new_rule):
                continue

            if not has_valid_directions(new_rule):
                continue

            tmp = frozenset((y.predicate, y.arguments) for y in new_body)
            if tmp in seen:
                continue
            seen.add(tmp)

            new_prog = frozenset([new_rule])

            skip = True
            for z in non_empty_powerset(new_body):
                asda = frozenset((y.predicate, y.arguments) for y in z)
                if asda in pruned2:
                    skip = True
                    pruned_subprog = True
                    break
            if skip:
                continue

            head2, body2 = functional_rename_vars(new_rule)
            if any(frozenset((y.predicate, y.arguments) for y in z) in pruned for z in non_empty_powerset(body2)):
                assert(False)
                continue

            for z in non_empty_powerset(body2):
                asda = frozenset((y.predicate, y.arguments) for y in z)
                if asda in pruned2:
                    print('PRUNED_A', format_prog2(new_prog), sorted(asda))
                assert(False)

            if tester.has_redundant_literal(new_prog):
                print('SUBSUMED BACKTRACK SKIP')
                assert(False)
                # continue

            sub_prog_pos_covered = tester.get_pos_covered(new_prog)
            if sub_prog_pos_covered == pos_covered2:
                if SHOW_PRUNED:
                    print('\t', format_prog2(new_prog), '\t', 'subsumed_2')
                    pass
                to_prune.add(new_prog)
                pruned_subprog = True
                with settings.stats.duration('variants'):
                    for _, x in find_variants(new_rule):
                        pruned2.add(x)

        to_delete.add(prog2)

        if pruned_subprog == False:
            with settings.stats.duration('variants'):
                for _, x in find_variants((head, body), settings.max_vars):
                    pruned2.add(x)
            if SHOW_PRUNED:
                print('\t', format_prog2(prog2), '\t', 'subsumed_1')
                pass
            to_prune.add(prog2)

    for x in to_delete:
        del could_prune_later[x]

    return to_prune

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def find_most_general_subsumed(prog, tester, success_sets, settings, seen=set()):
    head, body = list(prog)[0]
    body = list(body)

    if len(body) == 0:
        return []

    out = set()
    head_vars = set(head.arguments)

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_body = frozenset(new_body)

        if len(new_body) == 0:
            # print('\t'*2, 'too small2')
            continue

        tmp1 = frozenset((y.predicate, y.arguments) for y in new_body)

        if tmp1 in seen:
            continue
        seen.add(tmp1)

        new_rule = (head, new_body)
        new_prog = frozenset({new_rule})

        if not any(x in head_vars for literal in new_body for x in literal.arguments):
            continue

        skip = False
        for x in non_empty_powerset(new_body):
            tmp2 = frozenset((y.predicate, y.arguments) for y in x)
            if tmp1 == tmp2:
                continue
            if tmp2 in pruned2:
                skip = True
                break
        if skip:
            continue

        if not head_connected(new_rule):
            xs = find_most_general_subsumed(new_prog, tester, success_sets, settings, seen)
            out.update(xs)
            continue

        if not has_valid_directions(new_rule):
            xs = find_most_general_subsumed(new_prog, tester, success_sets, settings)
            out.update(xs)
            continue

        if tester.has_redundant_literal(new_prog):
            xs = find_most_general_subsumed(new_prog, tester, success_sets, settings)
            out.update(xs)
            continue
            # print('tester.has_redundant_literal',format_prog(new_prog))
            # assert(False)

        sub_prog_pos_covered = tester.get_pos_covered(new_prog, ignore=True)
        subsumed = sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)

        if not subsumed:
            continue

        xs = find_most_general_subsumed(new_prog, tester, success_sets, settings, seen)
        if len(xs) > 0:
            out.update(xs)
            continue

        for _, x in find_variants(new_rule, settings.max_vars):
            pruned2.add(x)

        out.add(new_prog)
    return out




def check_whether_ok(prog, tester, success_sets, settings,  min_coverage, check_coverage=False, check_subsumed=False, seen=set()):

    assert(check_coverage or check_subsumed)

    head, body = list(prog)[0]
    body = list(body)

    if len(body) == 0:
        return []

    out = set()
    head_vars = set(head.arguments)

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_body = frozenset(new_body)

        if len(new_body) == 0:
            continue

        tmp1 = frozenset((y.predicate, y.arguments) for y in new_body)

        if tmp1 in seen:
            continue
        seen.add(tmp1)

        new_rule = (head, new_body)
        new_prog = frozenset({new_rule})

        if not any(x in head_vars for literal in new_body for x in literal.arguments):
            continue

        skip = False
        for x in non_empty_powerset(new_body):
            tmp2 = frozenset((y.predicate, y.arguments) for y in x)
            if tmp1 == tmp2:
                continue
            if tmp2 in pruned2:
                skip = True
                break
        if skip:
            continue

        if not head_connected(new_rule):
            xs = check_whether_ok(new_prog, tester, success_sets, settings, min_coverage, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        if not has_valid_directions(new_rule):
            xs = check_whether_ok(new_prog, tester, success_sets, settings, min_coverage, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        if tester.has_redundant_literal(new_prog):
            xs = check_whether_ok(new_prog, tester, success_sets, settings, min_coverage, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        sub_prog_pos_covered = tester.get_pos_covered(new_prog, ignore=True)
        subsumed = sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)

        prune = check_subsumed and subsumed
        prune = prune or check_coverage and len(sub_prog_pos_covered) <= min_coverage

        if not prune:
            continue

        xs = check_whether_ok(new_prog, tester, success_sets, settings, min_coverage, check_coverage, check_subsumed, seen)
        if len(xs) > 0:
            out.update(xs)
            continue

        for _, x in find_variants(new_rule, settings.max_vars):
            pruned2.add(x)

        out.add(new_prog)
    return out


def popper(settings):
    with settings.stats.duration('load data'):
        tester = Tester(settings)

    explainer = Explainer(settings, tester)
    grounder = Grounder(settings)
    combiner = Combiner(settings, tester)

    num_pos = len(settings.pos_index)

    # track the success sets of tested hypotheses
    success_sets = {}
    rec_success_sets = {}
    last_size = None

    bkcons = []
    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            bkcons = get_bkcons(settings, tester)

    # generator that builds programs
    with settings.stats.duration('init'):
        generator = Generator(settings, grounder, bkcons)

    cached_pos_covered = {}
    could_prune_later = {}

    # count_check_redundant_literal2 = 0
    max_size = (1 + settings.max_body) * settings.max_rules

    for size in range(1, max_size+1):
        if size > settings.max_literals:
            break

        # code is odd/crap:
        # if there is no PI or recursion, we only add nogoods
        # otherwise we build constraints and add them as nogoods and then again as constraints to the solver
        if not settings.single_solve:
            settings.logger.info(f'SIZE: {size} MAX_SIZE: {settings.max_literals}')
            with settings.stats.duration('init'):
                generator.update_solver(size)

        handle = iter(generator.solver.solve(yield_ = True))

        while True:
            pruned_sub_incomplete = False
            pruned_sub_inconsistent = False
            pruned_more_general_shit = False
            add_spec = False
            add_gen = False
            add_redund1 = False
            add_redund2 = False

            tmp_new_cons = []

            # GENERATE A PROGRAM
            with settings.stats.duration('generate_clingo'):
            # with settings.stats.duration('generate'):
                # get the next model from the solver
                model = next(handle, None)
                if model is None:
                    break

            with settings.stats.duration('generate_parse'):
                atoms = model.symbols(shown = True)
                prog, rule_ordering, directions = parse_model(atoms)

            settings.stats.total_programs += 1

            if settings.debug:
                settings.logger.debug(f'Program {settings.stats.total_programs}:')
                settings.logger.debug(format_prog(prog))

            # messy way to track program size
            if settings.single_solve:
                k = prog_size(prog)
                if last_size == None or k != last_size:
                    last_size = k
                    settings.logger.info(f'Searching programs of size: {k}')
                if last_size > settings.max_literals:
                    return

            is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
            has_invention = settings.pi_enabled and prog_has_invention(prog)

            # TEST A PROGRAM
            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            num_pos_covered = len(pos_covered)

            # TODO: GENERALISE TO RECURSION + PI
            if len(prog) == 1:
                cached_pos_covered[list(prog)[0]] = pos_covered

            if not has_invention:
                explainer.add_seen(prog)
                if num_pos_covered == 0:
                    # if the programs does not cover any positive examples, check whether it is has an unsat core
                    with settings.stats.duration('find mucs'):
                        cons_ = explain_incomplete(settings, explainer, tester, prog, directions)
                        tmp_new_cons.extend(cons_)
                        pruned_sub_incomplete = len(cons_) > 0

            # check whether subsumed by a seen program
            subsumed = False
            if not is_recursive and num_pos_covered > 0:
                subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
                if subsumed:
                    add_spec = True
                if not has_invention and WITH_OPTIMISATIONS and WITH_MOST_GEN_OPTIMISATIONS:
                    covers_too_few = False
                    min_coverage = None
                    if combiner.solution_found:
                        min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                        if not AGGRESSIVE:
                            min_coverage = 2
                        covers_too_few = num_pos_covered < min_coverage
                        if covers_too_few:
                            add_spec = True
                    if subsumed or covers_too_few:
                        xs = check_whether_ok(prog, tester, success_sets, settings,  min_coverage, check_coverage=covers_too_few, check_subsumed=subsumed)
                        if len(xs) > 0:
                            pruned_more_general_shit = True
                        for x in xs:
                            if SHOW_PRUNED:
                                if subsumed and not covers_too_few:
                                    print('\t', format_prog2(x), '\t', 'subsumed_gen')
                                elif not subsumed and covers_too_few:
                                    print('\t', format_prog2(x), '\t', 'covers_too_few_gen', len(pos_covered))
                                else:
                                    print('\t', format_prog2(x), '\t', 'not ok', len(pos_covered))
                            tmp_new_cons.append((Constraint.SPECIALISATION, x, None))


                # elif combiner.solution_found and not is_recursive and not has_invention and WITH_OPTIMISATIONS:
                #     min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                #     if not AGGRESSIVE:
                #         min_coverage = 2
                #     # if we have a solution, any better solution must cover at least two examples
                #     if len(pos_covered) < min_coverage:
                #         add_spec = True
                #         if WITH_MOST_GEN_OPTIMISATIONS:
                #             with settings.stats.duration('find most gen incomplete'):
                #                 more_general_shit_progs = find_most_general_shit_subrule(prog, tester, settings, min_coverage)
                #                 if len(more_general_shit_progs):
                #                     pruned_more_general_shit = True
                #                 for x in more_general_shit_progs:
                #                     if SHOW_PRUNED:
                #                         print('\t', format_prog2(x), '\t', 'pruned_more_general_shit', len(pos_covered))
                #                     tmp_new_cons.append((Constraint.SPECIALISATION, x, None))

            if inconsistent:
                # if inconsistent, prune generalisations
                add_gen = True
                if is_recursive:
                    combiner.add_inconsistent(prog)
                    with settings.stats.duration('find sub inconsistent'):
                        cons_ = explain_inconsistent(settings, tester, prog)
                        tmp_new_cons.extend(cons_)
            else:
                # if consistent, prune specialisations
                add_spec = True

            # if consistent and partially complete, test whether functional
            if not inconsistent and settings.functional_test and num_pos_covered > 0 and tester.is_non_functional(prog) and not pruned_more_general_shit:
                # if not functional, rule out generalisations and set as inconsistent
                add_gen = True
                # v.important: do not prune specialisations!
                add_spec = False
                inconsistent = True

            # if it does not cover any example, prune specialisations
            if num_pos_covered == 0:
                add_spec = True
                # if recursion and no PI, apply redundancy constraints
                if settings.recursion_enabled:
                    add_redund2 = True
                    if len(prog) == 1 and not settings.pi_enabled:
                        add_redund1 = True

            # remove generalisations of programs with redundant literals
            if is_recursive:
                with settings.stats.duration('has_redundant_literal'):
                    for rule in prog:
                        if tester.has_redundant_literal([rule]):
                            print('has_redundant_literal')
                            print('\t',format_rule(rule))
                            add_gen = True
                            tmp_new_cons.append((Constraint.GENERALISATION,[rule], None))

            # remove a subset of theta-subsumed rules when learning recursive programs with more than two rules
            if settings.max_rules > 2 and is_recursive:
                tmp_new_cons.append((Constraint.TMP_ANDY, prog, rule_ordering))

            # remove generalisations of programs with redundant rules
            if is_recursive and len(prog) > 2 and tester.has_redundant_rule(prog):
                with settings.stats.duration('has_redundant_rule'):
                    add_gen = True
                    print('has_redundant_rule')
                    for rule in order_prog(prog):
                        print('\t',format_rule(order_rule(rule)))
                    r1, r2 = tester.find_redundant_rules(prog)
                    print('\t','r1',format_rule(order_rule(r1)))
                    print('\t','r2',format_rule(order_rule(r2)))
                    tmp_new_cons.append((Constraint.GENERALISATION, [r1,r2], None))

            # check whether subsumed by a seen program
            # subsumed = False

            # # WHY DO WE HAVE A RECURSIVE CHECK???
            # if num_pos_covered > 0 and not is_recursive:
            #     with settings.stats.duration('check_subsumed'):
            #         subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
            #         # if so, prune specialisations
            #         if subsumed:
            #             add_spec = True
            #             if not is_recursive and not has_invention and WITH_MOST_GEN_OPTIMISATIONS:
            #                 xs = find_most_general_subsumed(prog, tester, success_sets, settings)
            #                 for x in xs:
            #                     pruned_more_general_shit = True
            #                     if SHOW_PRUNED:
            #                         print('\t', format_prog2(x), '\t', 'subsumed_0')
            #                         pass
            #                     tmp_new_cons.append((Constraint.SPECIALISATION, x, None))

            # if not add_spec and False:
            # # if not add_spec:
            #     assert(pruned_sub_incomplete == False)
            #     with settings.stats.duration('check_redundant_literal2'):
            #         # TODO: CHECK WHETHER THE PROGRAM CONTAINS A REDUNDANT LITERAL
            #         # with settings.stats.duration('reduce consistent'):
            #         #     # print(format_rule(list(prog)[0]))
            #         #     check_can_be_reduced(prog, tester, settings, generator, new_cons)
            #         if check_redundant_literal2(prog, tester, settings):
            #             add_spec = True
            #             add_gen = True
            #             add_redund1 = True
            #             add_redund2 = True
            #             count_check_redundant_literal2 +=1
            #             print('count_check_redundant_literal2',count_check_redundant_literal2)
            #             for rule in order_prog(prog):
            #                 print('\t',format_rule(order_rule(rule)))
            #             # if not add_gen:
            #             #     print('count_check_redundant_literal2',count_check_redundant_literal2)
            #             #     for rule in prog:
            #             #         print('\t',format_rule(rule))

            if not add_spec and not pruned_more_general_shit and not pruned_sub_incomplete:
                assert(pruned_sub_incomplete == False)
                could_prune_later[prog] = pos_covered

            seen_better_rec = False
            # with settings.stats.duration('seen_better_rec'):
            if is_recursive and not inconsistent and not subsumed and not add_gen and num_pos_covered > 0:
                seen_better_rec = pos_covered in rec_success_sets or any(pos_covered.issubset(xs) for xs in rec_success_sets)

            if not add_spec and seen_better_rec:
                assert(False)

            # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
            if not inconsistent and not subsumed and not add_gen and num_pos_covered > 0 and not seen_better_rec and not pruned_more_general_shit:

                assert(pruned_sub_incomplete == False)
                assert(pruned_more_general_shit == False)

                # update success sets
                success_sets[pos_covered] = prog
                if is_recursive:
                    rec_success_sets[pos_covered] = prog

                # COMBINE
                with settings.stats.duration('combine'):
                    new_solution_found = combiner.update_best_prog(prog, pos_covered)

                # if we find a new solution, update the maximum program size
                # if only adding nogoods, eliminate larger programs
                if new_solution_found:

                    # if non-separable program covers all examples, stop
                    if not inconsistent and num_pos_covered == num_pos:
                        return

                    settings.max_literals = combiner.max_size-1
                    if size >= settings.max_literals:
                        return

                    if not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                        min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                        with settings.stats.duration('prune smaller backtrack'):
                            xs = prune_smaller_backtrack4(min_coverage, cached_pos_covered, could_prune_later, settings, tester)
                            for x in xs:
                                tmp_new_cons.append((Constraint.SPECIALISATION, x, None))

                    if settings.single_solve:
                        # AC: sometimes adding these size constraints can take longer
                        for i in range(combiner.max_size, max_size+1):
                            size_con = [(atom_to_symbol("size", (i,)), True)]
                            model.context.add_nogood(size_con)

                if not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                    with settings.stats.duration('prune subsumed backtrack'):
                        xs = prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester)
                        for x in xs:
                            tmp_new_cons.append((Constraint.SPECIALISATION, x, None))

            # BUILD CONSTRAINTS
            if add_spec and not pruned_sub_incomplete and not pruned_more_general_shit and not add_redund2:
                tmp_new_cons.append((Constraint.SPECIALISATION, prog, rule_ordering))

            if add_gen and not pruned_sub_inconsistent:
                if settings.recursion_enabled or settings.pi_enabled:
                    if not pruned_sub_incomplete:
                        tmp_new_cons.append((Constraint.GENERALISATION, prog, rule_ordering))
                else:
                    if not add_spec:
                        tmp_new_cons.append((Constraint.GENERALISATION, prog, rule_ordering))

            if add_redund1 and not pruned_sub_incomplete:
                tmp_new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog, rule_ordering))

            if add_redund2 and not pruned_sub_incomplete:
                tmp_new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog, rule_ordering))

            # CONSTRAIN
            with settings.stats.duration('constrain'):
                generator.constrain(tmp_new_cons, model)

        # if not pi_or_rec:
        if settings.single_solve:
            break

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
