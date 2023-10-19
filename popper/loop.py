import time
import numbers
from itertools import permutations
from itertools import chain, combinations
from collections import deque
from . explain import get_raw_prog as get_raw_prog2
from . combine import Combiner
from . explain import Explainer, head_connected, get_raw_prog, seen_more_general_unsat, prog_hash, has_valid_directions, order_body, connected
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, prog_has_invention, order_rule, calc_prog_size, format_literal, theory_subsumes, rule_subsumes, format_prog, format_prog2, order_rule2, Constraint, bias_order
from . core import Literal
from . tester import Tester
from . generate import Generator, Grounder, parse_model, atom_to_symbol, arg_to_symbol
from . bkcons import deduce_bk_cons, deduce_recalls
from . variants import find_variants

WITH_OPTIMISATIONS = True
# WITH_OPTIMISATIONS = False

pruned2 = set()

# find unsat cores
def explain_incomplete(settings, explainer, tester, prog, directions):
    unsat_cores = list(explainer.explain_totally_incomplete(prog, directions))

    out_cons = []
    for subprog, unsat_body in unsat_cores:
        pruned_subprog = True

        if settings.showcons:
            if len(subprog) > 1:
                print('\n')
            for i, rule in enumerate(order_prog(subprog)):
                print('\t', format_rule(order_rule(rule)), '\t', f'unsat')


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

# given a program with more than one rule, look for inconsistent subrules/subprograms
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


def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def subsumed_or_covers_too_few(prog, tester, success_sets, settings, check_coverage=False, check_subsumed=False, seen=set()):
    head, body = list(prog)[0]
    body = list(body)

    if len(body) == 0:
        return []

    out = set()
    head_vars = set(head.arguments)

    # try the body with one literal removed (the literal at position i)
    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_body = frozenset(new_body)

        if len(new_body) == 0:
            continue

        # check whether we have seen this body before
        k1 = frozenset((y.predicate, y.arguments) for y in new_body)
        if k1 in seen:
            continue
        seen.add(k1)

        new_rule = (head, new_body)
        new_prog = frozenset({new_rule})

        # ensure at least one head variable is in the body
        if not any(x in head_vars for literal in new_body for x in literal.arguments):
            continue

        # check whether we have pruned any subset (HORRIBLE CODE)
        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(new_body)):
            continue

        if not head_connected(new_rule):
            xs = subsumed_or_covers_too_few(new_prog, tester, success_sets, settings, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        if not has_valid_directions(new_rule):
            xs = subsumed_or_covers_too_few(new_prog, tester, success_sets, settings, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        if tester.has_redundant_literal(new_prog):
            xs = subsumed_or_covers_too_few(new_prog, tester, success_sets, settings, check_coverage, check_subsumed, seen)
            out.update(xs)
            continue

        sub_prog_pos_covered = tester.get_pos_covered(new_prog, ignore=True)


        if settings.order_space:
            # this check does not assume that we search by increasing program size
            subsumed = is_subsumed(sub_prog_pos_covered, calc_prog_size(new_prog), success_sets)
        else:
            # this check assumes that we search by increasing program size
            subsumed = sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)

        prune = check_subsumed and subsumed
        prune = prune or (check_coverage and len(sub_prog_pos_covered) == 1)

        if check_coverage and len(sub_prog_pos_covered) == 1 and not subsumed:
            print("POOOOOOOOOO")
            print(format_prog2(new_prog))
            assert(False)

        if not prune:
            continue

        xs = subsumed_or_covers_too_few(new_prog, tester, success_sets, settings, check_coverage, check_subsumed, seen)
        if len(xs) > 0:
            out.update(xs)
            continue

        # for each pruned program, add the variants to the list of pruned programs
        # doing so reduces the number of pointless checks
        for _, x in find_variants(new_rule, settings.max_vars):
            pruned2.add(x)

        out.add(new_prog)
    return out

def explain_none_functional(settings, tester, prog):
    new_cons = []

    if len(prog) == 1:
        return new_cons

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
        if tester.is_non_functional(subprog):
            new_cons.append((Constraint.GENERALISATION, subprog , None))
            pruned_subprog = True

    if pruned_subprog:
        return new_cons

    if len(rec) == 1:
        return new_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_non_functional(subprog):
                new_cons.append((Constraint.GENERALISATION, subprog , None))

    return new_cons

def prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester, prog_size, check_coverage):
    to_prune = set()
    to_delete = set()
    seen = set()

    # Order unpruned programs by size
    zs = sorted(could_prune_later.items(), key=lambda x: len(list(x[0])[0][1]), reverse=False)
    for prog2, pos_covered2 in zs:

        should_prune = check_coverage and len(pos_covered2) == 1 and not settings.order_space

        # TODO: FOR FG
        if settings.order_space:
            subsumed = pos_covered2.issubset(pos_covered)
        else:
            subsumed = pos_covered2.issubset(pos_covered) and calc_prog_size(prog2) >= prog_size

        should_prune = should_prune or subsumed
        if not should_prune:
            continue

        # if check_coverage and len(pos_covered2) == 1 and not pos_covered2.issubset(pos_covered):
            # assert(False)

        head, body = list(prog2)[0]

        if body in seen:
            continue
        seen.add(body)

        # print('X2', format_prog2([(head, body)]))

        # If we have seen a subset of the body then ignore this program
        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(body)):
            # print('PRUNED2')
            # assert(False)
            to_delete.add(prog2)
            continue

        # print('X3', format_prog2([(head, body)]))

        # if any(frozenset((y.predicate, y.arguments) for y in x) in seen for x in non_empty_powerset(body)):
        #     print('MOO')
        #     assert(False)
        #     to_delete.add(prog2)
        #     continue

        pruned_subprog = False

        # We now enumerate the subsets of the body of this role to find the most general subsumed subset
        for new_body in non_empty_powerset(body):

            if len(new_body) == 0:
                continue

            if len(new_body) == len(body):
                continue

            new_rule = (head, new_body)

            # print('X4', format_prog2([new_rule]))

            if not head_connected(new_rule):
                continue

            if not has_valid_directions(new_rule):
                continue

            tmp = frozenset((y.predicate, y.arguments) for y in new_body)
            if tmp in seen:
                # print('prog2', format_prog2(prog2))
                # print('new_rule', format_prog2([new_rule]))
                # print('ASDA PRICE')
                # assert(False)
                continue
            seen.add(tmp)


            # print('X5', format_prog2([new_rule]))

            new_prog = frozenset([new_rule])

            skip = False
            for z in non_empty_powerset(new_body):
                asda = frozenset((y.predicate, y.arguments) for y in z)
                if asda in pruned2:
                    # print('here!!!!!')
                    skip = True
                    pruned_subprog = True
                    break
            if skip:
                # print('skipppy!!!!!')
                continue


            # print('X6', format_prog2([new_rule]))

            head2, body2 = functional_rename_vars(new_rule)
            if any(frozenset((y.predicate, y.arguments) for y in z) in pruned2 for z in non_empty_powerset(body2)):
                # assert(False)
                continue

            for z in non_empty_powerset(body2):
                asda = frozenset((y.predicate, y.arguments) for y in z)
                if asda in pruned2:
                    print('PRUNED_A', format_prog2(new_prog), sorted(asda))
                    assert(False)

            if tester.has_redundant_literal(new_prog):
                # print('SUBSUMED BACKTRACK SKIP')
                continue
                # assert(False)
                # continue

            # print('X7', format_prog2([new_rule]))

            sub_prog_pos_covered = tester.get_pos_covered(new_prog)

            # prune if we have a solution and the subprogram only covers one example
            sub_covers_too_few = check_coverage and len(sub_prog_pos_covered) == 1 and not settings.order_space

            # TODO: FOR FG
            if settings.order_space:
                sub_prog_subsumed = False
            else:
                sub_prog_subsumed = sub_prog_pos_covered == pos_covered2

            if sub_prog_subsumed:
                if settings.showcons:
                    print('\t', format_prog2(new_prog), '\t', 'subsumed_backtrack (generalisation)')


            if not sub_prog_subsumed and sub_covers_too_few:
                # should_prune_sub= True
                if settings.showcons:
                    print('\t', format_prog2(new_prog), '\t', 'COVERS TOO FEW')
                assert(False)

            if sub_prog_subsumed or sub_covers_too_few:
                to_prune.add(new_prog)
                pruned_subprog = True
                with settings.stats.duration('variants'):
                    for _, x in find_variants(new_rule):
                        pruned2.add(x)

        to_delete.add(prog2)

        if pruned_subprog == False:
            with settings.stats.duration('variants'):
                for _, x in find_variants((head, body), settings.max_vars):
                    # print('hello, pruned2', x)
                    pruned2.add(x)
            if settings.showcons:
                print('\t', format_prog2(prog2), '\t', 'subsumed_backtrack')
                # pass
            to_prune.add(prog2)

    for x in to_delete:
        del could_prune_later[x]

    return to_prune

def is_subsumed(pos_covered, prog_size, success_sets):
    subsumed = pos_covered in success_sets and prog_size >= (success_sets[pos_covered])
    subsumed = subsumed or any(pos_covered.issubset(xs) and prog_size >= prog_size2 for xs, prog_size2 in success_sets.items())
    return subsumed

def popper(settings):
    with settings.stats.duration('load data'):
        tester = Tester(settings)

    explainer = Explainer(settings, tester)
    grounder = Grounder(settings)
    combiner = Combiner(settings, tester)

    num_pos = len(settings.pos_index)

    # deduce bk cons
    bkcons = []
    if settings.bkcons or settings.datalog:
        with settings.stats.duration('recalls'):
            bkcons.extend(deduce_recalls(settings))

    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            bkcons.extend(deduce_bk_cons(settings, tester))

    # generator that builds programs
    with settings.stats.duration('init'):
        generator = Generator(settings, grounder, bkcons)

    # track the success sets of tested hypotheses
    success_sets = {}
    rec_success_sets = {}

    # maintain a set of programs that we have not yet pruned
    could_prune_later = {}

    max_size = (1 + settings.max_body) * settings.max_rules
    last_size = None

    search_order = bias_order(settings)

    for (size, n_vars, n_rules, _) in search_order:
        if size > settings.max_literals:
            #break # Here we have to continue the loop given that we might be jumping back and forth over the size
            continue

        # code is odd/crap:
        # if there is no PI or recursion, we only add nogoods
        # otherwise we build constraints and add them as nogoods and then again as constraints to the solver
        if not settings.single_solve:
            if settings.order_space:
                settings.logger.info(f'SIZE: {size} VARS: {n_vars} RULES: {n_rules} MAX_SIZE: {settings.max_literals}')
            else:
                settings.logger.info(f'SIZE: {size} MAX_SIZE: {settings.max_literals}')

            with settings.stats.duration('init'):
                generator.update_solver(size, n_vars, n_rules)

        while True:
            pruned_sub_incomplete = False
            pruned_sub_inconsistent = False
            pruned_more_general = False
            add_spec = False
            add_gen = False
            add_redund1 = False
            add_redund2 = False

            new_cons = []

            # generate a program
            with settings.stats.duration('generate'):
                model = generator.get_model()
                if model is None:
                    break

            with settings.stats.duration('parse'):
                atoms = model.symbols(shown = True)
                prog, rule_ordering, directions = parse_model(atoms)

            prog_size = calc_prog_size(prog)

            settings.stats.total_programs += 1

            if settings.debug:
                settings.logger.debug(f'Program {settings.stats.total_programs}:')
                settings.logger.debug(format_prog(prog))

            # messy way to track program size
            if settings.single_solve:
                if last_size == None or prog_size != last_size:
                    last_size = prog_size
                    if not settings.order_space:
                        settings.logger.info(f'Searching programs of size: {prog_size}')
                if last_size > settings.max_literals and not settings.order_space:
                    return

            is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
            has_invention = settings.pi_enabled and prog_has_invention(prog)

            # test a program
            with settings.stats.duration('test'):
                pos_covered, inconsistent = tester.test_prog(prog)

            num_pos_covered = len(pos_covered)

            if num_pos_covered == 0:
                add_spec = True

            if not has_invention:
                explainer.add_seen(prog)
                if num_pos_covered == 0:
                    # if the programs does not cover any positive examples, check whether it is has an unsat core
                    with settings.stats.duration('find mucs'):
                        cons_ = explain_incomplete(settings, explainer, tester, prog, directions)
                        new_cons.extend(cons_)
                        pruned_sub_incomplete = len(cons_) > 0

            # check whether subsumed by a seen program
            subsumed = False
            if not is_recursive and num_pos_covered > 0:

                # if we do not search by increasing size, we need to use a strict form of subsumption
                if settings.order_space:
                    subsumed = is_subsumed(pos_covered, prog_size, success_sets)
                else:
                    subsumed = pos_covered in success_sets
                    subsumed = subsumed or any(pos_covered.issubset(xs) for xs in success_sets)

                if subsumed:
                    add_spec = True

                if not has_invention and WITH_OPTIMISATIONS:
                    # we check whether a program does not cover enough examples to be useful
                    # if the program only not cover enough examples, we prune it specialisations
                    covers_too_few = combiner.solution_found and not settings.order_space and num_pos_covered == 1
                    if covers_too_few:
                        add_spec = True

                    if subsumed or covers_too_few:
                        # If a program is subsumed or doesn't cover enough examples, we search for the most general subprogram that also is also subsumed or doesn't cover enough examples
                        # only applies to non-recursive and non-PI programs
                        xs = subsumed_or_covers_too_few(prog, tester, success_sets, settings, check_coverage=covers_too_few, check_subsumed=subsumed, seen=set())
                        pruned_more_general = len(xs) > 0
                        if settings.showcons and not pruned_more_general:
                            if subsumed:
                                print('\t', format_prog2(prog), '\t', 'subsumed')
                            else:
                                print('\t', format_prog2(prog), '\t', 'covers_too_few')
                        for x in xs:
                            if settings.showcons:
                                if subsumed:
                                    print('\t', format_prog2(x), '\t', 'subsumed (generalisation)')
                                else:
                                    print('\t', format_prog2(x), '\t', 'covers_too_few (generalisation)', len(pos_covered))
                            new_cons.append((Constraint.SPECIALISATION, x, None))

            if inconsistent:
                # if inconsistent, prune generalisations
                add_gen = True
                if is_recursive:
                    combiner.add_inconsistent(prog)
                    with settings.stats.duration('find sub inconsistent'):
                        cons_ = explain_inconsistent(settings, tester, prog)
                        new_cons.extend(cons_)
            else:
                # if consistent, prune specialisations
                add_spec = True

            # if consistent and partially complete, test whether functional
            if not inconsistent and settings.functional_test and num_pos_covered > 0 and not pruned_more_general:
                if tester.is_non_functional(prog):
                    # if not functional, rule out generalisations and set as inconsistent
                    add_gen = True
                    # V.IMPORTANT: do not prune specialisations!
                    add_spec = False
                    inconsistent = True

                    # check whether any subprograms are non-functional
                    with settings.stats.duration('explain_none_functional'):
                        cons_ = explain_none_functional(settings, tester, prog)
                        if cons_:
                            new_cons.extend(cons_)

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
                            add_gen = True
                            new_cons.append((Constraint.GENERALISATION,[rule], None))
                            if settings.showcons:
                                print('\t', format_rule(rule), '\t', 'has_redundant_literal')

            # remove a subset of theta-subsumed rules when learning recursive programs with more than two rules
            if settings.max_rules > 2 and is_recursive:
                new_cons.append((Constraint.TMP_ANDY, prog, rule_ordering))

            # remove generalisations of programs with redundant rules
            if is_recursive and len(prog) > 2 and tester.has_redundant_rule(prog):
                with settings.stats.duration('has_redundant_rule'):
                    add_gen = True
                    r1, r2 = tester.find_redundant_rules(prog)
                    if settings.showcons:
                        print('\t','r1',format_rule(order_rule(r1)))
                        print('\t','r2',format_rule(order_rule(r2)))
                    new_cons.append((Constraint.GENERALISATION, [r1,r2], None))

            if not add_spec and not pruned_more_general and not pruned_sub_incomplete:
                could_prune_later[prog] = pos_covered

            seen_better_rec = False
            if is_recursive and not inconsistent and not subsumed and not add_gen and num_pos_covered > 0:
                if settings.order_space:
                    # this check does not assume that we search by increasing program size
                    subsumed = is_subsumed(pos_covered, prog_size, rec_success_sets)
                else:
                    # this check assumes that we search by increasing program size
                    seen_better_rec = pos_covered in rec_success_sets or any(pos_covered.issubset(xs) for xs in rec_success_sets)

            # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
            if not inconsistent and not subsumed and not add_gen and num_pos_covered > 0 and not seen_better_rec and not pruned_more_general:

                # update success sets
                success_sets[pos_covered] = prog_size
                if is_recursive:
                    rec_success_sets[pos_covered] = prog_size

                # COMBINE
                with settings.stats.duration('combine'):
                    new_solution_found = combiner.update_best_prog(prog, pos_covered)

                # if we find a new solution, update the maximum program size
                # if only adding nogoods, eliminate larger programs
                if new_solution_found:

                    # if non-separable program covers all examples, stop
                    if not inconsistent and num_pos_covered == num_pos and not settings.order_space:
                        return

                    settings.max_literals = combiner.max_size-1
                    if size >= settings.max_literals and not settings.order_space:
                        return

                    if settings.single_solve:
                        # AC: sometimes adding these size constraints can take longer
                        for i in range(combiner.max_size, max_size+1):
                            size_con = [(atom_to_symbol("size", (i,)), True)]
                            model.context.add_nogood(size_con)

                if not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                    with settings.stats.duration('prune subsumed backtrack'):
                        xs = prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester, prog_size, check_coverage=combiner.solution_found)
                        for x in xs:
                            new_cons.append((Constraint.SPECIALISATION, x, None))

            # BUILD CONSTRAINTS
            if add_spec and not pruned_sub_incomplete and not pruned_more_general and not add_redund2:
                new_cons.append((Constraint.SPECIALISATION, prog, rule_ordering))

            if add_gen and not pruned_sub_inconsistent:
                if settings.recursion_enabled or settings.pi_enabled:
                    if not pruned_sub_incomplete:
                        new_cons.append((Constraint.GENERALISATION, prog, rule_ordering))

            if add_redund1 and not pruned_sub_incomplete:
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog, rule_ordering))

            if add_redund2 and not pruned_sub_incomplete:
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog, rule_ordering))

            # CONSTRAIN
            with settings.stats.duration('constrain'):
                generator.constrain(new_cons, model)

        # if not pi_or_rec:
        if settings.single_solve:
            break

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
