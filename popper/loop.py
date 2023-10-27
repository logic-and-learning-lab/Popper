import time
import numbers
import collections
from itertools import permutations
from itertools import chain, combinations
from collections import deque
from . explain import get_raw_prog as get_raw_prog2
# from . combine import Combiner
from . explain import Explainer, head_connected, get_raw_prog, seen_more_general_unsat, has_valid_directions, order_body, connected
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, prog_has_invention, order_rule, calc_prog_size, format_literal, theory_subsumes, rule_subsumes, format_prog, format_prog2, order_rule2, Constraint, bias_order, mdl_score
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
    unsat_cores = list(explainer.explain_totally_incomplete(prog, directions, settings.noisy))

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
            out_cons.append((Constraint.SPECIALISATION, subprog, None, None))
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
            out_cons.append((Constraint.GENERALISATION, subprog, None, None))
            pruned_subprog = True

    if pruned_subprog:
        return out_cons

    if len(rec) == 1:
        return out_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_inconsistent(subprog):
                out_cons.append((Constraint.GENERALISATION, subprog, None, None))
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
            new_cons.append((Constraint.GENERALISATION, subprog , None, None))
            pruned_subprog = True

    if pruned_subprog:
        return new_cons

    if len(rec) == 1:
        return new_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_non_functional(subprog):
                new_cons.append((Constraint.GENERALISATION, subprog , None, None))

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
                # with settings.stats.duration('variants'):
                for _, x in find_variants(new_rule):
                    pruned2.add(x)

        to_delete.add(prog2)

        if pruned_subprog == False:
            # with settings.stats.duration('variants'):
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

def build_constraints_previous_hypotheses(generator, num_pos, num_neg, seen_hyp_spec, seen_hyp_gen, score, best_size):
    cons = []
    # print(f"new best score {score}")
    for k in [k for k in seen_hyp_spec if k > score+num_pos+best_size]:
        to_delete = []
        for prog, tp, fn, tn, fp, size, rule_ordering in seen_hyp_spec[k]:
            # mdl = mdl_score(tuple((tp, fn, tn, fp, size)))
            mdl = mdl_score(fn, fp, size)
            if score+num_pos+best_size < fp+size+mdl:
                spec_size = score-mdl+num_pos+best_size
                if spec_size <= size:
                    to_delete.append([prog, tp, fn, tn, fp, size, rule_ordering])
                # _, con = generator.build_specialisation_constraint(prog, rule_ordering, spec_size=spec_size)
                # cons.add(con)
                cons.append((Constraint.SPECIALISATION, prog, rule_ordering, spec_size))
                # print('SPEC', format_prog(prog))
        for to_del in to_delete:
            seen_hyp_spec[k].remove(to_del)
    for k in [k for k in seen_hyp_gen if k > score + num_neg + best_size]:
        to_delete = []
        for prog, tp, fn, tn, fp, size, rule_ordering in seen_hyp_gen[k]:
            # mdl = mdl_score(tuple((tp, fn, tn, fp, size)))
            mdl = mdl_score(fn, fp, size)
            if score + num_neg + best_size < fn + size + mdl:
                gen_size = score - mdl + num_neg + best_size
                if gen_size <= size:
                    to_delete.append([prog, tp, fn, tn, fp, size, rule_ordering])
                # _, con = generator.build_generalisation_constraint(prog, rule_ordering, gen_size=gen_size)
                # cons.add(con)
                cons.append((Constraint.GENERALISATION, prog, rule_ordering, gen_size))
                # print('GEN', format_prog(prog))
        for to_del in to_delete:
            seen_hyp_gen[k].remove(to_del)
    return cons

def load_solver(settings, tester):
    if settings.solver == "clingo":
        if settings.noisy:
            from . combine_mdl import Combiner
            return Combiner(settings, tester)
            print('HELLO')

        else:
            from . combine import Combiner
            return Combiner(settings, tester)
    else:
        from . combine_ms import Combiner
        settings.maxsat_timeout = None
        settings.stats.maxsat_calls = 0
        settings.exact_maxsat_solver="rc2"
        if settings.noisy:
            settings.lex = False
        else:
            settings.lex = True
            settings.best_mdl = False
            settings.lex_via_weights = False
        return Combiner(settings, tester)

        # settings.lex = True






def popper(settings):
    with settings.stats.duration('load data'):
        tester = Tester(settings)

    explainer = Explainer(settings, tester)
    grounder = Grounder(settings)

    settings.solver = 'maxsat'
    # settings.solver = 'clingo'
    settings.nonoise = not settings.noisy
    # TODO AC: FIX
    settings.solution_found = False

    num_pos = len(settings.pos_index)
    num_neg = len(settings.neg_index)

    if settings.noisy:
        min_score = None
        saved_scores = dict()
        settings.best_prog_score = 0, num_pos, num_neg, 0, 0
        # settings.best_prog = []
        settings.best_mdl = num_pos
        max_size = min((1 + settings.max_body) * settings.max_rules, num_pos)
        
        # these are used to save hypotheses for which we pruned spec / gen from a certain size only
        # once we update the best mdl score, we can prune spec / gen from a better size for some of these
        seen_hyp_spec = collections.defaultdict(list)
        seen_hyp_gen = collections.defaultdict(list)
    else:
        max_size = (1 + settings.max_body) * settings.max_rules

    combiner = load_solver(settings, tester)

    # deduce bk cons
    bkcons = []
    if settings.bkcons:
        settings.datalog = True
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
    success_sets_noise = {}
    rec_success_sets = {}

    # maintain a set of programs that we have not yet pruned
    could_prune_later = {}

    to_combine = []

    last_size = None

    search_order = bias_order(settings, max_size)

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
            # else:
                # settings.logger.info(f'SIZE: {size} MAX_SIZE: {settings.max_literals}')

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
            subsumed = False
            spec_size = None
            gen_size = None
            size_change = False

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


            if last_size == None or prog_size != last_size:
                size_change = True
                last_size = prog_size
                if not settings.order_space:
                    settings.logger.info(f'Generating programs of size: {prog_size}')

            if settings.single_solve and last_size > settings.max_literals:
                break

            is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
            has_invention = settings.pi_enabled and prog_has_invention(prog)

            # test a program
            with settings.stats.duration('test'):
                if settings.noisy:
                    # TODO: DO WE ALWAYS NEED TO TEST AGAINST ALL EXAMPLES
                    pos_covered, neg_covered = tester.test_prog_all(prog)
                    inconsistent = len(neg_covered) > 0
                else:
                    pos_covered, inconsistent = tester.test_prog(prog)

            num_pos_covered = len(pos_covered)

            # if non-separable program covers all examples, stop
            if not inconsistent and num_pos_covered == num_pos and not settings.order_space:
                # settings.best_prog = prog
                settings.solution = prog
                settings.best_prog_score = num_pos, 0, num_neg, 0, prog_size
                settings.best_mdl = prog_size
                return


            if settings.noisy:
                tp = len(pos_covered)
                fp = len(neg_covered)
                fn = num_pos-tp
                tn = num_neg-fp
                score = tp, fn, tn, fp, prog_size
                mdl = mdl_score(fn, fp, prog_size)
                if settings.debug:
                    settings.logger.debug(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} mdl:{mdl}')

                saved_scores[prog] = [fp, fn, prog_size]
                if not min_score:
                    min_score = prog_size
                # print(mdl, settings.best_mdl)

                if mdl < settings.best_mdl:
                    # if settings.delete_combine:
                        # combiner.update_deleted_progs(settings.best_mdl-min_score, mdl-min_score)
                    # HORRIBLE
                    combiner.best_cost = mdl
                    settings.best_prog_score = score
                    settings.solution = prog
                    settings.best_mdl = mdl
                    settings.max_literals = mdl-1
                    settings.print_incomplete_solution2(prog, tp, fn, tn, fp, prog_size)
                    new_cons.extend(build_constraints_previous_hypotheses(generator, num_pos, num_neg, seen_hyp_spec, seen_hyp_gen, mdl, prog_size))

            # if it does not cover any example, prune specialisations
            if num_pos_covered == 0:
                add_spec = True
                # if recursion and no PI, apply redundancy constraints
                if settings.recursion_enabled:
                    add_redund2 = True
                    if len(prog) == 1 and not settings.pi_enabled:
                        add_redund1 = True

            # if consistent, prune specialisations
            if not inconsistent:
                add_spec = True

            #  if covers all positive examples prune generalisations
            if num_pos_covered == num_pos:
                add_gen = True

            if not has_invention:
                explainer.add_seen(prog)
                if num_pos_covered == 0 or (settings.noisy and len(pos_covered) < prog_size):
                    # if the programs does not cover any positive examples, check whether it is has an unsat core
                    with settings.stats.duration('find mucs'):
                        cons_ = explain_incomplete(settings, explainer, tester, prog, directions)
                        new_cons.extend(cons_)
                        pruned_sub_incomplete = len(cons_) > 0

            if not settings.noisy:
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
                        covers_too_few = settings.solution_found and not settings.order_space and num_pos_covered == 1
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
                                new_cons.append((Constraint.SPECIALISATION, x, None, None))

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
                    neg_covered = frozenset()

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

                seen_better_rec = False
                if is_recursive and not inconsistent and not subsumed and not add_gen and num_pos_covered > 0:
                    if settings.order_space:
                        # this check does not assume that we search by increasing program size
                        subsumed = is_subsumed(pos_covered, prog_size, rec_success_sets)
                    else:
                        # this check assumes that we search by increasing program size
                        seen_better_rec = pos_covered in rec_success_sets or any(pos_covered.issubset(xs) for xs in rec_success_sets)

            if settings.noisy:
                # if a program of size k covers less than k positive examples, we can prune its specialisations
                # otherwise no useful mdl induction has taken place
                if tp < prog_size:
                    add_spec = True

                # we can prune specialisations with size greater than prog_size+fp or tp
                # only prune if the specialisation bounds are smaller than existing bounds
                spec_size_ = min([tp, fp + prog_size])
                if spec_size_ <= prog_size:
                    add_spec = True
                elif len(prog) == 1 and spec_size_ < settings.max_body + 1 and spec_size_ < settings.max_literals:
                    spec_size = spec_size_
                elif len(prog) > 1 and spec_size_ < settings.max_literals:
                    spec_size = spec_size_

                # only prune if the generalisation bounds are smaller than existing bounds
                gen_size_ = min([fn + prog_size, num_pos-fp, settings.best_mdl - mdl + num_pos + prog_size])
                if gen_size_ <= prog_size:
                    add_gen = True
                if gen_size_ < settings.max_literals:
                    gen_size = gen_size_

                if not add_spec and False:
                    with settings.stats.duration('spec_subset'):
                        # print(type(prog))
                        for i in range(len(prog)):
                            (head, body) = list(prog)[i]
                            for subbody in chain.from_iterable(combinations(body, k) for k in range(1, len(body))):
                                subprog = list(prog)[:i] + [(head, subbody)] + list(prog)[i + 1:]
                                subprog = frozenset(subprog)
                                # print('\t'*3,format_prog(subprog))
                                if subprog in saved_scores:
                                    # print('\t'*3,format_prog(subprog))
                                    # assert(False)
                                    [old_fp, old_fn, old_size] = saved_scores[subprog]
                                    if rule_vars((head, subbody)) == rule_vars(list(prog)[i]) and old_fp + old_fn + old_size <= fn + fp + size:
                                        settings.stats.pruned_count += 1
                                        print('MOOOO')
                                        assert(False)
                                        add_spec = True
                                        break

            # remove generalisations of programs with redundant literals
            if is_recursive:
                for rule in prog:
                    if tester.has_redundant_literal([rule]):
                        add_gen = True
                        new_cons.append((Constraint.GENERALISATION, [rule], None, None))
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
                    new_cons.append((Constraint.GENERALISATION, [r1,r2], None, None))

            if not add_spec and not pruned_more_general and not pruned_sub_incomplete:
                could_prune_later[prog] = pos_covered


            call_combine = False
            if settings.noisy:
                if not is_recursive and not has_invention and tp >= prog_size+fp and num_pos_covered >= prog_size and fp+prog_size < settings.best_mdl:
                    success_sets_noise[tuple([pos_covered, neg_covered])] = prog
                    call_combine = True
            else:
                # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
                if not inconsistent and not subsumed and not add_gen and num_pos_covered > 0 and not seen_better_rec and not pruned_more_general:
                    call_combine = True

                if settings.order_space and not inconsistent and not subsumed and num_pos_covered > 0 and not seen_better_rec and not pruned_more_general:
                    call_combine = True

                if call_combine:
                    success_sets[pos_covered] = prog_size
                    if is_recursive:
                        rec_success_sets[pos_covered] = prog_size

            if call_combine:
                to_combine.append((prog, pos_covered, neg_covered))

                if not settings.noisy and not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                    with settings.stats.duration('prune backtrack'):
                        xs = prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester, prog_size, check_coverage=settings.solution_found)
                        for x in xs:
                            new_cons.append((Constraint.SPECIALISATION, x, None, None))

            if to_combine and (len(to_combine) >= settings.batch_size or size_change):

                # COMBINE
                with settings.stats.duration('combine'):
                    # is_new_solution_found = combiner.update_best_prog([(prog, pos_covered, [])])
                    # print('calling combine', len(to_combine))
                    # t1 = time.time()
                    is_new_solution_found = combiner.update_best_prog(to_combine)
                    # print(f'combine time: {time.time()-t1}')

                to_combine=[]

                new_hypothesis_found = is_new_solution_found != None

                # if we find a new solution, update the maximum program size
                # if only adding nogoods, eliminate larger programs
                if new_hypothesis_found:
                    # if settings.noisy:
                        # print('new_hypothesis_found', settings.best_mdl, combiner.best_cost)
                    new_hypothesis, conf_matrix = is_new_solution_found
                    tp, fn, tn, fp, hypothesis_size = conf_matrix
                    settings.best_prog_score = conf_matrix
                    settings.solution = new_hypothesis
                    best_score = mdl_score(fn, fp, hypothesis_size)
                    # if settings.noisy:
                        # print('new_hypothesis_found', settings.best_mdl, best_score)
                    settings.print_incomplete_solution2(new_hypothesis, tp, fn, tn, fp, hypothesis_size)

                    if settings.noisy and best_score < settings.best_mdl:
                        settings.best_mdl = best_score
                        settings.max_literals = settings.best_mdl - 1
                        new_cons.extend(build_constraints_previous_hypotheses(generator, num_pos, num_neg, seen_hyp_spec, seen_hyp_gen, settings.best_mdl, prog_size))
                        if settings.single_solve:
                            # AC: sometimes adding these size constraints can take longer
                            for i in range(best_score, max_size+1):
                                size_con = [(atom_to_symbol("size", (i,)), True)]
                                model.context.add_nogood(size_con)
                    # print("HERE!!!", tp, fn, tn, fp)
                    if not settings.noisy and fp == 0 and fn == 0:
                        settings.solution_found = True
                        settings.max_literals = hypothesis_size-1

                        if size >= settings.max_literals and not settings.order_space:
                            print('POOPER')
                            return

                        # AC: sometimes adding these size constraints can take longer
                        for i in range(hypothesis_size, max_size+1):
                            size_con = [(atom_to_symbol("size", (i,)), True)]
                            model.context.add_nogood(size_con)

            # BUILD CONSTRAINTS
            if add_spec and not pruned_sub_incomplete and not pruned_more_general and not add_redund2:
                new_cons.append((Constraint.SPECIALISATION, prog, rule_ordering, None))

            if settings.noisy and not add_spec and spec_size and not pruned_sub_incomplete:
                if spec_size <= settings.max_literals and ((is_recursive or has_invention or spec_size <= settings.max_body)):
                    new_cons.append((Constraint.SPECIALISATION, prog, rule_ordering, spec_size))
                    seen_hyp_spec[fp+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size, rule_ordering])

            if add_gen and not pruned_sub_inconsistent:
                if settings.noisy or settings.recursion_enabled or settings.pi_enabled:
                    if not pruned_sub_incomplete:
                        new_cons.append((Constraint.GENERALISATION, prog, rule_ordering, None))
                else:
                    if not add_spec:
                        new_cons.append((Constraint.GENERALISATION, prog, rule_ordering, None))

            if settings.noisy and not add_gen and gen_size and not pruned_sub_inconsistent:
                if gen_size <= settings.max_literals and (settings.recursion_enabled or settings.pi_enabled) and not pruned_sub_incomplete:
                    new_cons.append((Constraint.GENERALISATION, prog, rule_ordering, gen_size))
                    seen_hyp_gen[fn+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size, rule_ordering])

            if add_redund1 and not pruned_sub_incomplete:
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog, rule_ordering))

            if add_redund2 and not pruned_sub_incomplete:
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog, rule_ordering))

            if settings.noisy and not add_spec and not add_gen:
                new_cons.append((Constraint.BANISH, prog, rule_ordering))

            # CONSTRAIN
            with settings.stats.duration('constrain'):
                generator.constrain(new_cons, model)

        # if not pi_or_rec:
        if to_combine:
            # TODO: AWFUL: FIX REFACOTRING
            # COMBINE
            with settings.stats.duration('combine'):
                # is_new_solution_found = combiner.update_best_prog([(prog, pos_covered, [])])
                # print('calling combine', len(to_combine))
                # t1 = time.time()
                is_new_solution_found = combiner.update_best_prog(to_combine)
                # print(f'combine time: {time.time()-t1}')
            to_combine=[]

            new_hypothesis_found = is_new_solution_found != None

            # if we find a new solution, update the maximum program size
            # if only adding nogoods, eliminate larger programs
            if new_hypothesis_found:
                new_hypothesis, conf_matrix = is_new_solution_found
                tp, fn, tn, fp, hypothesis_size = conf_matrix
                settings.best_prog_score = conf_matrix
                settings.solution = new_hypothesis
                best_score = mdl_score(fn, fp, hypothesis_size)
                settings.print_incomplete_solution2(new_hypothesis, tp, fn, tn, fp, hypothesis_size)

                if not settings.noisy and fp == 0 and fn == 0:
                    settings.solution_found = True
                    settings.max_literals = hypothesis_size-1

                    if size >= settings.max_literals and not settings.order_space:
                        print('POOPER')
                        return
        if settings.single_solve:
            break
        # print('I AM HERE!!!!!!', len(to_combine))
    # print('PLEASE NOOOOO', len(to_combine))
    assert(len(to_combine) == 0)

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
