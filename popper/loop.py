import time
from collections import defaultdict
from bitarray.util import subset, any_and, ones
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations
from . tester import Tester
from . bkcons import get_bk_cons
from . unsat import UnsatCoreFinder
from . allsat import AllSatCoreFinder
from . subsume import SubsumeChecker
from . state import SearchState
from . combine_helper import CombineHelper


def popper(settings, tester, bkcons):
    state = SearchState()
    unsatcore_finder = UnsatCoreFinder(settings, tester)
    allsatcore_finder = AllSatCoreFinder(settings, tester)
    subsumer = SubsumeChecker(settings, tester, state)


    num_pos, num_neg = tester.num_pos, tester.num_neg


    # TODO, MOVE TO COMBINE_HELPER
    to_combine = set()
    combine_helper = CombineHelper(settings, tester, state, to_combine)

    # generator that builds programs
    with settings.stats.duration('init'):
        if settings.single_solve:
            from .gen2 import Generator
        elif settings.max_rules == 2 and not settings.pi_enabled:
            from .gen3 import Generator
        else:
            from .generate import Generator
        generator = Generator(settings, bkcons)

    # Only used in noisy mode (initialized later)
    seen_hyp_spec = None
    seen_hyp_gen = None


    # given a program with more than one rule, look for inconsistent subrules/subprograms
    def explain_inconsistent(prog):
        base = []
        rec = []
        pruned_base = False

        for rule in prog:
            if rule_is_recursive(rule):
                rec.append(rule)
                continue

            base.append(rule)
            subprog = frozenset([rule])
            if tester.test_prog_inconsistent(subprog):
                pruned_base = True
                yield (Constraint.GENERALISATION, subprog)

        if pruned_base or len(rec) == 1:
            return

        for r1 in base:
            for r2 in rec:
                subprog = frozenset([r1, r2])
                if tester.test_prog_inconsistent(subprog):
                    yield (Constraint.GENERALISATION, subprog)

    def build_constraints_previous_hypotheses(score, best_size):
        # NOTE: fixed the trailing comma bug from the class version
        nonlocal seen_hyp_spec, seen_hyp_gen
        cons = []

        if seen_hyp_spec is None or seen_hyp_gen is None:
            return cons

        for k in [k for k in seen_hyp_spec if k > score + num_pos + best_size]:
            to_delete = []
            for prog, tp, fn, tn, fp, size in seen_hyp_spec[k]:
                mdl = mdl_score(fn, fp, size)
                if score + num_pos + best_size < fp + size + mdl:
                    spec_size = score - mdl + num_pos + best_size
                    if spec_size <= size:
                        to_delete.append([prog, tp, fn, tn, fp, size])
                    cons.append((Constraint.SPECIALISATION, prog, spec_size))
            for to_del in to_delete:
                seen_hyp_spec[k].remove(to_del)

        for k in [k for k in seen_hyp_gen if k > score + num_neg + best_size]:
            to_delete = []
            for prog, tp, fn, tn, fp, size in seen_hyp_gen[k]:
                mdl = mdl_score(fn, fp, size)
                if score + num_neg + best_size < fn + size + mdl:
                    gen_size = score - mdl + num_neg + best_size
                    if gen_size <= size:
                        to_delete.append([prog, tp, fn, tn, fp, size])
                    cons.append((Constraint.GENERALISATION, prog, gen_size))
            for to_del in to_delete:
                seen_hyp_gen[k].remove(to_del)

        for x in cons:
            print(x)
        return cons

    if settings.noisy:
        min_score = None
        saved_scores = dict()
        settings.best_prog_score = (0, num_pos, num_neg, 0, 0)
        settings.best_mdl = num_pos
        # save hypotheses for which we pruned spec / gen from a certain size only
        # once we update the best mdl score, we can prune spec / gen from a better size for some of these
        seen_hyp_spec, seen_hyp_gen = defaultdict(list), defaultdict(list)
        settings.max_size = min((1 + settings.max_body) * settings.max_rules, num_pos)
    else:
        settings.max_size = (1 + settings.max_body) * settings.max_rules


    last_size = None
    min_coverage = settings.min_coverage = 1

    for size in range(2, settings.max_size + 1):
        if size > settings.max_literals:
            continue

        with settings.stats.duration('init'):
            generator.update_solver(size)

        while True:
            pruned_sub_inconsistent = pruned_more_general = False
            add_spec = add_gen = add_redund1 = add_redund2 = False
            subsumed = subsumed_by_two = covers_too_few = noisy_subsumed = False
            spec_size = gen_size = None
            size_change = False

            new_cons = []

            # GENERATE A PROGRAM
            with settings.stats.duration('generate'):
                prog = generator.get_prog()
                if prog is None:
                    break

            prog_size = calc_prog_size(prog)
            is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
            has_invention = settings.pi_enabled and prog_has_invention(prog)

            settings.stats.total_programs += 1

            if settings.stats.total_programs % 10000 == 0:
                tester.janus_clear_cache()

            if settings.debug:
                settings.logger.debug(f'Program {settings.stats.total_programs}:')
                settings.logger.debug(format_prog(prog))

            if last_size is None or prog_size != last_size:
                size_change = True
                last_size = prog_size
                settings.search_depth = prog_size
                settings.logger.info(f'Generating programs of size: {prog_size}')

            # TEST
            with settings.stats.duration('test'):
                if settings.noisy:
                    pos_covered, neg_covered, inconsistent, skipped, skip_early_neg = tester.test_prog_noisy(prog, prog_size)
                else:
                    pos_covered, inconsistent = tester.test_prog(prog)
                    skipped, skip_early_neg = False, False

            tp = pos_covered.count(1)
            fn = num_pos - tp
            fp = None

            # if non-separable program covers all examples, stop
            if not inconsistent and tp == num_pos and not skipped:
                settings.solution = prog
                settings.best_prog_score = (num_pos, 0, num_neg, 0, prog_size)
                settings.best_mdl = prog_size
                return

            if settings.noisy and not skipped:
                fp = neg_covered.count(1)
                tn = num_neg - fp
                score = (tp, fn, tn, fp, prog_size)
                mdl = mdl_score(fn, fp, prog_size)
                if settings.debug:
                    settings.logger.debug(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} mdl:{mdl}')
                saved_scores[prog] = [fp, fn, prog_size]
                if not min_score:
                    min_score = prog_size

                if mdl < settings.best_mdl:
                    if skip_early_neg:
                        assert False
                    # HORRIBLE
                    combine_helper.combiner.best_cost = mdl
                    settings.best_prog_score = score
                    settings.solution = prog
                    settings.best_mdl = mdl
                    settings.max_literals = mdl - 1
                    settings.print_incomplete_solution2(prog, tp, fn, tn, fp, prog_size)
                    new_cons.extend(build_constraints_previous_hypotheses(mdl, prog_size))

            # if it does not cover any example, prune specialisations
            if tp == 0:
                add_spec = True
                # if recursion and no PI, apply redundancy constraints
                if settings.recursion_enabled:
                    add_redund2 = True
                    if len(prog) == 1 and not settings.pi_enabled:
                        add_redund1 = True

            # if consistent, prune specialisations
            if not inconsistent and not skipped:
                add_spec = True

            # if covers all positive examples prune generalisations
            if tp == num_pos:
                add_gen = True

            # if the program does not cover any positive examples, check whether it has an unsat core
            if not has_invention:
                if tp < min_coverage or (settings.noisy and tp <= prog_size):
                    with settings.stats.duration('find mucs'):
                        cons_ = tuple(unsatcore_finder.explain_incomplete(prog))
                        new_cons.extend(cons_)
                        pruned_more_general = len(cons_) > 0

            if tp > 0 and state.success_sets and (not settings.noisy or (settings.noisy and fp == 0)):
                with settings.stats.duration('check subsumed and covers_too_few'):
                    subsumed = pos_covered in state.success_sets or any(subset(pos_covered, xs) for xs in state.success_sets)
                    subsumed_by_two = (not subsumed) and subsumer.check_subsumed_by_two(pos_covered, prog_size)
                    covers_too_few = (not subsumed) and (not subsumed_by_two) and (not settings.noisy) and subsumer.check_covers_too_few(prog_size, pos_covered)

                if subsumed or subsumed_by_two or covers_too_few:
                    add_spec = True
                    noisy_subsumed = True

            if (not settings.noisy) and (not has_invention) and (not is_recursive) and (subsumed or subsumed_by_two or covers_too_few):
                subsumed_progs = []
                with settings.stats.duration('find most general subsumed/covers_too_few'):
                    subsumed_progs = subsumer.subsumed_or_covers_too_few(prog, seen=set())
                pruned_more_general = len(subsumed_progs) > 0

                if settings.showcons and not pruned_more_general:
                    if subsumed:
                        print('\t', 'SUBSUMED:', '\t', format_prog(prog))
                    elif subsumed_by_two:
                        print('\t', 'SUBSUMED BY TWO:', '\t', format_prog(prog))
                    elif covers_too_few:
                        print('\t', 'COVERS TOO FEW:', '\t', format_prog(prog))

                for subsumed_prog, message in subsumed_progs:
                    if settings.showcons:
                        print('\t', message, '\t', format_prog(prog))
                    subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog)
                    new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))


            # print(not settings.noisy, not pruned_more_general)
            if not settings.noisy and not pruned_more_general:
                if inconsistent:
                    add_gen = True
                    if is_recursive:
                        combine_helper.combiner.add_inconsistent(prog)
                        cons_ = frozenset(explain_inconsistent(prog))
                        new_cons.extend(cons_)
                        pruned_sub_inconsistent = len(cons_) > 0
                else:
                    neg_covered = frozenset()

                if not inconsistent and settings.functional_test and tp > 0 and not pruned_more_general:
                    if tester.is_non_functional(prog):
                        add_gen = True
                        add_spec = False
                        inconsistent = True
                        with settings.stats.duration('explain_none_functional'):
                            cons_ = explain_none_functional(settings, tester, prog)
                            new_cons.extend(cons_)

            if settings.noisy:
                if tp <= prog_size:
                    add_spec = True

                if not skipped:
                    spec_size_ = min([tp, fp + prog_size])
                    if spec_size_ <= prog_size:
                        add_spec = True
                    elif len(prog) == 1 and spec_size_ < settings.max_body + 1 and spec_size_ < settings.max_literals:
                        spec_size = spec_size_
                    elif len(prog) > 1 and spec_size_ < settings.max_literals:
                        spec_size = spec_size_

                if skipped or skip_early_neg:
                    gen_size_ = fn + prog_size
                    if gen_size_ <= prog_size:
                        add_gen = True
                    if gen_size_ < settings.max_literals:
                        gen_size = gen_size_
                else:
                    gen_size_ = min([fn + prog_size, num_pos - fp, settings.best_mdl - mdl + num_pos + prog_size])
                    if gen_size_ <= prog_size:
                        add_gen = True
                    if gen_size_ < settings.max_literals:
                        gen_size = gen_size_

            # remove generalisations of programs with redundant literals
            if is_recursive:
                for rule in prog:
                    if rule_is_recursive(rule) and settings.max_rules == 2:
                        continue
                    if tester.has_redundant_literal(frozenset([rule])):
                        add_gen = True
                        new_cons.append((Constraint.GENERALISATION, [rule]))
                        if settings.showcons:
                            print('\t', format_rule(rule), '\t', 'has_redundant_literal')

            # remove a subset of theta-subsumed rules when learning recursive programs with more than two rules
            if settings.max_rules > 2 and is_recursive:
                new_cons.append((Constraint.TMP_ANDY, prog))

            with settings.stats.duration('check_reducible1'):
                xs, pruned_smaller = allsatcore_finder.check_redundant_literal(prog)
                if pruned_smaller:
                    pruned_more_general = True
                if xs:
                    add_spec = True
                    for x in xs:
                        if settings.showcons:
                            print('\t', 'REDUCIBLE_1:', '\t', ','.join(format_literal(literal) for literal in x))
                        new_cons.append((Constraint.UNSAT, x))

            # REDUCIBLE_2 (negative reducible)
            if (not add_spec) and (not pruned_more_general) and settings.datalog and (not settings.recursion_enabled) and num_neg > 0:
                with settings.stats.duration('check_reducible2'):
                    bad_prog = allsatcore_finder.check_neg_reducible(prog)
                    if bad_prog:
                        add_spec = True
                        pruned_more_general = True
                        if settings.showcons:
                            print('\t', 'REDUCIBLE_2:', '\t', format_prog(bad_prog))
                        new_cons.append((Constraint.SPECIALISATION, bad_prog))

            # must cover minimum number of examples
            if (not add_spec) and (not pruned_more_general):
                if tp < min_coverage:
                    add_spec = True

            # TMPHACK
            if not settings.noisy:
                neg_covered = None


            # COMBINE
            new_hypothesis_result = combine_helper.combine(prog, prog_size, pos_covered, neg_covered, inconsistent, subsumed, noisy_subsumed,add_gen, tp, fp, fn, pruned_more_general, skipped, skip_early_neg, is_recursive, has_invention)

            if new_hypothesis_result is None:
                pass
            else:
                new_hypothesis, conf_matrix = new_hypothesis_result
                tp3, fn3, tn3, fp3, hypothesis_size = conf_matrix
                settings.best_prog_score = conf_matrix
                settings.solution = new_hypothesis
                best_score = mdl_score(fn3, fp3, hypothesis_size)

                settings.print_incomplete_solution2(new_hypothesis, tp3, fn3, tn3, fp3, hypothesis_size)

                if settings.noisy and best_score < settings.best_mdl:
                    settings.best_mdl = best_score
                    settings.max_literals = settings.best_mdl - 1
                    new_cons.extend(build_constraints_previous_hypotheses(settings.best_mdl, prog_size))
                    if settings.single_solve:
                        for i in range(best_score, settings.max_size + 1):
                            generator.prune_size(i)

                if (not settings.noisy) and fp3 == 0 and fn3 == 0:
                    settings.solution_found = True
                    settings.max_literals = hypothesis_size - 1
                    min_coverage = settings.min_coverage = 2

                    if size >= settings.max_literals:
                        print('POOPER')
                        return

                    for i in range(hypothesis_size, settings.max_size + 1):
                        generator.prune_size(i)

            # BUILD CONSTRAINTS
            if add_spec and (not pruned_more_general) and (not add_redund2):
                new_cons.append((Constraint.SPECIALISATION, prog))

            if not skipped:
                if settings.noisy and (not add_spec) and spec_size and (not pruned_more_general):
                    if spec_size <= settings.max_literals and ((is_recursive or has_invention or spec_size <= settings.max_body)):
                        new_cons.append((Constraint.SPECIALISATION, prog, spec_size))
                        seen_hyp_spec[fp + prog_size + mdl].append([prog, tp, fn, tn, fp, prog_size])

            if add_gen and (not pruned_sub_inconsistent):
                if settings.noisy or settings.recursion_enabled or settings.pi_enabled:
                    if not pruned_more_general:
                        new_cons.append((Constraint.GENERALISATION, prog))
                else:
                    if not add_spec:
                        new_cons.append((Constraint.GENERALISATION, prog))

            if settings.noisy and (not add_gen) and gen_size and (not pruned_sub_inconsistent):
                if gen_size <= settings.max_literals and (settings.recursion_enabled or settings.pi_enabled) and (not pruned_more_general):
                    new_cons.append((Constraint.GENERALISATION, prog, gen_size))
                    seen_hyp_gen[fn + prog_size + mdl].append([prog, tp, fn, tn, fp, prog_size])

            if add_redund1 and (not pruned_more_general):
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog))

            if add_redund2 and (not pruned_more_general):
                new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog))

            if settings.noisy and (not add_spec) and (not add_gen):
                new_cons.append((Constraint.BANISH, prog))

            # CONSTRAIN
            with settings.stats.duration('constrain'):
                generator.constrain(new_cons)

        # last combine stage
        if to_combine:
            settings.last_combine_stage = True
            with settings.stats.duration('combine'):
                is_new_solution_found = combine_helper.combiner.update_best_prog(to_combine)
            # to_combine = set()
            to_combine.clear()

            if is_new_solution_found is not None:
                new_hypothesis, conf_matrix = is_new_solution_found
                tp4, fn4, tn4, fp4, hypothesis_size = conf_matrix
                settings.best_prog_score = conf_matrix
                settings.solution = new_hypothesis
                best_score = mdl_score(fn4, fp4, hypothesis_size)
                settings.print_incomplete_solution2(new_hypothesis, tp4, fn4, tn4, fp4, hypothesis_size)

                if (not settings.noisy) and fp4 == 0 and fn4 == 0:
                    settings.solution_found = True
                    settings.max_literals = hypothesis_size - 1
                    min_coverage = settings.min_coverage = 2
                    if size >= settings.max_literals:
                        assert False

        if settings.single_solve:
            break

    assert len(to_combine) == 0


def learn_solution(settings):
    t1 = time.time()
    settings.nonoise = not settings.noisy
    settings.solution_found = False

    with settings.stats.duration('load data'):
        tester = Tester(settings)

    bkcons = get_bk_cons(settings, tester)
    time_so_far = time.time()-t1
    timeout(settings, popper, (settings, tester, bkcons), timeout_duration=int(settings.timeout-time_so_far),)
    return settings.solution, settings.best_prog_score, settings.stats


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
            new_cons.append((Constraint.GENERALISATION, subprog))
            pruned_subprog = True

    if pruned_subprog:
        return new_cons

    if len(rec) == 1:
        return new_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_non_functional(subprog):
                new_cons.append((Constraint.GENERALISATION, subprog))

    return new_cons

