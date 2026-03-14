from bitarray.util import subset, ones
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, remap_variables, format_prog, print_incomplete_solution2
from . tester import Tester
from . bkcons import get_bk_cons
from . unsat import UnsatCoreFinder
from . allsat import AllSatCoreFinder
from . subsume import SubsumeChecker
from . state import SearchState
from . joiner import Joiner
from . combine_helper import CombineHelper
from . import logger
from . import stats

def load_generator(settings, state, bkcons):
    if settings.single_solve:
        from .gen2 import Generator
    elif settings.max_rules == 2 and not settings.pi_enabled:
        from .gen3 import Generator
    else:
        from .generate import Generator
    return Generator(settings, state, bkcons)

def update_best_hypothesis(settings, state, hypothesis, hypothesis_size, conf_matrix):
    state.best_hypothesis_score = conf_matrix
    state.best_hypothesis_size = hypothesis_size
    state.best_hypothesis = hypothesis
    print_incomplete_solution2(hypothesis, hypothesis_size, conf_matrix)
    _, fn, _, fp = conf_matrix

    if settings.noisy:
        mdl = mdl_score(fn, fp, hypothesis_size)
        state.best_hypothesis_mdl = mdl
        state.max_literals = mdl - 1
    elif fp == 0 and fn == 0:
        state.solution_found = True
        state.max_literals = hypothesis_size - 1
        state.min_pos_coverage = 2

def check_size_change(state, prog_size):
    size_change = False
    if state.search_depth is None or prog_size != state.search_depth:
        size_change = True
        state.search_depth = prog_size
        logger.info(f'Generating hypotheses of size: {prog_size}')
    return size_change

def popper(settings, tester, state, bkcons):
    unsatcore_finder = UnsatCoreFinder(settings, tester)
    allsatcore_finder = AllSatCoreFinder(settings, tester)
    subsumer = SubsumeChecker(settings, tester, state)
    generator = load_generator(settings, state, bkcons)
    combine_helper = CombineHelper(settings, tester, state)
    joiner = Joiner(settings, tester, state)
    num_pos, num_neg = tester.num_pos, tester.num_neg

    # initialise components depending on cost function
    if settings.noisy:
        state.best_hypothesis_score = (0, num_pos, num_neg, 0)
        state.best_hypothesis_mdl = num_pos
        build_constraints = build_constraints_noisy
        test_prog = tester.test_prog_noisy
    else:
        build_constraints = build_constraints_noiseless
        test_prog = tester.test_prog

    # GENERATE PROGRAMS
    for prog in generator.get_prog():

        stats.stats.total_programs += 1

        # HORRIBLE HACK DUE TO PROLOG MEMORY LEAK
        if stats.stats.total_programs % 10000 == 0:
            tester.janus_clear_cache()

        if settings.debug:
            logger.debug(f'Program {stats.stats.total_programs}:')
            logger.debug(format_prog(prog))

        prog_size = calc_prog_size(prog)

        # TEST
        with stats.duration('test'):
            test_result = test_prog(prog, prog_size)

        # if non-separable hypothesis is perfect, stop
        if not test_result.inconsistent and test_result.tp == num_pos:
            state.best_hypothesis = prog
            state.best_hypothesis_score = (num_pos, 0, num_neg, 0)
            return

        # if non-separable hypothesis has better mdl score, update best hypothesis
        if settings.noisy and test_result.mdl is not None and test_result.mdl < state.best_hypothesis_mdl:
            update_best_hypothesis(settings, state, prog, prog_size, test_result.conf_matrix)

        # BUILD CONSTRAINTS
        new_cons, add_to_combiner = build_constraints(settings, tester, state, unsatcore_finder, allsatcore_finder, subsumer, prog, prog_size, combine_helper, test_result)

        size_change = check_size_change(state, prog_size)

        # JOINER
        if False:
            with stats.duration('join'):
                join_result = joiner.join(prog, prog_size, test_result, size_change, add_to_combiner)

        # COMBINE
        combine_result = combine_helper.combine(prog, prog_size, test_result, size_change, add_to_combiner)

        # IF NEW HYPOTHESIS
        if combine_result:
            update_best_hypothesis(settings, state, *combine_result)

            # AC: TRY TO REFACTOR OUT
            if settings.noisy:
                new_cons.extend(build_constraints_previous_hypotheses(state.best_hypothesis_mdl, prog_size, num_pos, num_neg, state))

            # PRUNE BIGGER SPACES
            if (settings.noisy and settings.single_solve) or (not settings.noisy and state.solution_found):
                for i in range(state.max_literals+1, 1000):
                    generator.prune_size(i)

        # CONSTRAIN
        with stats.duration('constrain'):
            generator.constrain(new_cons)

    # LAST COMBINE STAGE
    with stats.duration('combine'):
        combine_result = combine_helper.update_best_prog(combine_helper.to_combine, last_combine_stage=True)
    
    if combine_result:
        update_best_hypothesis(settings, state, *combine_result)

def learn_solution(settings):
    state = SearchState()
    state.start_time()
    with stats.duration('load data'):
        tester = Tester(settings, state)
    # nasty
    state.uncovered = ones(tester.num_pos)
    bkcons = get_bk_cons(settings, tester)
    timeout(settings, popper, (settings, tester, state, bkcons), timeout_duration=state.time_remaining(settings.timeout),)
    return state.best_hypothesis, state.best_hypothesis_score, stats

def build_constraints_noiseless(settings, tester, state, unsatcore_finder, allsatcore_finder, subsumer, prog, prog_size, combine_helper, test_result):
    new_cons = []
    pruned_sub_inconsistent = pruned_more_general = False
    add_spec = add_gen = add_redund1 = add_redund2 = False
    is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
    has_invention = settings.pi_enabled and prog_has_invention(prog)
    add_to_combiner_ = True
    tp = test_result.tp

    # if hypothesis covers all positive examples prune generalisations (generalisations are variants in the case of no recursion)
    if tp == tester.num_pos:
        add_gen = True

    # if hypothesis does not cover enough example, prune specialisations
    if tp < state.min_pos_coverage:
        add_spec = True
        add_to_combiner_ = False

        # if recursion and no PI, apply redundancy constraints
        if settings.recursion_enabled:
            add_redund2 = True
            if len(prog) == 1 and not settings.pi_enabled:
                add_redund1 = True

        # check whether a hypothesis has an unsat core
        if not has_invention:
            with stats.duration('find mucs'):
                cons_ = tuple(unsatcore_finder.explain_incomplete(prog))
                new_cons.extend(cons_)
                pruned_more_general = len(cons_) > 0

    # check whether a hypothesis is coverage subsumed by a previously seen hypothesis or whether it entails too few examples
    if tp > 0 and state.success_sets:
        pos_covered = test_result.pos_covered

        with stats.duration('check subsumed and covers_too_few'):
            subsumed = pos_covered in state.success_sets or any(subset(pos_covered, xs) for xs in state.success_sets)
            subsumed_by_two = not subsumed and subsumer.check_subsumed_by_two(pos_covered, prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and subsumer.check_covers_too_few(prog_size, pos_covered)

        if subsumed or subsumed_by_two or covers_too_few:
            add_spec = True
            add_to_combiner_ = False

            if not has_invention and not is_recursive:
                subsumed_progs = []
                with stats.duration('find most general subsumed/covers_too_few'):
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

    # if hypothesis is inconsistent, prune generalisations and do not add to the combiner
    if test_result.inconsistent:
        add_to_combiner_ = False
        add_gen = True
        if not pruned_more_general and is_recursive:
            combine_helper.add_inconsistent(prog)
            cons_ = frozenset(explain_inconsistent(tester, prog))
            new_cons.extend(cons_)
            pruned_sub_inconsistent = len(cons_) > 0
    else:
        # if hypothesis is consistent, prune specialisations
        add_spec = True

    # prune generalisations of rules with redundant literals
    if is_recursive:
        rec_cons, rec_add_gen = check_recursive_redundancy(settings, tester, prog)
        new_cons.extend(rec_cons)
        add_gen = add_gen and rec_add_gen

    # check whether a rule contains an implied literal, .e.g even(A) -> int(A)
    # https://arxiv.org/pdf/2502.01232
    with stats.duration('check_reducible1'):
        xs, pruned_smaller = allsatcore_finder.check_redundant_literal(prog)
        if pruned_smaller:
            pruned_more_general = True
        if xs:
            add_to_combiner_ = False
            add_spec = True
            for x in xs:
                if settings.showcons:
                    print('\t', 'REDUCIBLE_1:', '\t', ','.join(format_literal(literal) for literal in x))
                new_cons.append((Constraint.UNSAT, x))

    # check whether a rule contains an indiscriminate literal
    # https://arxiv.org/pdf/2502.01232
    if not add_spec and not pruned_more_general and settings.datalog and not settings.recursion_enabled and tester.num_neg > 0:
        with stats.duration('check_reducible2'):
            bad_prog = allsatcore_finder.check_neg_reducible(prog)
            if bad_prog:
                add_to_combiner_ = False
                add_spec = True
                pruned_more_general = True
                if settings.showcons:
                    print('\t', 'REDUCIBLE_2:', '\t', format_prog(bad_prog))
                new_cons.append((Constraint.SPECIALISATION, bad_prog))

    # BUILD CONSTRAINTS
    if not pruned_more_general:
        if add_gen and not pruned_sub_inconsistent:
            if settings.recursion_enabled or settings.pi_enabled:
                new_cons.append((Constraint.GENERALISATION, prog))
        elif not add_spec:
            new_cons.append((Constraint.GENERALISATION, prog))

        if add_spec and not add_redund2:
            new_cons.append((Constraint.SPECIALISATION, prog))

        if add_redund1:
            new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog))

        if add_redund2:
            new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog))

    return new_cons, add_to_combiner_

def build_constraints_noisy(settings, tester, state, unsatcore_finder, allsatcore_finder, subsumer, prog, prog_size, combine_helper, test_result):
    pos_covered = test_result.pos_covered
    inconsistent = test_result.inconsistent
    mdl = test_result.mdl
    too_few_tp, too_many_fp = test_result.too_few_tp, test_result.too_many_fp
    num_pos, num_neg = tester.num_pos, tester.num_neg
    tp, fn, fp, tn = test_result.tp, test_result.fn, test_result.fp, test_result.tn
    seen_hyp_spec, seen_hyp_gen = state.seen_hyp_spec, state.seen_hyp_gen
    new_cons = []
    pruned_more_general = False
    add_spec = add_gen = add_redund1 = add_redund2 = False
    noisy_subsumed = False
    spec_size = gen_size = None
    is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
    has_invention = settings.pi_enabled and prog_has_invention(prog)

    # if non-separable hypothesis has better mdl score, update best prog
    if test_result.mdl is not None and test_result.mdl < state.best_hypothesis_mdl:
        update_best_hypothesis(settings, state, prog, prog_size, test_result.conf_matrix)
        new_cons.extend(build_constraints_previous_hypotheses(test_result.mdl, prog_size, num_pos, num_neg, state))

    # if it does not cover enough example, prune specialisations
    if tp < state.min_pos_coverage:
        add_spec = True
        # if recursion and no PI, apply redundancy constraints
        if settings.recursion_enabled:
            add_redund2 = True
            if len(prog) == 1 and not settings.pi_enabled:
                add_redund1 = True

    # if consistent, prune specialisations
    if not inconsistent and not too_few_tp:
        add_spec = True

    # if covers all positive examples prune generalisations
    if tp == num_pos:
        add_gen = True

    # if the program does not cover any positive examples, check whether it has an unsat core
    if not has_invention and tp < state.min_pos_coverage or tp <= prog_size:
        with stats.duration('find mucs'):
            cons_ = tuple(unsatcore_finder.explain_incomplete(prog))
            new_cons.extend(cons_)
            pruned_more_general = len(cons_) > 0

    if tp > 0 and state.success_sets and fp == 0:
        with stats.duration('check subsumed and covers_too_few'):
            subsumed = pos_covered in state.success_sets or any(subset(pos_covered, xs) for xs in state.success_sets)
            subsumed_by_two = not subsumed and subsumer.check_subsumed_by_two(pos_covered, prog_size)

        if subsumed or subsumed_by_two:
            add_spec = True
            noisy_subsumed = True

    if tp <= prog_size:
        add_spec = True

    if not too_few_tp:
        spec_size_ = min([tp, fp + prog_size])
        if spec_size_ <= prog_size:
            add_spec = True
        elif len(prog) == 1 and spec_size_ < settings.max_body + 1 and spec_size_ < state.max_literals:
            spec_size = spec_size_
        elif len(prog) > 1 and spec_size_ < state.max_literals:
            spec_size = spec_size_

    if too_few_tp or too_many_fp:
        gen_size_ = fn + prog_size
        if gen_size_ <= prog_size:
            add_gen = True
        if gen_size_ < state.max_literals:
            gen_size = gen_size_
    else:
        gen_size_ = min([fn + prog_size, num_pos - fp, state.best_hypothesis_mdl - mdl + num_pos + prog_size])
        if gen_size_ <= prog_size:
            add_gen = True
        if gen_size_ < state.max_literals:
            gen_size = gen_size_

    # prune generalisations of rules with redundant literals
    if is_recursive:
        rec_cons, rec_add_gen = check_recursive_redundancy(settings, tester, prog)
        new_cons.extend(rec_cons)
        add_gen = add_gen and rec_add_gen

    with stats.duration('check_reducible1'):
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
    if not add_spec and not pruned_more_general and settings.datalog and not settings.recursion_enabled and num_neg > 0:
        with stats.duration('check_reducible2'):
            bad_prog = allsatcore_finder.check_neg_reducible(prog)
            if bad_prog:
                add_spec = True
                pruned_more_general = True
                if settings.showcons:
                    print('\t', 'REDUCIBLE_2:', '\t', format_prog(bad_prog))
                new_cons.append((Constraint.SPECIALISATION, bad_prog))

    # BUILD CONSTRAINTS
    if not pruned_more_general:

        if add_spec and not add_redund2:
            new_cons.append((Constraint.SPECIALISATION, prog))

        if not too_few_tp and not add_spec and spec_size and spec_size <= state.max_literals and (is_recursive or has_invention or spec_size <= settings.max_body):
            new_cons.append((Constraint.SPECIALISATION, prog, spec_size))
            seen_hyp_spec[fp + prog_size + mdl].append([prog, tp, fn, tn, fp, prog_size])

        if not add_gen and gen_size and gen_size <= state.max_literals and (settings.recursion_enabled or settings.pi_enabled):
            new_cons.append((Constraint.GENERALISATION, prog, gen_size))
            seen_hyp_gen[fn + prog_size + mdl].append([prog, tp, fn, tn, fp, prog_size])
            # print('seen_hyp_gen', format_prog(prog), fn, prog_size, mdl)

        if add_gen:
            new_cons.append((Constraint.GENERALISATION, prog))

        if add_redund1:
            new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog))

        if add_redund2:
            new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog))

    if not add_spec and not add_gen:
        new_cons.append((Constraint.BANISH, prog))

    add_to_combiner = (not too_few_tp) and (not too_many_fp) and (not is_recursive) and (not has_invention) and tp > prog_size + fp and fp + prog_size < state.best_hypothesis_mdl and (not noisy_subsumed)

    return new_cons, add_to_combiner

# given a program with more than one rule, look for inconsistent subrules/subprograms
def explain_inconsistent(tester, prog):
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

def check_recursive_redundancy(settings, tester, prog):
    new_cons = []
    add_gen = False

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

    return new_cons, add_gen

def build_constraints_previous_hypotheses(score, best_size, num_pos, num_neg, state):
    cons = []

    seen_hyp_spec, seen_hyp_gen = state.seen_hyp_spec, state.seen_hyp_gen

    if seen_hyp_spec is None or seen_hyp_gen is None:
        return cons

    # Prune Specialisations
    for k in [k for k in seen_hyp_spec if k > score + num_pos + best_size]:
        to_delete = []
        for prog, tp, fn, tn, fp, size in seen_hyp_spec[k]:
            mdl = mdl_score(fn, fp, size)
            if score + num_pos + best_size < fp + size + mdl:
                spec_size = score - mdl + num_pos + best_size
                if spec_size <= size:
                    to_delete.append([prog, tp, fn, tn, fp, size])
                # print('SPEC', format_prog(prog), score, best_size, spec_size)
                cons.append((Constraint.SPECIALISATION, prog, spec_size))
        for to_del in to_delete:
            seen_hyp_spec[k].remove(to_del)

    # Prune Generalisations
    for k in [k for k in seen_hyp_gen if k > score + num_neg + best_size]:
        to_delete = []
        for prog, tp, fn, tn, fp, size in seen_hyp_gen[k]:
            mdl = mdl_score(fn, fp, size)
            if score + num_neg + best_size < fn + size + mdl:
                gen_size = score - mdl + num_neg + best_size
                if gen_size <= size:
                    to_delete.append([prog, tp, fn, tn, fp, size])
                # print('GEN', format_prog(prog), score, best_size, gen_size)
                cons.append((Constraint.GENERALISATION, prog, gen_size))
        for to_del in to_delete:
            seen_hyp_gen[k].remove(to_del)

    return cons


# def explain_none_functional(settings, tester, prog):
#     new_cons = []

#     if len(prog) == 1:
#         return new_cons

#     base = []
#     rec = []
#     for rule in prog:
#         if rule_is_recursive(rule):
#             rec.append(rule)
#         else:
#             base.append(rule)

#     pruned_subprog = False
#     for rule in base:
#         subprog = frozenset([rule])
#         if tester.is_non_functional(subprog):
#             new_cons.append((Constraint.GENERALISATION, subprog))
#             pruned_subprog = True

#     if pruned_subprog:
#         return new_cons

#     if len(rec) == 1:
#         return new_cons

#     for r1 in base:
#         for r2 in rec:
#             subprog = frozenset([r1,r2])
#             if tester.is_non_functional(subprog):
#                 new_cons.append((Constraint.GENERALISATION, subprog))

#     return new_cons