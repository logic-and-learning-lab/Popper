import time
from collections import defaultdict
from bitarray.util import subset, any_and, ones
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog
from . tester import Tester
from . bkcons import deduce_bk_cons, deduce_recalls, deduce_type_cons, deduce_non_singletons
from . combine import Combiner


def load_solver(settings, tester, coverage_pos, coverage_neg, prog_lookup):
    if settings.debug:
        settings.logger.debug(f'Load exact solver: {settings.solver}')

    if settings.solver not in ['rc2', 'uwr', 'wmaxcdcl']:
        print('INVALID SOLVER')
        exit()

    settings.maxsat_timeout = None
    settings.stats.maxsat_calls = 0

    if settings.solver == 'rc2':
        settings.exact_maxsat_solver = 'rc2'
        settings.old_format = False
    elif settings.solver == 'uwr':
        settings.exact_maxsat_solver='uwrmaxsat'
        settings.exact_maxsat_solver_params="-v0 -no-sat -no-bin -m -bm"
        settings.old_format = False
    else:
        settings.exact_maxsat_solver='wmaxcdcl'
        settings.exact_maxsat_solver_params=""
        settings.old_format = True

    if settings.noisy:
        settings.lex = False
    else:
        settings.lex = True
        settings.best_mdl = False
        settings.lex_via_weights = False

    if settings.debug:
        settings.logger.debug(f'Load anytime solver:{settings.anytime_solver}')

    if settings.anytime_solver in ['wmaxcdcl', 'nuwls']:
        settings.maxsat_timeout = settings.anytime_timeout
        if settings.anytime_solver == 'wmaxcdcl':
            settings.anytime_maxsat_solver = 'wmaxcdcl'
            settings.anytime_maxsat_solver_params = ""
            settings.anytime_maxsat_solver_signal = 10
            settings.old_format = True
        elif settings.anytime_solver == 'nuwls':
            settings.anytime_maxsat_solver = 'NuWLS-c'
            settings.anytime_maxsat_solver_params = ""
            settings.anytime_maxsat_solver_signal = 15
        else:
            print('INVALID ANYTIME SOLVER')
            exit()

    # TODO: temporary config, need to be modified
    settings.last_combine_stage = False

    return Combiner(settings, tester, coverage_pos, coverage_neg, prog_lookup)

class Popper():
    def __init__(self, settings, tester):
        self.settings = settings
        self.tester = tester
        self.pruned2 = set()

        # AC: SELF.SEEN_PROG CAN GROW VERY BIG
        self.seen_prog = set()
        self.unsat = set()
        self.tmp = {}
        self.seen_allsat = set()

    def run(self, bkcons):

        settings, tester = self.settings, self.tester
        num_pos, num_neg = self.num_pos, self.num_neg = tester.num_pos, tester.num_neg

        uncovered = ones(self.num_pos)

        if settings.noisy:
            min_score = None
            saved_scores = dict()
            settings.best_prog_score = 0, num_pos, num_neg, 0, 0
            settings.best_mdl = num_pos
            # save hypotheses for which we pruned spec / gen from a certain size only
            # once we update the best mdl score, we can prune spec / gen from a better size for some of these
            self.seen_hyp_spec, self.seen_hyp_gen = defaultdict(list), defaultdict(list)
            max_size = min((1 + settings.max_body) * settings.max_rules, num_pos)
        else:
            max_size = (1 + settings.max_body) * settings.max_rules

        settings.max_size = max_size

        # generator that builds programs
        # AC: all very hacky until the refactoring is complete
        with settings.stats.duration('init'):
            if settings.single_solve:
                from . gen2 import Generator
            elif settings.max_rules == 2 and not settings.pi_enabled:
                from . gen3 import Generator
            else:
                from . generate import Generator
            generator = self.generator = Generator(settings, bkcons)

        # track the success sets of tested hypotheses

        # pos_covered_bit_array -> prog_size
        # it only maintains success sets for programs where fp = 0
        success_sets = self.success_sets = {}
        success_sets_aux = self.success_sets_aux = {}

        # (pos_covered_bit_array, neg_covered_bitarray) -> prog_size
        success_sets_noise = {}

        # pos_covered_bit_array -> prog_size+prog_size2
        # it only maintains success sets for pairs of programs where fp = 0
        paired_success_sets = self.paired_success_sets = defaultdict(set)

        covered_by_pos = defaultdict(set)
        covered_by_neg = defaultdict(set)

        # program_hash -> coverage (bit_arrary)
        coverage_pos = {}
        coverage_neg = {}

        cached_prog_size = self.cached_prog_size = {}

        prog_lookup = {}

        combiner = load_solver(settings, tester, coverage_pos, coverage_neg, prog_lookup)

        scores = self.scores = {}

        # could_prune_later = self.could_prune_later = []
        # could_prune_later_rec = self.could_prune_later_rec = []

        to_combine = set()

        self.min_size = None

        last_size = None

        min_coverage = self.settings.min_coverage = 1

        for size in range(2, max_size+1):
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

                # new cons to add to the solver
                new_cons = []

                # generate a program
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

                with settings.stats.duration('test'):
                    if settings.noisy:
                        pos_covered, neg_covered, inconsistent, skipped, skip_early_neg = tester.test_prog_noisy(prog, prog_size)
                    else:
                        pos_covered, inconsistent = tester.test_prog(prog)
                        # @CH: can you explain these?
                        skipped, skip_early_neg = False, False

                tp = pos_covered.count(1)
                fn = num_pos-tp

                # if non-separable program covers all examples, stop
                if not inconsistent and tp == num_pos and not skipped:
                    # if not settings.functional_test or not tester.is_non_functional(prog):
                    settings.solution = prog
                    settings.best_prog_score = num_pos, 0, num_neg, 0, prog_size
                    settings.best_mdl = prog_size
                    return

                if settings.noisy and not skipped:
                    fp = neg_covered.count(1)
                    tn = num_neg-fp
                    score = tp, fn, tn, fp, prog_size
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
                        combiner.best_cost = mdl
                        settings.best_prog_score = score
                        settings.solution = prog
                        settings.best_mdl = mdl
                        settings.max_literals = mdl-1
                        settings.print_incomplete_solution2(prog, tp, fn, tn, fp, prog_size)
                        new_cons.extend(self.build_constraints_previous_hypotheses(mdl, prog_size))

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

                # if the program does not cover any positive examples, check whether it is has an unsat core
                if not has_invention:
                    if tp < min_coverage or (settings.noisy and tp <= prog_size):
                        with settings.stats.duration('find mucs'):
                            cons_ = tuple(self.explain_incomplete(prog))
                            new_cons.extend(cons_)
                            pruned_more_general = len(cons_) > 0

                if tp > 0 and success_sets and (not settings.noisy or (settings.noisy and fp==0)):
                    with settings.stats.duration('check subsumed and covers_too_few'):
                        subsumed = pos_covered in success_sets or any(subset(pos_covered, xs) for xs in success_sets)
                        subsumed_by_two = not subsumed and self.check_subsumed_by_two(pos_covered, prog_size)
                        covers_too_few = not subsumed and not subsumed_by_two and not settings.noisy and self.check_covers_too_few(prog_size, pos_covered)

                    if subsumed or subsumed_by_two or covers_too_few:
                        add_spec = True
                        noisy_subsumed = True

                if not settings.noisy and not has_invention and not is_recursive and (subsumed or subsumed_by_two or covers_too_few):

                    # TODO: FIND MOST GENERAL SUBSUMED RECURSIVE PROGRAM
                    # xs = self.subsumed_or_covers_too_few2(prog, check_coverage=False, check_subsumed=True)
                    # if xs:
                    # pruned_more_general
                    #     for x in xs:
                    #         print('')
                    #         for rule in x:
                    #             # print(rule)
                    #             print('\t', 'moo', format_rule(rule))
                    #         new_cons.append((Constraint.SPECIALISATION, [functional_rename_vars(rule) for rule in x]))


                    # If a program is subsumed or dioesn't cover enough examples, we search for the most general subprogram that also is also subsumed or doesn't cover enough examples
                    # only applies to non-recursive and non-PI programs
                    subsumed_progs = []
                    with settings.stats.duration('find most general subsumed/covers_too_few'):
                        subsumed_progs = self.subsumed_or_covers_too_few(prog, seen=set())
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

                if not settings.noisy and not pruned_more_general:
                    if inconsistent:
                        # if inconsistent, prune generalisations
                        add_gen = True
                        if is_recursive:
                            combiner.add_inconsistent(prog)
                            cons_ = frozenset(self.explain_inconsistent(prog))
                            new_cons.extend(cons_)
                            pruned_sub_inconsistent = len(cons_) > 0
                    else:
                        # messy thing so the combiner can look at something
                        neg_covered = frozenset()

                    # if consistent and partially complete, test whether functional
                    if not inconsistent and settings.functional_test and tp > 0 and not pruned_more_general:
                        if tester.is_non_functional(prog):
                            # if not functional, rule out generalisations and set as inconsistent
                            add_gen = True
                            # V.IMPORTANT: do not prune specialisations!
                            add_spec = False
                            inconsistent = True

                            # check whether any subprograms are non-functional
                            with settings.stats.duration('explain_none_functional'):
                                cons_ = explain_none_functional(settings, tester, prog)
                                new_cons.extend(cons_)

                if settings.noisy:
                    # if a program of size k covers less than k positive examples, we can prune its specialisations
                    # otherwise no useful mdl induction has taken place
                    if tp <= prog_size:
                        add_spec = True

                    # we can prune specialisations with size greater than prog_size+fp or tp
                    # only prune if the specialisation bounds are smaller than existing bounds
                    if not skipped:
                        spec_size_ = min([tp, fp + prog_size])
                        if spec_size_ <= prog_size:
                            add_spec = True
                        elif len(prog) == 1 and spec_size_ < settings.max_body + 1 and spec_size_ < settings.max_literals:
                            spec_size = spec_size_
                        elif len(prog) > 1 and spec_size_ < settings.max_literals:
                            spec_size = spec_size_

                    if skipped or skip_early_neg:
                        # only prune if the generalisation bounds are smaller than existing bounds
                        gen_size_ = fn + prog_size
                        if gen_size_ <= prog_size:
                            add_gen = True
                        if gen_size_ < settings.max_literals:
                            gen_size = gen_size_
                    else:
                        # only prune if the generalisation bounds are smaller than existing bounds
                        gen_size_ = min([fn + prog_size, num_pos-fp, settings.best_mdl - mdl + num_pos + prog_size])
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

                # remove generalisations of programs with redundant rules
                # if is_recursive and len(prog) > 2 and tester.has_redundant_rule(prog):
                #     with settings.stats.duration('has_redundant_rule'):
                #         add_gen = True
                #         r1, r2 = tester.find_redundant_rules(prog)
                #         if settings.showcons:
                #             print('\t','r1',format_rule(order_rule(r1)))
                #             print('\t','r2',format_rule(order_rule(r2)))
                #         new_cons.append((Constraint.GENERALISATION, [r1,r2], None, None))

                with settings.stats.duration('check_reducible1'):
                    xs, pruned_smaller = self.check_redundant_literal(prog)
                    if pruned_smaller:
                        pruned_more_general = True
                    if xs:
                        add_spec = True
                        for x in xs:
                            if settings.showcons:
                                print('\t', 'REDUCIBLE_1:', '\t', ','.join(format_literal(literal) for literal in x))
                            new_cons.append((Constraint.UNSAT, x))

                if not add_spec and not pruned_more_general and settings.datalog and not settings.recursion_enabled:
                    with settings.stats.duration('check_reducible2'):
                        bad_prog = self.check_neg_reducible(prog)
                        if bad_prog:
                            add_spec = True
                            pruned_more_general = True
                            if settings.showcons:
                                print('\t', 'REDUCIBLE_2:', '\t', format_prog(bad_prog))
                            new_cons.append((Constraint.SPECIALISATION, bad_prog))

                # must cover minimum number of examples
                if not add_spec and not pruned_more_general:
                    if tp < min_coverage:
                        add_spec = True

                add_to_combiner = False
                if settings.noisy and not skipped and not skip_early_neg and not is_recursive and not has_invention and tp > prog_size+fp and fp+prog_size < settings.best_mdl and not noisy_subsumed:

                    local_delete = set()
                    ignore_this_prog = (pos_covered, neg_covered) in success_sets_noise


                    # if pos_covered is a subset and neg_covered is a superset of a previously seen program (which must be of equal size or smaller) then we can ignore this program
                    if not ignore_this_prog:
                        # find all progs where pos_covered is a subset of pos_covered_ of the other prog
                        # s_pos_ = set.intersection(*(covered_by_pos[ex] for ex in pos_covered))
                        s_pos = set.intersection(*(covered_by_pos[ex] for ex, ex_covered_ in enumerate(pos_covered) if ex_covered_ == 1))
                        # now check whether neg_covered is a superset of the other program
                        for prog1 in s_pos:
                            n1 = coverage_neg[prog1]
                            # if neg_covered(old) ⊆ new_covered(new) then ignore new
                            if subset(n1, neg_covered):
                                ignore_this_prog = True
                                # print('skip1')
                                break

                    if not ignore_this_prog and (inconsistent or fp>0):
                        # neg_covered is a subset of all programs in s_neg
                        s_neg = set.intersection(*(covered_by_neg[ex] for ex, ex_covered_ in enumerate(neg_covered) if ex_covered_ == 1))
                        # if neg_covered(new) ⊆ neg_covered(old)
                        for prog1 in s_neg:
                            # if pos_covered(old) ⊆ pos_covered(new)
                            if subset(coverage_pos[prog1], pos_covered):
                                size1, tp1, fp1 = scores[prog1]
                                if size1 == prog_size:
                                    # ignore OLD
                                    # print('ignore old')
                                    local_delete.add(prog1)
                                    continue

                                if fp == fp1 and (tp-prog_size) < (tp1-size1):
                                    # NEW tp:50 fp:1 size:6 memberofdomainregion(V0,V1):- synsetdomaintopicof(V2,V3),synsetdomaintopicof(V1,V3),hypernym(V2,V4),membermeronym(V0,V5),synsetdomaintopicof(V2,V4).
                                    # OLD tp:49 fp:1 size:4 memberofdomainregion(V0,V1):- synsetdomaintopicof(V1,V3),instancehypernym(V2,V3),membermeronym(V0,V4).
                                    # print('skip2')
                                    ignore_this_prog = True
                                    break

                                if tp == tp1 and (fp+prog_size) >= (fp1+size1):
                                    # NEW tp:10 fp:1 mdl:350 less_toxic(V0,V1):- ring_subst_3(V1,V4),ring_substitutions(V1,V3),alk_groups(V0,V3),x_subst(V0,V2,V5).
                                    # OLD tp:10 fp:2 mdl:350 less_toxic(V0,V1):- ring_substitutions(V0,V4),x_subst(V0,V3,V2),ring_subst_3(V1,V5).
                                    # print('skip3')
                                    ignore_this_prog = True
                                    break

                                if (tp-fp-prog_size) <= (tp1-fp1-size1):
                                    # NEW tp:12 fp:3 size:7 less_toxic(V0,V1):- gt(V2,V5),gt(V2,V3),ring_subst_2(V1,V4),ring_substitutions(V0,V3),alk_groups(V0,V2),ring_substitutions(V1,V5).
                                    # OLD tp:11 fp:4 size:3 less_toxic(V0,V1):- ring_subst_2(V1,V3),r_subst_3(V0,V2).
                                    # print('skip4')
                                    ignore_this_prog = True
                                    break

                    # backtrack prune for consistent programs
                    if not inconsistent:
                        # new is consistent
                        # if pos_covered(old) ⊆ pos_covered(new) and size(old) >= size(new) then ignore old
                        not_covered = tester.pos_examples_ ^ pos_covered
                        progs_not_subsumed = set.union(*(covered_by_pos[ex] for ex, ex_covered_ in enumerate(not_covered) if ex_covered_ == 1))
                        all_progs = set.union(*(covered_by_pos[ex] for ex, ex_covered_ in enumerate(tester.pos_examples_) if ex_covered_ == 1))
                        s_pos = all_progs.difference(progs_not_subsumed)
                        for prog1 in s_pos:
                            size1, tp1, fp1 = scores[prog1]
                            if size1 >= prog_size:
                                local_delete.add(prog1)

                    for k in local_delete:
                        assert(k in combiner.saved_progs|to_combine)
                        if k in combiner.saved_progs:
                            combiner.saved_progs.remove(k)
                        elif k in to_combine:
                            to_combine.remove(k)

                        k_pos, k_neg = coverage_pos[k], coverage_neg[k]
                        del success_sets_noise[(k_pos, k_neg)]
                        for ex, ex_covered_ in enumerate(k_pos):
                            if ex_covered_ == 1:
                                covered_by_pos[ex].remove(k)
                        for ex, ex_covered_ in enumerate(k_neg):
                            if ex_covered_ == 1:
                                covered_by_neg[ex].remove(k)
                        del coverage_pos[k]
                        del coverage_neg[k]
                        del scores[k]
                        del cached_prog_size[k]
                        del prog_lookup[k]

                    if not ignore_this_prog:
                        success_sets_noise[(pos_covered, neg_covered)] = prog, prog_size, fn, fp, tp
                        add_to_combiner = True
                        k = hash(prog)

                        for ex, x in enumerate(pos_covered):
                            if x == 1:
                                covered_by_pos[ex].add(k)
                        for ex, x in enumerate(neg_covered):
                            if x == 1:
                                covered_by_neg[ex].add(k)

                        coverage_pos[k] = pos_covered
                        coverage_neg[k] = neg_covered
                        cached_prog_size[k] = prog_size
                        scores[k] = prog_size, tp, fp
                        prog_lookup[k] = prog

                        if fp == 0:
                            success_sets[pos_covered] = prog_size
                            for p, s in success_sets.items():
                                if p == pos_covered:
                                    continue
                                # print(p, pos_covered, p|pos_covered)
                                paired_success_sets[s+prog_size].add(p|pos_covered)

                elif not settings.noisy:
                    # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
                    if not inconsistent and not subsumed and not add_gen and tp > 0 and not pruned_more_general:
                        add_to_combiner = True

                        if not settings.recursion_enabled:
                            to_delete = []
                            for pos_covered2, prog_size2 in success_sets.items():
                                if prog_size > prog_size2:
                                    continue
                                if subset(pos_covered2, pos_covered):
                                    to_delete.append(success_sets_aux[pos_covered2])

                            for prog2_hash in to_delete:
                                pos_covered2 = coverage_pos[prog2_hash]
                                if prog2_hash in to_combine:
                                    to_combine.remove(prog2_hash)
                                elif prog2_hash in combiner.saved_progs:
                                    combiner.saved_progs.remove(prog2_hash)
                                else:
                                    assert(False)
                                del success_sets[pos_covered2]
                                del success_sets_aux[pos_covered2]
                                del coverage_pos[prog2_hash]
                                del coverage_neg[prog2_hash]
                                del prog_lookup[prog2_hash]
                                # AC: SOMEHOW DELETE FROM PAIRED_SUCCESS_SETS

                        k = hash(prog)
                        success_sets[pos_covered] = prog_size
                        success_sets_aux[pos_covered] = k
                        coverage_pos[k] = pos_covered
                        coverage_neg[k] = neg_covered
                        prog_lookup[k] = prog

                        for p, s in success_sets.items():
                            if p == pos_covered:
                                continue
                            paired_success_sets[s+prog_size].add(p|pos_covered)

                        if self.min_size is None:
                            self.min_size = prog_size

                if add_to_combiner:
                    to_combine.add(hash(prog))

                call_combine = len(to_combine) > 0
                call_combine = call_combine and (settings.noisy or settings.solution_found)
                call_combine = call_combine and (len(to_combine) >= settings.batch_size or size_change)

                if add_to_combiner and not settings.noisy and not settings.solution_found and not settings.recursion_enabled:

                    if any_and(uncovered, pos_covered):

                        if settings.solution:
                            settings.solution = settings.solution | prog
                        else:
                            settings.solution = prog
                        uncovered = uncovered & ~pos_covered
                        tp = num_pos- uncovered.count(1)
                        fn = uncovered.count(1)
                        tn = num_neg
                        fp = 0
                        hypothesis_size = calc_prog_size(settings.solution)
                        settings.best_prog_score = tp, fn, tn, fp, hypothesis_size
                        settings.print_incomplete_solution2(settings.solution, tp, fn, tn, fp, hypothesis_size)

                        if not uncovered.any():
                            settings.solution_found = True
                            settings.max_literals = hypothesis_size-1
                            min_coverage = settings.min_coverage = 2
                            # if we have a solution with two rules then a new rule must entail all the examples
                            if min_coverage != num_pos and len(settings.solution) == 2:
                                min_coverage = settings.min_coverage = num_pos

                            # AC: sometimes adding these size constraints can take longer
                            for i in range(settings.max_literals+1, max_size+1):
                                generator.prune_size(i)

                        call_combine = not uncovered.any()

                if call_combine:
                    if settings.noisy:
                        self.filter_combine_programs(combiner, to_combine)

                    # COMBINE
                    # print('call combiner')
                    with settings.stats.duration('combine'):
                        is_new_solution_found = combiner.update_best_prog(to_combine)

                    to_combine=set()

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
                        # print('here???')

                        settings.print_incomplete_solution2(new_hypothesis, tp, fn, tn, fp, hypothesis_size)

                        if settings.noisy and best_score < settings.best_mdl:
                            settings.best_mdl = best_score
                            settings.max_literals = settings.best_mdl - 1
                            new_cons.extend(self.build_constraints_previous_hypotheses(settings.best_mdl, prog_size))
                            if settings.single_solve:
                                # AC: sometimes adding these size constraints can take longer
                                for i in range(best_score, max_size+1):
                                    generator.prune_size(i)
                        # print("HERE!!!", tp, fn, tn, fp)
                        if not settings.noisy and fp == 0 and fn == 0:
                            settings.solution_found = True
                            settings.max_literals = hypothesis_size-1

                            min_coverage = settings.min_coverage = 2
                            # if we have a solution with two rules then a new rule must entail all the examples
                            if min_coverage != num_pos and len(settings.solution) == 2:
                                min_coverage = settings.min_coverage = num_pos

                            # if size >= settings.max_literals and not settings.order_space:
                            if size >= settings.max_literals:
                                print('POOPER')
                                return

                            # AC: sometimes adding these size constraints can take longer
                            for i in range(hypothesis_size, max_size+1):
                                generator.prune_size(i)

                # BUILD CONSTRAINTS
                if add_spec and not pruned_more_general and not add_redund2:
                    new_cons.append((Constraint.SPECIALISATION, prog))

                if not skipped:
                    if settings.noisy and not add_spec and spec_size and not pruned_more_general:
                        if spec_size <= settings.max_literals and ((is_recursive or has_invention or spec_size <= settings.max_body)):
                            new_cons.append((Constraint.SPECIALISATION, prog, spec_size))
                            self.seen_hyp_spec[fp+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size])

                if add_gen and not pruned_sub_inconsistent:
                    if settings.noisy or settings.recursion_enabled or settings.pi_enabled:
                        if not pruned_more_general:
                            new_cons.append((Constraint.GENERALISATION, prog))
                    else:
                        if not add_spec:
                            new_cons.append((Constraint.GENERALISATION, prog))

                if settings.noisy and not add_gen and gen_size and not pruned_sub_inconsistent:
                    if gen_size <= settings.max_literals and (settings.recursion_enabled or settings.pi_enabled) and not pruned_more_general:
                        new_cons.append((Constraint.GENERALISATION, prog, gen_size))
                        self.seen_hyp_gen[fn+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size])

                if add_redund1 and not pruned_more_general:
                    new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog))

                if add_redund2 and not pruned_more_general:
                    new_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, prog))

                if settings.noisy and not add_spec and not add_gen:
                    new_cons.append((Constraint.BANISH, prog))

                # CONSTRAIN
                with settings.stats.duration('constrain'):
                    generator.constrain(new_cons)

            # if not pi_or_rec:
            if to_combine:
                # print('LAST CALL')
                settings.last_combine_stage = True
                # TODO: AWFUL: FIX REFACOTRING
                # COMBINE
                with settings.stats.duration('combine'):
                    is_new_solution_found = combiner.update_best_prog(to_combine)
                to_combine=set()

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
                        min_coverage = settings.min_coverage = 2
                        # if we have a solution with two rules then a new rule must entail all the examples
                        if min_coverage != num_pos and len(settings.solution) == 2:
                            min_coverage = settings.min_coverage = num_pos

                        # if size >= settings.max_literals and not settings.order_space:
                        if size >= settings.max_literals:
                            assert(False)
            if settings.single_solve:
                break
        assert(len(to_combine) == 0)

    def check_redundant_literal(self, prog):
        tester, settings = self.tester, self.settings

        if len(prog) > 1:
            return [], False

        rule = list(prog)[0]
        head, body = rule

        if len(body) == 1:
            return [], False

        body = tuple(body)
        out = []
        allsat1, allsat2 = set(), set()

        # loop through each body literal
        for i in range(len(body)):

            # potential redundant literal
            redundant_literal = body[i]

            new_body = frozenset(body) - {redundant_literal}

            # do it
            out.extend(self.check_redundant_literal_aux(new_body, redundant_literal, allsat1, allsat2, depth=0))

        pruned_smaller = False
        new_out = []
        for rule, smaller in out:
            pruned_smaller = pruned_smaller or smaller
            new_out.append(rule)
        return new_out, pruned_smaller

    def check_redundant_literal_aux(self, body, literal, allsat1, not_all_sat1, depth):
        out = []
        prog = frozenset([(None, body)])

        if len(body) == 0:
            return out


        # print('\tA \t\t\t',format_prog(prog), '\t\t', format_literal(literal))
        # prog_key = (prog, literal)
        prog_key = (body, literal)
        if hash(prog_key) in self.seen_allsat:
            return out

        h_, b = remap_variables((None, body | frozenset([literal])))
        if hash(b) in self.seen_allsat:
            return out

        self.seen_allsat.add(hash(prog_key))
        self.seen_allsat.add(hash(b))

        body_vars = set()
        for atom in body:
            body_vars.update(atom.arguments)

        if not all(x in body_vars for x in literal.arguments):
            return out

        if not connected(body | frozenset([literal])):
            for new_body in combinations(body, len(body)-1):
                new_body = frozenset(new_body)
                out.extend(self.check_redundant_literal_aux(new_body, literal, allsat1, not_all_sat1, depth+1))
            return out

        if any(body.issubset(seen_body) for seen_body in not_all_sat1):
            return out

        if any(seen_body.issubset(body) for seen_body in allsat1):
            return out

        if not self.has_valid_directions((None, body)):
            return out

        if not self.tester.is_literal_redundant(body, literal):
            not_all_sat1.add(body)
            return out

        # print('REDUNDANT', format_rule((None, body)), '---->', format_literal(literal))

        allsat1.add(body)

        for new_body in combinations(body, len(body)-1):
            new_body = frozenset(new_body)
            out.extend(self.check_redundant_literal_aux(new_body, literal, allsat1, not_all_sat1, depth+1))

        if len(out) > 0:
            return out

        to_prune = frozenset(body | {literal})

        return [(to_prune, depth > 0)]

    def check_neg_reducible(self, prog):
        tester = self.tester
        settings = self.settings

        for rule in prog:
            head, body = rule

            head_vars = set()
            for arg in head.arguments:
                head_vars.add(arg)

            for literal in body:
                literal_p, literal_args = literal
                literal_args = set(literal_args)

                if len(body) == 1:
                    # AC: SPECIAL CASE FOR A SINGLE BODY LITERAL IMPLIED BY THE HEAD
                    if settings.non_datalog_flag and literal_args.issubset(head_vars) and tester.diff_subs_single(literal):
                        bad_rule = (head, frozenset([literal]))
                        bad_prog = frozenset([bad_rule])
                        return bad_prog
                    continue

                body_ = frozenset(body) - {literal}
                body_vars = set()

                for p, args in body_:
                    for arg in args:
                        body_vars.add(arg)

                if not literal_args.issubset(body_vars | head_vars):
                    continue

                if not settings.non_datalog_flag and any(x not in body_vars for x in head_vars):
                    continue

                new_rule = (head, body_)
                new_prog = frozenset([new_rule])

                if not self.has_valid_directions(new_rule):
                    continue

                if tester.is_neg_reducible(body_, literal):
                    return frozenset([rule])

    def filter_combine_programs(self, combiner, to_combine):

        # assert(False)
        xs = combiner.saved_progs | to_combine
        min_size = min(self.cached_prog_size[prog] for prog in xs)
        must_beat = self.settings.best_mdl-min_size

        to_delete = set()
        # FILTER COMBINE PROGRAMS
        for prog_hash in xs:
            size, tp, fp = self.scores[prog_hash]
            if fp + size >= must_beat:
                to_delete.add(prog_hash)

        for prog_hash in to_delete:
            # AC: DELETE FROM SUCCESS_SETS_NOISE
            if prog_hash in combiner.saved_progs:
                combiner.saved_progs.remove(prog_hash)
            else:
                to_combine.remove(prog_hash)

    def check_subsumed_by_two(self, pos_covered, prog_size):
        paired_success_sets = self.paired_success_sets
        for i in range(2, prog_size+2):
            if pos_covered in paired_success_sets[i]:
                return True
            for x in paired_success_sets[i]:
                if subset(pos_covered, x):
                    return True
        return False

    def check_subsumed_by_two_v2(self, prog_size, prog2_size, pos_covered, pos_covered2):
        space = prog2_size-prog_size

        if space < self.min_size:
            return False
        uncovered = pos_covered2 & ~pos_covered

        for xs, size in self.success_sets.items():
            if size > space:
                continue
            if subset(xs, uncovered):
                return True
        return False

    def check_covers_too_few(self, prog_size, pos_covered):

        tmp = self.tmp
        k1 = hash((prog_size, pos_covered))
        if k1 in self.tmp:
            v = self.tmp[k1]
            if v:
                return True

        k2 = hash((prog_size, pos_covered, self.settings.max_literals, self.settings.search_depth))
        if k2 in tmp:
            return tmp[k2]

        v = self.check_covers_too_few_(prog_size, pos_covered)

        tmp[k1] = v
        tmp[k2] = v
        return v

    def check_covers_too_few_(self, prog_size, pos_covered):
        num_pos = self.num_pos

        len_pos_covered = pos_covered.count(1)
        if len_pos_covered == num_pos:
            return False

        min_size = self.min_size
        # assert(min_size)

        max_literals = self.settings.max_literals

        # MAX RULES = 1
        if ((prog_size + min_size) > max_literals):
            if len_pos_covered != num_pos:
                return True

        # MAX RULES = 2
        elif ((prog_size + (min_size*2)) > max_literals):
            space_remaining = max_literals-prog_size
            if space_remaining > self.settings.search_depth:
                return False

            uncovered = self.tester.pos_examples_ & ~pos_covered
            for pos_covered2, size2 in self.success_sets.items():
                if size2 > space_remaining:
                    continue
                if subset(uncovered, pos_covered2):
                    return False
            # print('MR2_')
            return True

        # # MAX RULES = 3
        elif ((prog_size + (min_size*3)) > max_literals):
            space_remaining = max_literals-prog_size

            if space_remaining-min_size > self.settings.search_depth:
                return False

            # uncovered = self.tester.pos_examples-pos_covered
            uncovered = self.tester.pos_examples_ & ~pos_covered
            success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.success_sets.items()), key=lambda x: x[1])
            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break
                if subset(uncovered, pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                uncovered2 = uncovered & ~pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    if subset(uncovered2, pos_covered3):
                        return False
            return True

        # # MAX RULES = 4
        elif prog_size + (min_size*4) > max_literals:
            space_remaining = max_literals-prog_size
            space_remaining -= (min_size*2)

            if space_remaining > self.settings.search_depth:
                return False

            missing = self.tester.pos_examples_ & ~pos_covered

            success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.success_sets.items()), key=lambda x: x[1])
            space_remaining = max_literals-prog_size

            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break
                if subset(missing, pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                missing2 = missing & ~pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    # if pos_covered3.isdisjoint(missing2):
                        # continue
                    if subset(missing2, pos_covered3):
                        return False
                    space_remaining__ = space_remaining_-size3
                    if space_remaining__ < min_size:
                        continue
                    missing3 = missing2 & ~pos_covered3
                    for k in range(j+1, n):
                        pos_covered4, size4 = success_sets[k]
                        if size4 > space_remaining__:
                            break
                        if subset(missing3, pos_covered4):
                            return False
            return True
        # elif ((prog_size + (min_size*5)) > max_literals):
            # print('MAX RULES IS 5!')

        return False


    # find unsat cores
    def explain_incomplete(self, prog):

        # print('')
        # print('')
        # print('EXPLAIN_INCOMPLETE')
        # print(format_prog(prog))

        settings, tester = self.settings, self.tester
        unsat_cores = self.explain_totally_incomplete(prog)

        for subprog, unsat_body in unsat_cores:

            if settings.showcons:
                if len(subprog) > 1:
                    print('\n')
                for i, rule in enumerate(subprog):
                    # print('\t', format_rule(rule), '\t', f'unsat')
                    print('\t', f'UNSAT:', '\t', format_rule(rule))

            if unsat_body:
                _, body = list(subprog)[0]
                yield (Constraint.UNSAT, body)
                continue

            if not (settings.recursion_enabled or settings.pi_enabled):
                yield (Constraint.SPECIALISATION, [remap_variables(rule) for rule in subprog])
                continue

            if len(subprog) == 1:
                yield (Constraint.REDUNDANCY_CONSTRAINT1, [remap_variables(rule) for rule in subprog])

            yield (Constraint.REDUNDANCY_CONSTRAINT2, [remap_variables(rule) for rule in subprog])

    # given a program with more than one rule, look for inconsistent subrules/subprograms
    def explain_inconsistent(self, prog):
        base = []
        rec = []

        pruned_base = False

        for rule in prog:
            if rule_is_recursive(rule):
                rec.append(rule)
                continue

            base.append(rule)
            # test whether base case is inconsistent
            subprog = frozenset([rule])
            if self.tester.test_prog_inconsistent(subprog):
                pruned_base = True
                yield (Constraint.GENERALISATION, subprog)

        if pruned_base or len(rec) == 1:
            return

        for r1 in base:
            for r2 in rec:
                subprog = frozenset([r1, r2])
                if self.tester.test_prog_inconsistent(subprog):
                    yield (Constraint.GENERALISATION, subprog)

    def build_constraints_previous_hypotheses(self, score, best_size):
        generator, num_pos, num_neg = self.generator, self.num_pos, self.num_neg,
        seen_hyp_spec, seen_hyp_gen = self.seen_hyp_spec, self.seen_hyp_gen
        cons = []
        # print(f"new best score {score}")
        for k in [k for k in seen_hyp_spec if k > score+num_pos+best_size]:
            to_delete = []
            for prog, tp, fn, tn, fp, size in seen_hyp_spec[k]:
                # mdl = mdl_score(tuple((tp, fn, tn, fp, size)))
                mdl = mdl_score(fn, fp, size)
                if score+num_pos+best_size < fp+size+mdl:
                    spec_size = score-mdl+num_pos+best_size
                    if spec_size <= size:
                        to_delete.append([prog, tp, fn, tn, fp, size])
                    # _, con = generator.build_specialisation_constraint(prog, rule_ordering, spec_size=spec_size)
                    # cons.add(con)
                    cons.append((Constraint.SPECIALISATION, prog, spec_size))
                    # print('SPEC', format_prog(prog))
            for to_del in to_delete:
                seen_hyp_spec[k].remove(to_del)
        for k in [k for k in seen_hyp_gen if k > score + num_neg + best_size]:
            to_delete = []
            for prog, tp, fn, tn, fp, size in seen_hyp_gen[k]:
                # mdl = mdl_score(tuple((tp, fn, tn, fp, size)))
                mdl = mdl_score(fn, fp, size)
                if score + num_neg + best_size < fn + size + mdl:
                    gen_size = score - mdl + num_neg + best_size
                    if gen_size <= size:
                        to_delete.append([prog, tp, fn, tn, fp, size])
                    # _, con = generator.build_generalisation_constraint(prog, rule_ordering, gen_size=gen_size)
                    # cons.add(con)
                    cons.append((Constraint.GENERALISATION, prog, gen_size))
                    # print('GEN', format_prog(prog))
            for to_del in to_delete:
                seen_hyp_gen[k].remove(to_del)
        return cons

    def subsumed_or_covers_too_few(self, prog, seen=set()):
        tester, success_sets, settings = self.tester, self.success_sets, self.settings
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
            k1 = frozenset(new_body)
            if k1 in seen:
                continue
            seen.add(k1)

            new_rule = (head, new_body)
            new_prog = frozenset({new_rule})

            # ensure at least one head variable is in the body
            if not settings.non_datalog_flag and not any(x in head_vars for literal in new_body for x in literal.arguments):
                continue

            # check whether we have pruned any subset (HORRIBLE CODE)
            if any(hash(frozenset(x)) in self.pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if not self.has_valid_directions(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if tester.has_redundant_literal(new_prog):
                xs = self.subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = tester.get_pos_covered(new_prog)

            # with self.settings.stats.duration('old'):
            subsumed = sub_prog_pos_covered in success_sets or any(subset(sub_prog_pos_covered, xs) for xs in success_sets)
            subsumed_by_two = not subsumed and self.check_subsumed_by_two(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and self.check_covers_too_few(new_prog_size, sub_prog_pos_covered)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, seen)
            if len(xs) > 0:
                out.update(xs)
                continue

            # for each pruned program, add the variants to the list of pruned programs
            # doing so reduces the number of pointless checks
            for x in self.find_variants(remap_variables(new_rule)):
                self.pruned2.add(hash(x))

            if subsumed:
                out.add((new_prog, ('SUBSUMED (GENERALISATION)')))
            elif subsumed_by_two:
                out.add((new_prog, ('SUBSUMED BY TWO (GENERALISATION)')))
            elif covers_too_few:
                out.add((new_prog, ('COVERS TOO FEW (GENERALISATION)')))
            else:
                assert(False)
        return out

    def find_variants(self, rule):
        head, body = rule
        _head_pred, head_args = head
        head_arity = len(head_args)
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head_arity)
        subset = range(head_arity, self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head_args + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (pred, new_args)
                new_body.append(new_literal)
            yield frozenset(new_body)

    def build_test_prog(self, subprog):
        directions = self.settings.directions
        test_prog = []
        for head, body in subprog:
            if head:
                head_pred, head_args = head
                head_literal = Literal(head_pred, head_args)
            else:
                head_literal = False
            body_literals = set()
            for pred, args in body:
                body_literals.add(Literal(pred, args))
            rule = head_literal, frozenset(body_literals)
            test_prog.append(rule)
        return frozenset(test_prog)

    def explain_totally_incomplete(self, prog):
        return list(self.explain_totally_incomplete_aux2(prog, set(), set()))

    def explain_totally_incomplete_aux2(self, prog, unsat2=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        out = []
        for subprog in generalisations(prog, allow_headless=True, recursive=has_recursion):

            # print('---')
            # for rule in subprog:
            #     print('\t', 'A', format_rule(rule))

            # raw_prog2 = get_raw_prog2(subprog)

            # if raw_prog2 in self.seen_prog:
                # continue

            subprog = frozenset(subprog)
            if hash(subprog) in self.seen_prog:
                continue
            raw_prog = get_raw_prog(subprog)
            if hash(raw_prog) in self.seen_prog:
                continue

            self.seen_prog.add(hash(subprog))
            self.seen_prog.add(hash(raw_prog))
            # self.seen_prog.add(raw_prog2)


            # for rule in subprog:
                # print('\t', 'B', format_rule(rule))

            def should_skip():
                if len(subprog) > 0:
                    return False
                h_, b_ = list(subprog)[0]
                for x in non_empty_powerset(b_):
                    sub_ = [(None, x)]
                    if frozenset(sub_) in self.unsat:
                        return True
                    if get_raw_prog(sub_) in self.unsat:
                        return True
                    sub_ = [(h_, x)]
                    if frozenset(sub_) in self.unsat:
                        return True
                    if get_raw_prog(sub_) in self.unsat:
                        return True
                return False

            if should_skip():
                continue

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if seen_more_general_unsat(subprog, unsat2):
                continue
                # pass

            # if seen_more_general_unsat(subprog, unsat):
            #     print('wtf?2')
            #     continue

            # if seen_more_general_unsat(raw_prog, unsat2):
            #     print('wtf?3')
            #     continue
            #     # pass

            # for rule in subprog:
                # print('\t', 'C', format_rule(rule))


            if not self.prog_is_ok(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
                out.extend(xs)
                continue

            # for rule in subprog:
                # print('\t', 'D', format_rule(rule))

            if self.tester.has_redundant_literal(frozenset(subprog)):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
                out.extend(xs)
                continue


            # if len(subprog) > 2 and self.tester.has_redundant_rule(subprog):
            #     xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat, noisy)
            #     out.extend(xs)
            #     continue

            test_prog = self.build_test_prog(subprog)

            # headless = is_headless(subprog)
            headless = any(head is None for head, body in subprog)


            # print('\t\t\t testing',format_prog(subprog))

            if headless:
                body = list(test_prog)[0][1]
                if self.tester.is_body_sat(body):
                    # sat.add(raw_prog)
                    continue
            else:
                if self.tester.is_sat(test_prog):
                    # print('\t\t\t SAT',format_prog(subprog))
                    # sat.add(raw_prog)
                    continue
                # print('\t\t\t UNSAT',format_prog(subprog))

            unsat.add(raw_prog)
            unsat2.add(subprog)
            self.unsat.add(raw_prog)
            self.unsat.add(subprog)

            xs = self.explain_totally_incomplete_aux2(subprog, unsat2, unsat)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))
        return out


    def has_valid_directions(self, rule):
        if self.settings.has_directions:
            return self.has_valid_directions_(rule)
        return True

    @cache
    def has_valid_directions_(self, rule):
        head, body = rule

        if head:
            head_pred, head_args = head
            head_inputs = self.settings.literal_inputs[(head_pred, head_args)]
            if len(head_inputs) == 0:
                return True

            grounded_variables = head_inputs
            body_literals = set(body)

            while body_literals:
                selected_literal = None
                for literal in body_literals:
                    pred, args = literal
                    literal_inputs = self.settings.literal_inputs[(pred, args)]
                    if not literal_inputs.issubset(grounded_variables):
                        continue
                    if pred != head_pred:
                        # find the first ground non-recursive body literal and stop
                        selected_literal = literal
                        break
                    elif selected_literal == None:
                        # otherwise use the recursive body literal
                        selected_literal = literal

                if selected_literal == None:
                    return False

                pred, args = selected_literal
                selected_literal_outputs = self.settings.literal_outputs[(pred, args)]
                grounded_variables = grounded_variables.union(selected_literal_outputs)
                body_literals = body_literals.difference({selected_literal})
            return True
        else:

            # literal_inputs =
            if all(len(self.settings.literal_inputs[(pred, args)]) == 0 for pred, args in body):
                return True

            body_literals = set(body)
            grounded_variables = set()

            while body_literals:
                selected_literal = None
                for literal in body_literals:
                    pred, args = literal
                    literal_outputs = self.settings.literal_outputs[(pred, args)]
                    if len(literal_outputs) == len(literal.arguments):
                        selected_literal = literal
                        break
                    literal_inputs = self.settings.literal_inputs[(pred, args)]
                    if literal_inputs.issubset(grounded_variables):
                        selected_literal = literal
                        break

                if selected_literal == None:
                    return False

                grounded_variables = grounded_variables.union(selected_literal.arguments)
                body_literals = body_literals.difference({selected_literal})

            return True

    def prog_is_ok(self, prog):
        for rule in prog:
            head, body = rule
            if head and not head_connected(rule):
                return False

            if not head and not connected(body):
                return False

            if not self.has_valid_directions(rule):
                return False

        if len(prog) == 1:
            return True

        # if more than two rules then there must be recursion
        has_recursion = False
        for rule in prog:
            h, b = rule

            if h == None:
                return False

            if rule_is_recursive(rule):
                has_recursion = True
                h, b = rule
                if len(b) == 1:
                    return False

        if not has_recursion:
            return False


        if self.needs_datalog(prog) and not tmp(prog):
            return False

        return True

    def print_incomplete_solution2(self, prog, tp, fn, tn, fp, size):
        self.logger.info('*'*20)
        self.logger.info('New best hypothesis:')
        if self.noisy:
            self.logger.info(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size} mdl:{size+fn+fp}')
        else:
            self.logger.info(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size}')
        for rule in order_prog(prog):
            self.logger.info(format_rule(order_rule(rule)))
        self.logger.info('*'*20)

    def needs_datalog(self, prog):
        if not self.settings.has_directions:
            return False
        for rule in prog:
            rec_outputs = set()
            non_rec_inputs = set()
            head, body = rule
            head_pred, _head_args = head
            for literal in body:
                pred, args = literal
                if pred == head_pred:
                    literal_outputs = self.settings.literal_outputs[(pred, args)]
                    rec_outputs.update(literal_outputs)
                else:
                    # if any(x in xr)
                    literal_inputs = self.settings.literal_inputs[(pred, args)]
                    non_rec_inputs.update(literal_inputs)
            if any(x in rec_outputs for x in non_rec_inputs):
                return True
        return False

def popper(settings, tester, bkcons):
    Popper(settings, tester).run(bkcons)

def get_bk_cons(settings, tester):
    bkcons = []

    with settings.stats.duration('find_pointless_relations'):
        pointless = settings.pointless = tester.find_pointless_relations()

    for p,a in pointless:
        if settings.showcons:
            print('remove pointless relation', p, a)
        settings.body_preds.remove((p,a))

    # if settings.datalog:
    settings.logger.debug(f'Loading recalls')
    with settings.stats.duration('recalls'):
        recalls = deduce_recalls(settings)

    if recalls == None:
        settings.datalog = False
    else:
        settings.datalog = True
        if settings.showcons:
            for x in recalls:
                print('recall', x)
        bkcons.extend(recalls)

    if settings.datalog:

        xs = deduce_non_singletons(settings)
        if settings.showcons:
            for x in xs:
                print('singletons', x)
        bkcons.extend(xs)


        type_cons = tuple(deduce_type_cons(settings))
        if settings.showcons:
            for x in type_cons:
                print('type_con', x)
        bkcons.extend(type_cons)


    if not settings.datalog:
        settings.logger.debug(f'Loading recalls FAILURE')
    else:
        import signal

        def handler(signum, frame):
            raise TimeoutError()

        settings.logger.debug(f'Loading bkcons')
        xs = []
        with settings.stats.duration('bkcons'):
            signal.signal(signal.SIGALRM, handler)
            signal.alarm(settings.bkcons_timeout)
            try:
                xs = deduce_bk_cons(settings, tester)
            except TimeoutError as _exc:
                settings.logger.debug(f'Loading bkcons FAILURE')
            finally:
                signal.alarm(0)
        if settings.showcons:
            for x in sorted(xs):
                print('BKCON', x)
        bkcons.extend(xs)
    return bkcons

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

def generalisations(prog, allow_headless=True, recursive=False):

    if len(prog) == 1:
        rule = list(prog)[0]
        head, body = rule

        if allow_headless:
            if head and len(body) > 0:
                new_rule = (None, body)
                new_prog = [new_rule]
                yield new_prog

        if (recursive and len(body) > 2 and head) or (not recursive and len(body) > 1):
            body = list(body)
            for i in range(len(body)):
                # do not remove recursive literals
                if recursive and body[i].predicate == head.predicate:
                    continue
                new_body = body[:i] + body[i+1:]
                new_rule = (head, frozenset(new_body))
                new_prog = [new_rule]
                yield new_prog

    else:
        prog = list(prog)
        for i in range(len(prog)):
            subrule = prog[i]
            recursive = rule_is_recursive(subrule)
            for new_subrule in generalisations([subrule], allow_headless=False, recursive=recursive):
                new_prog = prog[:i] + new_subrule + prog[i+1:]
                yield new_prog

def tmp(prog):
    for rule in prog:
        head, body = rule
        _head_pred, head_args = head
        body_args = set(x for _pred, args in body for x in args)
        if any(x not in body_args for x in head_args):
            return False
    return True

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

# TODO: THIS CHECK IS NOT COMPLETE
# IT DOES NOT ACCOUNT FOR VARIABLE RENAMING
# R1 = (None, frozenset({('c3', ('A',)), ('c2', ('A',))}))
# R2 = (None, frozenset({('c3', ('B',)), ('c2', ('B',), true_value(A,B))}))
def rule_subsumes(r1, r2):
    # r1 subsumes r2 if r1 is a subset of r2
    h1, b1 = r1
    h2, b2 = r2
    if h1 != None and h2 == None:
        return False
    return b1.issubset(b2)

# P1 subsumes P2 if for every rule R2 in P2 there is a rule R1 in P1 such that R1 subsumes R2
def theory_subsumes(prog1, prog2):
    return all(any(rule_subsumes(r1, r2) for r1 in prog1) for r2 in prog2)

def seen_more_general_unsat(prog, unsat):
    return any(theory_subsumes(seen, prog) for seen in unsat)

def head_connected(rule):
    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    while body_literals:
        connected = []
        for literal in body_literals:
            if any (x in head_connected_vars for x in literal.arguments):
                head_connected_vars.update(literal.arguments)
                connected.append(literal)
        if not connected and body_literals:
            return False
        body_literals.difference_update(connected)
    return True

def connected(body):
    if len(body) == 1:
        return True

    body = list(body)
    connected_vars = set(body[0].arguments)
    body_literals = set(body[1:])

    while body_literals:
        connected = []
        for literal in body_literals:
            if any (x in connected_vars for x in literal.arguments):
                connected_vars.update(literal.arguments)
                connected.append(literal)
        if not connected and body_literals:
            return False
        body_literals.difference_update(connected)
    return True

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))
