import time
from collections import defaultdict
from functools import cache
from itertools import chain, combinations, permutations, combinations_with_replacement
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog
from . tester import Tester
from . bkcons import deduce_bk_cons, deduce_recalls, deduce_type_cons

NEW_IDEAS = False
NEW_IDEAS = True

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

def load_solver(settings, tester):
    if settings.debug:
        settings.logger.debug(f'Load exact solver: {settings.solver}')

    if settings.solver == "clingo":
        if settings.noisy:
            from . combine_mdl import Combiner
        else:
            from . combine import Combiner
    elif settings.solver in ['rc2', 'uwr', 'wmaxcdcl']:
        from . combine_ms import Combiner
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
    else:
        print('INVALID SOLVER')
        exit()

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

    return Combiner(settings, tester)

class Popper():
    def __init__(self, settings, tester, bkcons):
        self.settings = settings
        self.tester = tester
        self.bkcons = bkcons
        self.pruned2 = set()
        self.seen_prog = set()
        self.unsat = set()

        self.tmp = {}

    def run(self):
        settings, tester = self.settings, self.tester
        num_pos, num_neg = self.num_pos, self.num_neg = len(settings.pos_index), len(settings.neg_index)
        uncovered = set(settings.pos_index)

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

        combiner = load_solver(settings, tester)

        # generator that builds programs
        with settings.stats.duration('init'):
            if settings.single_solve:
                from . gen2 import Generator
            elif settings.max_rules == 2 and not settings.pi_enabled:
                from . gen3 import Generator
            else:
                from . generate import Generator
            generator = self.generator = Generator(settings, self.bkcons)

        # track the success sets of tested hypotheses
        success_sets = self.success_sets = {}

        # success_sets_by_size = self.success_sets_by_size = defaultdict(set)
        success_sets_noise = {}
        paired_success_sets = self.paired_success_sets = defaultdict(set)

        self.covered_by = defaultdict(set)

        # maintain a set of programs that we have not yet pruned
        could_prune_later = self.could_prune_later = [0]*(max_size+1)
        for i in range(max_size+1):
            could_prune_later[i] = dict()

        to_combine = []

        self.min_size = None

        last_size = None

        for size in range(2, max_size+1):
            if size > settings.max_literals:
                continue

            with settings.stats.duration('init'):
                generator.update_solver(size)

            while True:
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
                neg_covered = None
                inconsistent = None

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

                if settings.debug:
                    settings.logger.debug(f'Program {settings.stats.total_programs}:')
                    settings.logger.debug(format_prog(prog))

                if last_size is None or prog_size != last_size:
                    size_change = True
                    last_size = prog_size
                    settings.search_depth = prog_size
                    settings.logger.info(f'Generating programs of size: {prog_size}')

                    # if settings.solution_found:
                    #     with settings.stats.duration('prune backtrack covers too few SIZE CHANGE'):
                    #         subsumed_progs = tuple(self.prune_subsumed_backtrack_specialcase())
                    #         print('prune backtrack covers too few SIZE CHANGE')
                    #         print(len(subsumed_progs))
                    #         for subsumed_prog_ in subsumed_progs:
                    #             print(format_prog(subsumed_prog_))
                    #             subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog_)
                    #             new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))

                # TODO: refactor out for readability
                # test a program
                skipped, skip_early_neg = False, False

                with settings.stats.duration('test'):
                    if settings.noisy:
                        if settings.recursion_enabled or settings.pi_enabled:
                            pos_covered, neg_covered = tester.test_prog_all(prog)
                            inconsistent = len(neg_covered) > 0
                        else:
                            # AC: we could push all this reasoning to Prolog to only need a single call
                            pos_covered = tester.test_prog_pos(prog)
                            tp = len(pos_covered)
                            if tp > prog_size:
                                # maximum size of specialisations allowed
                                test_at_most_k_neg1 = min([settings.max_body-(prog_size-1), settings.max_literals-prog_size])
                                # conditions which determine whether a program can be part of a solution
                                test_at_most_k_neg2 = min([settings.best_mdl - prog_size, tp-prog_size])
                                test_at_most_k_neg = max([test_at_most_k_neg1, test_at_most_k_neg2])
                                neg_covered = tester.test_single_rule_neg_at_most_k(prog, test_at_most_k_neg)
                                if len(neg_covered) == test_at_most_k_neg:
                                    skip_early_neg = True

                                inconsistent = len(neg_covered) > 0
                            else:
                                skipped = True
                    else:
                        if settings.recursion_enabled or settings.pi_enabled:
                            pos_covered, inconsistent = tester.test_prog(prog)
                        else:
                            pos_covered = tester.test_prog_pos(prog)
                            inconsistent = True
                            # if no positive example covered, no need to check negative examples
                            if len(pos_covered) > 0:
                                if not settings.solution_found or len(pos_covered) > 1:
                                    inconsistent = tester.test_prog_inconsistent(prog)

                tp = len(pos_covered)
                fn = num_pos-tp

                # if non-separable program covers all examples, stop
                if not skipped and not inconsistent and tp == num_pos: # and not settings.order_space:
                    if not settings.functional_test or not tester.is_non_functional(prog):
                        settings.solution = prog
                        settings.best_prog_score = num_pos, 0, num_neg, 0, prog_size
                        settings.best_mdl = prog_size
                        return

                if settings.noisy:
                    fp, tn = None, None
                    if not skipped:
                        fp = len(neg_covered)
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
                            # if settings.delete_combine:
                                # combiner.update_deleted_progs(settings.best_mdl-min_score, mdl-min_score)
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
                    self.seen_prog.add(get_raw_prog(prog))
                    if tp == 0 or (settings.noisy and tp <= prog_size):
                        with settings.stats.duration('find mucs'):
                            cons_ = tuple(self.explain_incomplete(prog))
                            new_cons.extend(cons_)
                            pruned_more_general = len(cons_) > 0

                if not settings.noisy:
                    if tp > 0 and success_sets:

                        with settings.stats.duration('check subsumed'):
                            subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)

                        with settings.stats.duration('check subsumed by two'):
                            subsumed_by_two = self.subsumed_by_two_new(pos_covered, prog_size)

                        with settings.stats.duration('check covers too few'):
                            covers_too_few = self.check_covers_too_few(prog_size, pos_covered)

                        if subsumed or subsumed_by_two or covers_too_few:
                            add_spec = True

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

                        if not has_invention and not is_recursive:
                            if subsumed or subsumed_by_two or covers_too_few:
                                # If a program is subsumed or doesn't cover enough examples, we search for the most general subprogram that also is also subsumed or doesn't cover enough examples
                                # only applies to non-recursive and non-PI programs
                                subsumed_progs = []
                                with settings.stats.duration('find most general subsumed/covers_too_few'):
                                    subsumed_progs = self.subsumed_or_covers_too_few(prog, check_coverage=True, check_subsumed=True, check_subsumed_by_two=True, seen=set())
                                pruned_more_general = len(subsumed_progs) > 0

                                if settings.showcons and not pruned_more_general:
                                    if subsumed:
                                        print('\t', format_prog(prog), '\t', 'subsumed')
                                    elif subsumed_by_two:
                                        print('\t', format_prog(prog), '\t', 'subsumed by two')
                                    elif covers_too_few:
                                        print('\t', format_prog(prog), '\t', 'covers_too_few')
                                for subsumed_prog, message in subsumed_progs:
                                    if settings.showcons:
                                        print('\t', format_prog(subsumed_prog), '\t', message)

                                    subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog)
                                    new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))

                    if inconsistent:
                        # if inconsistent, prune generalisations
                        add_gen = True
                        if is_recursive:
                            combiner.add_inconsistent(prog)
                            with settings.stats.duration('find sub inconsistent'):
                                cons_ = frozenset(self.explain_inconsistent(prog))
                                new_cons.extend(cons_)
                                pruned_sub_inconsistent = len(cons_) > 0
                    else:
                        # if consistent, prune specialisations
                        add_spec = True
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

                    # if not add_spec and False:
                    #     with settings.stats.duration('spec_subset'):
                    #         # print(type(prog))
                    #         for i in range(len(prog)):
                    #             (head, body) = list(prog)[i]
                    #             for subbody in chain.from_iterable(combinations(body, k) for k in range(1, len(body))):
                    #                 subprog = list(prog)[:i] + [(head, subbody)] + list(prog)[i + 1:]
                    #                 subprog = frozenset(subprog)
                    #                 # print('\t'*3,format_prog(subprog))
                    #                 if subprog in saved_scores:
                    #                     # print('\t'*3,format_prog(subprog))
                    #                     # assert(False)
                    #                     [old_fp, old_fn, old_size] = saved_scores[subprog]
                    #                     if rule_vars((head, subbody)) == rule_vars(list(prog)[i]) and old_fp + old_fn + old_size <= fn + fp + size:
                    #                         settings.stats.pruned_count += 1
                    #                         print('MOOOO')
                    #                         assert(False)
                    #                         add_spec = True
                    #                         break

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

                if not add_spec and not pruned_more_general:
                    could_prune_later[prog_size][prog] = pos_covered

                add_to_combiner = False
                if settings.noisy:
                    if not skipped and not skip_early_neg and not is_recursive and not has_invention and tp >= prog_size+fp and tp >= prog_size and fp+prog_size < settings.best_mdl:
                        success_sets_noise[(pos_covered, neg_covered)] = prog
                        add_to_combiner = True
                else:
                    # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
                    if not inconsistent and not subsumed and not add_gen and tp > 0 and not pruned_more_general:
                        add_to_combiner = True

                        # with settings.stats.duration('backtrack remove subsumed'):

                        if not settings.recursion_enabled:
                            to_delete = []
                            for a,b in success_sets.items():
                                if a.issubset(pos_covered) and prog_size <= b:
                                    to_delete.append(a)
                            for x in to_delete:
                                # TODO: DELETE FROM COMBINER!!!!!!
                                del success_sets[x]

                        success_sets[pos_covered] = prog_size
                        # success_sets_by_size[prog_size].add(pos_covered)

                        for e in pos_covered:
                            self.covered_by[e].add((prog, prog_size, pos_covered))

                        for p, s in success_sets.items():
                            paired_success_sets[s+prog_size].add(p|pos_covered)

                        # print('good rule', prog_size)
                        if self.min_size is None:
                            self.min_size = prog_size

                if add_to_combiner:
                    to_combine.append((prog, pos_covered, neg_covered))

                    if not settings.noisy and not has_invention:
                        with settings.stats.duration('prune backtrack subsumed'):
                            subsumed_progs = tuple(self.prune_subsumed_backtrack(pos_covered, prog_size))
                            for subsumed_prog_ in subsumed_progs:
                                subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog_)
                                new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))

                call_combine = len(to_combine) > 0
                call_combine = call_combine and (settings.noisy or settings.solution_found)
                call_combine = call_combine and (len(to_combine) >= settings.batch_size or size_change)

                if add_to_combiner and not settings.noisy and not settings.solution_found and not settings.recursion_enabled:
                    if any(x in uncovered for x in pos_covered):

                        if settings.solution:
                            settings.solution = settings.solution | prog
                        else:
                            settings.solution = prog
                        uncovered = uncovered-pos_covered
                        tp = num_pos-len(uncovered)
                        fn = len(uncovered)
                        tn = num_neg
                        fp = 0
                        hypothesis_size = calc_prog_size(settings.solution)
                        settings.best_prog_score = tp, fn, tn, fp, hypothesis_size
                        settings.print_incomplete_solution2(settings.solution, tp, fn, tn, fp, hypothesis_size)

                        if len(uncovered) == 0:
                            settings.solution_found = True
                            settings.max_literals = hypothesis_size-1

                            # AC: sometimes adding these size constraints can take longer
                            for i in range(settings.max_literals+1, max_size+1):
                                generator.prune_size(i)

                            if not settings.noisy and not settings.recursion_enabled:
                                if NEW_IDEAS:
                                    with settings.stats.duration('prune backtrack covers too few'):
                                        subsumed_progs = tuple(self.prune_subsumed_backtrack_specialcase())
                                        for subsumed_prog_ in subsumed_progs:
                                            subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog_)
                                            new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))

                        call_combine = len(uncovered) == 0

                if call_combine:

                    # COMBINE
                    with settings.stats.duration('combine'):
                        is_new_solution_found = combiner.update_best_prog(to_combine)

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

                            if not settings.noisy and not settings.recursion_enabled:
                                if NEW_IDEAS:
                                    with settings.stats.duration('prune backtrack covers too few'):
                                        subsumed_progs = tuple(self.prune_subsumed_backtrack_specialcase())
                                        # print('prune_subsumed_backtrack_specialcase2', len(subsumed_progs))
                                        for subsumed_prog_ in subsumed_progs:
                                            subsumed_prog_ = frozenset(remap_variables(rule) for rule in subsumed_prog_)
                                            new_cons.append((Constraint.SPECIALISATION, subsumed_prog_))

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

                        # if size >= settings.max_literals and not settings.order_space:
                        if size >= settings.max_literals:
                            assert(False)
            if settings.single_solve:
                break
        assert(len(to_combine) == 0)

    # #
    # def subsumed_by_two(self, prog, pos_covered, prog_size):
    #     something = set()
    #     for pos_covered2, size2 in self.success_sets.items():
    #         if size2 + 1 >= prog_size:
    #             continue
    #         if not pos_covered2.isdisjoint(pos_covered):
    #             something.add((pos_covered2, size2))

    #     # checks = 0
    #     something = tuple(something)
    #     n = len(something)
    #     for i in range(0, n):
    #         p1, s1 = something[i]
    #         missing = None
    #         n = len(something)
    #         for j in range(i+1, n):
    #             p2, s2 = something[j]
    #             if s1+s2 > prog_size+1:
    #                 continue
    #             if missing is None:
    #                 missing = pos_covered-p1
    #             if missing.issubset(p2):
    #                 return True
    #     return False

    def subsumed_by_two_new(self, pos_covered, prog_size):
        paired_success_sets = self.paired_success_sets
        for i in range(2, prog_size+2):
            for x in paired_success_sets[i]:
                if pos_covered.issubset(x):
                    return True
        return False

    def check_covers_too_few(self, prog_size, pos_covered):
        # k = (prog_size, pos_covered)

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

    # @profile
    def check_covers_too_few_(self, prog_size, pos_covered):
        num_pos = self.num_pos
        if len(pos_covered) == num_pos:
            return False

        min_size = self.min_size
        assert(min_size)

        max_literals = self.settings.max_literals

        # MAX RULES = 1
        if ((prog_size + min_size) > max_literals):
            if len(pos_covered) != num_pos:
                return True

        # MAX RULES = 2
        elif ((prog_size + (min_size*2)) > max_literals):
            space_remaining = max_literals-prog_size
            if space_remaining > self.settings.search_depth:
                return False

            uncovered = self.tester.pos_examples-pos_covered
            for pos_covered2, size2 in self.success_sets.items():
                if size2 > space_remaining:
                    continue
                if uncovered.issubset(pos_covered2):
                    return False
            return True

        # MAX RULES = 3
        elif ((prog_size + (min_size*3)) > max_literals):
            space_remaining = max_literals-prog_size

            if space_remaining-min_size > self.settings.search_depth:
                return False

            uncovered = self.tester.pos_examples-pos_covered
            success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.success_sets.items() if not pos_covered_.isdisjoint(uncovered)), key=lambda x: x[1])
            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break
                if uncovered.issubset(pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                uncovered2 = uncovered-pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    if uncovered2.issubset(pos_covered3):
                        return False
            return True

        # MAX RULES = 4
        elif prog_size + (min_size*4) > max_literals:
            space_remaining = max_literals-prog_size
            space_remaining -= (min_size*2)

            if space_remaining > self.settings.search_depth:
                return False

            missing = self.tester.pos_examples-pos_covered

            success_sets = sorted(((pos_covered_, size) for (pos_covered_, size) in self.success_sets.items() if not pos_covered_.isdisjoint(missing)), key=lambda x: x[1])
            space_remaining = max_literals-prog_size

            n = len(success_sets)

            for i in range(n):
                pos_covered2, size2 = success_sets[i]
                if size2 > space_remaining:
                    break
                if missing.issubset(pos_covered2):
                    return False
                space_remaining_ = space_remaining-size2
                if space_remaining_ < min_size:
                    continue
                missing2 = missing-pos_covered2
                for j in range(i+1, n):
                    pos_covered3, size3 = success_sets[j]
                    if size3 > space_remaining_:
                        break
                    if pos_covered3.isdisjoint(missing2):
                        continue
                    if missing2.issubset(pos_covered3):
                        return False
                    space_remaining__ = space_remaining_-size3
                    if space_remaining__ < min_size:
                        continue
                    missing3 = missing2-pos_covered3
                    for k in range(j+1, n):
                        pos_covered4, size4 = success_sets[k]
                        if size4 > space_remaining__:
                            break
                        if missing3.issubset(pos_covered4):
                            return False
            return True

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
                    print('\t', format_rule(rule), '\t', f'unsat')

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

    #
    def subsumed_or_covers_too_few(self, prog, check_coverage=False, check_subsumed=False, check_subsumed_by_two=False, seen=set()):
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
            if any(frozenset(x) in self.pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, check_subsumed_by_two, seen)
                out.update(xs)
                continue

            if not self.has_valid_directions(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, check_subsumed_by_two, seen)
                out.update(xs)
                continue

            if tester.has_redundant_literal(new_prog):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, check_subsumed_by_two, seen)
                out.update(xs)
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = tester.get_pos_covered(new_prog, ignore=True)

            subsumed = check_subsumed and sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)
            subsumed_by_two = not subsumed and self.subsumed_by_two_new(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and self.check_covers_too_few(new_prog_size, sub_prog_pos_covered)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, check_subsumed_by_two, seen)
            if len(xs) > 0:
                out.update(xs)
                continue

            # for each pruned program, add the variants to the list of pruned programs
            # doing so reduces the number of pointless checks
            for x in self.find_variants(remap_variables(new_rule)):
                self.pruned2.add(x)

            if subsumed:
                out.add((new_prog, ('subsumed (generalisation)')))
            elif subsumed_by_two:
                out.add((new_prog, ('subsumed by two (generalisation)')))
            elif covers_too_few:
                out.add((new_prog, ('covers too few (generalisation)')))
            else:
                assert(False)
        return out

    def check_more_general_subsumed_by_two(self, prog):
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

            # # check whether we have seen this body before
            # k1 = frozenset(new_body)
            # if k1 in seen:
            #     continue
            # seen.add(k1)

            new_rule = (head, new_body)
            new_prog = frozenset({new_rule})

            # ensure at least one head variable is in the body
            if not settings.non_datalog_flag and not any(x in head_vars for literal in new_body for x in literal.arguments):
                continue

            # check whether we have pruned any subset (HORRIBLE CODE)
            # if any(frozenset(x) in self.pruned2 for x in non_empty_powerset(new_body)):
                # continue

            if not head_connected(new_rule):
                xs = self.check_more_general_subsumed_by_two(new_prog)
                out.update(xs)
                continue

            if not self.has_valid_directions(new_rule):
                xs = self.check_more_general_subsumed_by_two(new_prog)
                out.update(xs)
                continue

            # if tester.has_redundant_literal(new_prog):
            #     xs = self.check_more_general_subsumed_by_two(new_prog)
            #     out.update(xs)
            #     continue

            # print('MOOOOO')
            pos_covered = tester.get_pos_covered(new_prog, ignore=True)

            # subsumed = sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)

            # prune = check_subsumed and subsumed
            # prune = prune or (check_coverage and len(sub_prog_pos_covered) == 1)

            prog_size = calc_prog_size(new_prog)

            something = set()
            for pos_covered2, size2 in success_sets.items():
                if size2 + 1 == prog_size:
                    continue
                if pos_covered2.issubset(pos_covered):
                    something.add((pos_covered2, size2))

            shitsub = False
            for p1, s1 in something:
                if shitsub:
                    break
                for p2, s2 in something:
                    if shitsub:
                        break
                    # print(s1+s2-1)
                    if s1+s2-1 > prog_size:
                        continue
                    # print('x', p1|p2)
                    if pos_covered.issubset((p1|p2)):
                        shitsub = True
                        # print(s1+s2, prog_size)
                        print("SHIT_SUB"*4, format_prog(prog))
                        break

            if not shitsub:
                continue

            xs = self.check_more_general_subsumed_by_two(new_prog)
            if len(xs) > 0:
                out.update(xs)
                continue

            # for each pruned program, add the variants to the list of pruned programs
            # doing so reduces the number of pointless checks
            # for x in self.find_variants(remap_variables(new_rule)):
                # self.pruned2.add(x)

            out.add(new_prog)
        return out


    # def subsumed_or_covers_too_few2(self, prog, check_coverage=False, check_subsumed=False, seen=set()):
    #     tester, success_sets, settings = self.tester, self.success_sets, self.settings

    #     out = []
    #     for subprog in generalisations(prog, allow_headless=False, recursive=True):


    #         if not self.prog_is_ok(subprog):
    #             xs = self.subsumed_or_covers_too_few2(subprog, check_coverage, check_subsumed, seen)
    #             out.extend(xs)
    #             continue

    #         if self.tester.has_redundant_literal(subprog):
    #             xs = self.subsumed_or_covers_too_few2(subprog, check_coverage, check_subsumed, seen)
    #             out.extend(xs)
    #             continue

    #         test_prog = frozenset(self.build_test_prog(subprog))

    #         sub_prog_pos_covered = tester.get_pos_covered(test_prog)

    #         # this check assumes that we search by increasing program size
    #         subsumed = sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets)

    #         prune = check_subsumed and subsumed
    #         prune = prune or (check_coverage and len(sub_prog_pos_covered) == 1)

    #         if check_coverage and len(sub_prog_pos_covered) == 1 and not subsumed:
    #             print("POOOOOOOOOO")
    #             print(format_prog(new_prog))
    #             assert(False)

    #         if not prune:
    #             continue

    #         xs = self.subsumed_or_covers_too_few2(subprog, check_coverage, check_subsumed, seen)
    #         if len(xs) > 0:
    #             out.extend(xs)
    #             continue

    #         # for each pruned program, add the variants to the list of pruned programs
    #         # doing so reduces the number of pointless checks
    #         # for x in self.find_variants(functional_rename_vars(new_rule)):
    #             # self.pruned2.add(x)

    #         out.append(subprog)
    #     return out

    def prune_subsumed_backtrack(self, pos_covered, prog_size):
        could_prune_later, tester, settings = self.could_prune_later, self.tester, self.settings
        to_prune = set()
        to_delete = set()
        seen = set()
        pruned2 = self.pruned2

        def get_zs():
            for x in could_prune_later:
                yield from x.items()

        for prog2, pos_covered2 in get_zs():

            if len(prog2) > 1:
                # should_prune = check_coverage and len(pos_covered2) == 1
                subsumed = pos_covered2.issubset(pos_covered)
                if subsumed:
                    # print('PRUNE!!!')
                    # print(format_prog(prog2))
                    # TODO: FIND MOST GENERAL SUBSUMED PROGRAM
                    to_delete.add(prog2)
                    to_prune.add(prog2)
                continue

# <<<<<<< HEAD
# =======
            # print('HELLO')
            # exit()

            # should_prune = check_coverage and len(pos_covered2) == 1
            # if settings.solution_found and not settings.noisy and len(settings.solution) == 2:
                # print('RAH1!!')
                # exit()
            # should_prune = should_prune or (settings.solution_found and not settings.recursion_enabled and not settings.noisy and len(settings.solution) == 2 and len(pos_covered2) != self.num_pos)
# >>>>>>> main

            prog2_size = calc_prog_size(prog2)
            subsumed = pos_covered2.issubset(pos_covered)
            subsumed_by_two = not subsumed and self.subsumed_by_two_new(pos_covered, prog2_size)

            if not (subsumed or subsumed_by_two):
                continue

            head, body = tuple(prog2)[0]

            seen.add(body)

            # If we have seen a subset of the body then ignore this program
            if any(frozenset(x) in pruned2 for x in non_empty_powerset(body)):
                to_delete.add(prog2)
                continue

            pruned_subprog = False

            # We now enumerate the subsets of the body of this role to find the most general subsumed subset
            for new_body in non_empty_subset(body):
                new_rule = (head, new_body)

                if not head_connected(new_rule):
                    continue

                if not self.has_valid_directions(new_rule):
                    continue

                tmp = frozenset(new_body)
                if tmp in seen:
                    continue
                seen.add(tmp)

                new_prog = frozenset([new_rule])

                if tester.has_redundant_literal(new_prog):
                    continue

                sub_prog_pos_covered = tester.get_pos_covered(new_prog)

                sub_prog_subsumed = sub_prog_pos_covered == pos_covered2

                sub_subsumed_by_two = not sub_prog_subsumed and self.subsumed_by_two_new(sub_prog_pos_covered, calc_prog_size(new_prog))

                if self.settings.showcons:
                    if sub_prog_subsumed:
                        print('\t', format_prog(new_prog), '\t', 'subsumed backtrack (generalisation)')
                    elif sub_subsumed_by_two:
                        print('\t', format_prog(new_prog), '\t', 'subsumed by two backtrack (generalisation)')

                if sub_prog_subsumed or sub_subsumed_by_two:
                    to_prune.add(new_prog)
                    pruned_subprog = True
                    for x in self.find_variants(remap_variables(new_rule)):
                        pruned2.add(x)
                    break

            to_delete.add(prog2)

            if pruned_subprog:
                continue

            for x in self.find_variants(remap_variables((head, body))):
                pruned2.add(x)

            if self.settings.showcons:
                print('\t', format_prog(prog2), '\t', 'subsumed backtrack')
            to_prune.add(prog2)

        for x in to_delete:
            del could_prune_later[calc_prog_size(x)][x]

        return to_prune

    def prune_subsumed_backtrack_specialcase(self):
        could_prune_later, tester, settings = self.could_prune_later, self.tester, self.settings
        to_prune = set()
        to_delete = set()
        seen = set()
        pruned2 = self.pruned2

        if not settings.solution_found:
            return

        def get_zs():
            for x in could_prune_later:
                yield from x.items()

        for prog2, pos_covered2 in get_zs():

            if len(prog2) > 1:
                continue

            covers_too_few = self.check_covers_too_few(calc_prog_size(prog2), pos_covered2)
            if not covers_too_few:
                continue
            # print('SUB_COVERS_TOO_FEW BACKTRACKING')

            head, body = tuple(prog2)[0]

            seen.add(body)

            # If we have seen a subset of the body then ignore this program
            if any(frozenset(x) in pruned2 for x in non_empty_powerset(body)):
                to_delete.add(prog2)
                continue

            pruned_subprog = False

            # We now enumerate the subsets of the body of this role to find the most general subsumed subset
            for new_body in non_empty_subset(body):
                new_rule = (head, new_body)

                if not head_connected(new_rule):
                    continue

                if not self.has_valid_directions(new_rule):
                    continue

                tmp = frozenset(new_body)
                if tmp in seen:
                    continue
                seen.add(tmp)

                new_prog = frozenset([new_rule])

                if tester.has_redundant_literal(new_prog):
                    continue

                sub_prog_pos_covered = tester.get_pos_covered(new_prog)
                sub_covers_too_few = self.check_covers_too_few(calc_prog_size(new_prog), sub_prog_pos_covered)

                if sub_covers_too_few:
                    if self.settings.showcons:
                        print('\t', format_prog(new_prog), '\t', 'covers too few backtrack (generalisation)')
                    to_prune.add(new_prog)
                    pruned_subprog = True
                    for x in self.find_variants(remap_variables(new_rule)):
                        pruned2.add(x)
                    break

            to_delete.add(prog2)

            if pruned_subprog:
                continue

            for x in self.find_variants(remap_variables((head, body))):
                pruned2.add(x)

            if self.settings.showcons:

                print('\t', format_prog(prog2), '\t', 'covers too few backtrack')
            to_prune.add(prog2)

        for x in to_delete:
            del could_prune_later[calc_prog_size(x)][x]

        return to_prune

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
        return list(self.explain_totally_incomplete_aux2(prog, unsat=set(), unsat2=set()))

    def explain_totally_incomplete_aux2(self, prog, unsat=set(), unsat2=set()):
        has_recursion = prog_is_recursive(prog)

        # print('---')
        # for rule in prog:
        #     print('\t', 'A', format_rule(rule))
        # for prog_ in unsat:
        #     print('\t', 'unsat', prog_)

        out = []
        for subprog in generalisations(prog, allow_headless=True, recursive=has_recursion):



            subprog = frozenset(subprog)
            if subprog in self.seen_prog:
                continue
            self.seen_prog.add(subprog)

            raw_prog = get_raw_prog(subprog)
            if raw_prog in self.seen_prog:
                continue
            self.seen_prog.add(raw_prog)

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

            if seen_more_general_unsat(subprog, unsat2):
                continue

            if seen_more_general_unsat(raw_prog, unsat):
                continue

            if not self.prog_is_ok(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat, unsat2)
                out.extend(xs)
                continue

            # for rule in subprog:
                # print('\t', 'D', format_rule(rule))

            if self.tester.has_redundant_literal(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, unsat, unsat2)
                out.extend(xs)
                continue

            # if len(subprog) > 2 and self.tester.has_redundant_rule(subprog):
            #     xs = self.explain_totally_incomplete_aux2(subprog, directions, sat, unsat, noisy)
            #     out.extend(xs)
            #     continue

            test_prog = self.build_test_prog(subprog)

            headless = is_headless(subprog)

            # print('\t\t\t testing',format_prog(subprog))

            if headless:
                body = list(test_prog)[0][1]
                if self.tester.is_body_sat(body):
                    # print('\t\t\t SAT',format_prog(subprog))
                    continue
            else:
                if self.tester.is_sat(test_prog):
                    # print('\t\t\t SAT',format_prog(subprog))
                    continue
            # print('\t\t\t UNSAT',format_prog(subprog))

            unsat.add(raw_prog)
            unsat2.add(subprog)
            self.unsat.add(subprog)
            self.unsat.add(raw_prog)

            xs = self.explain_totally_incomplete_aux2(subprog, unsat, unsat2)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))
        return out

    @cache
    def has_valid_directions(self, rule):
        if not self.settings.has_directions:
            return True
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
    Popper(settings, tester, bkcons).run()

def learn_solution(settings):
    t1 = time.time()
    settings.nonoise = not settings.noisy
    settings.solution_found = False

    with settings.stats.duration('load data'):
        tester = Tester(settings)

    bkcons = []

    pointless = settings.pointless = set()
    settings.logger.debug(f'Loading recalls')
    with suppress_stdout_stderr():
        try:
            pointless = settings.pointless = find_pointless_relations(settings)
            settings.datalog = True
        except:
            settings.datalog = False

    for p,a in pointless:
        if settings.showcons:
            print('remove pointless relation', p, a)
        settings.body_preds.remove((p,a))

    if settings.datalog:
        with settings.stats.duration('recalls'):
            recalls = tuple(deduce_recalls(settings))
        if settings.showcons:
            for x in recalls:
                print('recall', x)
        bkcons.extend(recalls)

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
            for x in xs:
                print('bkcon', x)
        # bkcons.extend(xs)
    time_so_far = time.time()-t1
    timeout(settings, popper, (settings, tester, bkcons), timeout_duration=int(settings.timeout-time_so_far),)
    return settings.solution, settings.best_prog_score, settings.stats

def generalisations(prog, allow_headless=True, recursive=False):

    if len(prog) == 1:
        rule = list(prog)[0]
        head, body = rule

        if allow_headless:
            if head and len(body) > 1:
                new_rule = (None, body)
                new_prog = [new_rule]
                yield new_prog

        if (recursive and len(body) > 2 and head) or (not recursive and head and len(body) > 1) or (not recursive and not head and len(body) > 2):
            body = list(body)

            for i in range(len(body)):
                # do not remove recursive literals
                pred, args = body[i]
                if recursive:
                    head_pred, head_args = head
                    if pred == head_pred:
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

def connected(body):
    if len(body) == 1:
        return True

    body = list(body)
    connected_vars = set(body[0].arguments)
    body_literals = set(body[1:])

    while body_literals:
        changed = False
        for literal in body_literals:
            if any (x in connected_vars for x in literal.arguments):
                connected_vars.update(literal.arguments)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            return False

    return True

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

@cache
def head_connected(rule):
    head, body = rule
    head_connected_vars = set(head.arguments)
    body_literals = set(body)

    if not any(x in head_connected_vars for literal in body for x in literal.arguments):
        return False

    while body_literals:
        changed = False
        for literal in body_literals:
            if any (x in head_connected_vars for x in literal.arguments):
                head_connected_vars.update(literal.arguments)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            return False

    return True

def is_headless(prog):
    return any(head is None for head, body in prog)

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def non_empty_subset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)))

def find_pointless_relations(settings):

    import clingo
    encoding = []

    encoding.append('#show same/2.')

    arities = {}

    for p, pa in settings.body_preds:
        arities[p] = pa
        for q, qa in settings.body_preds:
            if p == q:
                continue
            if pa != qa:
                continue

            if settings.body_types[p] != settings.body_types[q]:
                continue

            arg_str = ','.join(f'V{i}' for i in range(pa))

            rule1 = f'diff({p},{q}):- {p}({arg_str}), not {q}({arg_str}).'
            rule2 = f'diff({p},{q}):- {q}({arg_str}), not {p}({arg_str}).'
            rule3 = f'same({p},{q}):- {p}<{q}, not diff({p},{q}).'

            encoding.extend([rule1, rule2, rule3])

    encoding.append('\n')
    with open(settings.bk_file) as f:
        bk = f.read()
        encoding.append(bk)

    encoding = '\n'.join(encoding)

    # with open('encoding.pl', 'w') as f:
    #     f.write(encoding)

    solver = clingo.Control(['-Wnone'])
    solver.add('base', [], encoding)
    solver.ground([('base', [])])

    keep = set()
    pointless = set()

    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                # print(str(atom))
                a, b = str(atom)[5:-1].split(',')
                if a in keep and b in keep:
                    assert(False)
                if a not in pointless and b not in pointless:
                    if a in keep:
                        pointless.add(b)
                        # print('drop1', b)
                    elif b in keep:
                        pointless.add(a)
                        # print('drop1', a)
                    else:
                        keep.add(a)
                        pointless.add(b)
                        # print('drop1', b)
                elif a in pointless or b in pointless:
                    if a not in keep:
                        pointless.add(a)
                        # print('drop5', a)
                    if b not in keep:
                        pointless.add(b)
                        # print('drop5', b)
                elif a not in pointless and b not in pointless:
                    keep.add(a)
                    pointless.add(b)
                    # print('drop2', b)
                elif a in pointless:
                    pointless.add(b)
                    # print('drop3', b)
                elif b in pointless:
                    pointless.add(b)
                    # print('keep', a)
                    # print('drop4', b)


                # if a in keep and b in keep:
                #     assert(False)
                # elif a in keep and b not in keep:
                #     pointless.add(b)
                #     print('drop', b)
                # elif a not in keep and b in keep:
                #     pointless.add(a)
                #     print('drop', a)
                # elif a not in keep and b not in keep:
                #     keep.add(a)
                #     pointless.add(b)
                #     print('keep', a)
                #     print('drop', b)

                # same(input_plow_row,input_harvest_col)
    # print('-----')
    # for x in keep:
        # print('keep', x)
    # for x in pointless:
        # print('drop', x)
    # exit()
    return frozenset((p, arities[p]) for p in pointless)
    # settings.drop_preds = pointless
