import time
from collections import defaultdict
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, format_prog, Constraint, bias_order, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal
from . tester import Tester
from . bkcons import deduce_bk_cons, deduce_recalls

def functional_rename_vars(rule):
    head, body = rule
    seen_args = set(atom.arguments for atom in body)

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
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(body_literal.predicate, tuple(new_args), body_literal.directions)
        new_body.add(new_atom)

    return head, frozenset(new_body)

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


def is_subsumed(pos_covered, prog_size, success_sets):
    subsumed = pos_covered in success_sets and prog_size >= (success_sets[pos_covered])
    subsumed = subsumed or any(pos_covered.issubset(xs) and prog_size >= prog_size2 for xs, prog_size2 in success_sets.items())
    return subsumed

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

    def run(self):
        settings = self.settings
        tester = self.tester
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
            else:
                from . generate import Generator
            generator = self.generator = Generator(settings, self.bkcons)

        # track the success sets of tested hypotheses
        success_sets = self.success_sets = {}
        success_sets_noise = {}
        success_sets_recursion = {}

        # maintain a set of programs that we have not yet pruned
        could_prune_later = self.could_prune_later = [0]*(max_size+1)
        for i in range(max_size+1):
            could_prune_later[i] = dict()


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
                neg_covered = None
                inconsistent = None

                new_cons = []

                # generate a program
                with settings.stats.duration('generate'):
                    prog = generator.get_prog()
                    if prog is None:
                        break

                prog_size = calc_prog_size(prog)

                settings.stats.total_programs += 1

                if settings.debug:
                    settings.logger.debug(f'Program {settings.stats.total_programs}:')
                    settings.logger.debug(format_prog(prog))

                if last_size is None or prog_size != last_size:
                    size_change = True
                    last_size = prog_size
                    if not settings.order_space:
                        settings.logger.info(f'Generating programs of size: {prog_size}')

                # AC: TODO: CLINGO SPECIFIC
                if settings.single_solve and last_size > settings.max_literals:
                    break

                if last_size > settings.max_literals:
                    print("last_size > settings.max_literals")
                    assert(False)

                is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
                has_invention = settings.pi_enabled and prog_has_invention(prog)

                # TODO: refactor out for readability
                # test a program
                skipped, skip_early_neg = False, False
                # print('testing?')
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
                            # check pos examples
                            pos_covered = tester.test_prog_pos(prog)
                            inconsistent = True
                            # if no positive example covered, no need to check negative examples
                            if len(pos_covered) > 0:
                                if not settings.solution_found or len(pos_covered) > 1:
                                    inconsistent = tester.test_prog_inconsistent(prog)
                            # print('moo', pos_covered, inconsistent)

                tp = len(pos_covered)
                fn = num_pos-tp

                # if non-separable program covers all examples, stop
                if not skipped and not inconsistent and tp == num_pos and not settings.order_space:
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
                if not skipped and not inconsistent:
                    add_spec = True

                #  if covers all positive examples prune generalisations
                if tp == num_pos:
                    add_gen = True

                if not has_invention:
                    self.seen_prog.add(get_raw_prog(prog))
                    if tp == 0 or (settings.noisy and tp < prog_size):
                        # if the programs does not cover any positive examples, check whether it is has an unsat core
                        with settings.stats.duration('find mucs'):
                            cons_ = tuple(self.explain_incomplete(prog))
                            new_cons.extend(cons_)
                            pruned_sub_incomplete = len(cons_) > 0

                if not settings.noisy:
                    if not is_recursive and tp > 0:
                        # if we do not search by increasing size, we need to use a strict form of subsumption
                        if settings.order_space:
                            subsumed = is_subsumed(pos_covered, prog_size, success_sets)
                        else:
                            subsumed = pos_covered in success_sets
                            subsumed = subsumed or any(pos_covered.issubset(xs) for xs in success_sets)

                        if subsumed:
                            add_spec = True

                        if not has_invention:
                            # we check whether a program does not cover enough examples to be useful
                            # if the program only not cover enough examples, we prune it specialisations
                            covers_too_few = settings.solution_found and not settings.order_space and tp == 1
                            if covers_too_few:
                                add_spec = True

                            if subsumed or covers_too_few:
                                # If a program is subsumed or doesn't cover enough examples, we search for the most general subprogram that also is also subsumed or doesn't cover enough examples
                                # only applies to non-recursive and non-PI programs
                                with settings.stats.duration('self.subsumed_or_covers_too_few'):
                                    xs = self.subsumed_or_covers_too_few(prog, check_coverage=covers_too_few, check_subsumed=subsumed, seen=set())
                                pruned_more_general = len(xs) > 0
                                if settings.showcons and not pruned_more_general:
                                    if subsumed:
                                        print('\t', format_prog(prog), '\t', 'subsumed')
                                    else:
                                        print('\t', format_prog(prog), '\t', 'covers_too_few')
                                for x in xs:
                                    if settings.showcons:
                                        if subsumed:
                                            print('\t', format_prog(x), '\t', 'subsumed (generalisation)')
                                        else:
                                            print('\t', format_prog(x), '\t', 'covers_too_few (generalisation)', tp)


                                    new_cons.append((Constraint.SPECIALISATION, [functional_rename_vars(list(x)[0])]))

                    if inconsistent:
                        # if inconsistent, prune generalisations
                        add_gen = True
                        if is_recursive:
                            combiner.add_inconsistent(prog)
                            with settings.stats.duration('find sub inconsistent'):
                                cons_ = set(self.explain_inconsistent(prog))
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
                                if cons_:
                                    new_cons.extend(cons_)

                    seen_better_rec = False
                    if is_recursive and not inconsistent and not subsumed and not add_gen and tp > 0:
                        if settings.order_space:
                            # this check does not assume that we search by increasing program size
                            subsumed = is_subsumed(pos_covered, prog_size, success_sets_recursion)
                        else:
                            # this check assumes that we search by increasing program size
                            seen_better_rec = pos_covered in success_sets_recursion or any(pos_covered.issubset(xs) for xs in success_sets_recursion)

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

                if not add_spec and not pruned_more_general and not pruned_sub_incomplete:
                    could_prune_later[prog_size][prog] = pos_covered


                add_to_combiner = False
                if settings.noisy:
                    if not skipped and not skip_early_neg and not is_recursive and not has_invention and tp >= prog_size+fp and tp >= prog_size and fp+prog_size < settings.best_mdl:
                        success_sets_noise[(pos_covered, neg_covered)] = prog
                        add_to_combiner = True
                else:
                    # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
                    if not inconsistent and not subsumed and not add_gen and tp > 0 and not seen_better_rec and not pruned_more_general:
                        add_to_combiner = True

                    if settings.order_space and not inconsistent and not subsumed and tp > 0 and not seen_better_rec and not pruned_more_general:
                        add_to_combiner = True

                    if add_to_combiner:
                        success_sets[pos_covered] = prog_size
                        if is_recursive:
                            success_sets_recursion[pos_covered] = prog_size

                if add_to_combiner:
                    to_combine.append((prog, pos_covered, neg_covered))

                    if not settings.noisy and not has_invention and not is_recursive:
                        with settings.stats.duration('prune backtrack'):
                            xs = self.prune_subsumed_backtrack2(pos_covered, prog_size, check_coverage=settings.solution_found)
                            for x in xs:
                                new_cons.append((Constraint.SPECIALISATION, [functional_rename_vars(list(x)[0])]))

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

                            if size >= settings.max_literals and not settings.order_space:
                                print('POOPER')
                                return

                            # AC: sometimes adding these size constraints can take longer
                            for i in range(hypothesis_size, max_size+1):
                                generator.prune_size(i)
                                # size_con = [(atom_to_symbol("size", (i,)), True)]
                                # model.context.add_nogood(size_con)

                # BUILD CONSTRAINTS
                if add_spec and not pruned_sub_incomplete and not pruned_more_general and not add_redund2:
                    new_cons.append((Constraint.SPECIALISATION, prog))

                if not skipped:
                    if settings.noisy and not add_spec and spec_size and not pruned_sub_incomplete:
                        if spec_size <= settings.max_literals and ((is_recursive or has_invention or spec_size <= settings.max_body)):
                            new_cons.append((Constraint.SPECIALISATION, prog, spec_size))
                            self.seen_hyp_spec[fp+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size])

                if add_gen and not pruned_sub_inconsistent:
                    if settings.noisy or settings.recursion_enabled or settings.pi_enabled:
                        if not pruned_sub_incomplete:
                            new_cons.append((Constraint.GENERALISATION, prog))
                    else:
                        if not add_spec:
                            new_cons.append((Constraint.GENERALISATION, prog))

                if settings.noisy and not add_gen and gen_size and not pruned_sub_inconsistent:
                    if gen_size <= settings.max_literals and (settings.recursion_enabled or settings.pi_enabled) and not pruned_sub_incomplete:
                        new_cons.append((Constraint.GENERALISATION, prog, gen_size))
                        self.seen_hyp_gen[fn+prog_size+mdl].append([prog, tp, fn, tn, fp, prog_size])

                if add_redund1 and not pruned_sub_incomplete:
                    new_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, prog))

                if add_redund2 and not pruned_sub_incomplete:
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

    # find unsat cores
    def explain_incomplete(self, prog):
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
                yield (Constraint.SPECIALISATION, [functional_rename_vars(subprog[0])])
                continue

            if len(subprog) == 1:
                yield (Constraint.REDUNDANCY_CONSTRAINT1, subprog)

            yield (Constraint.REDUNDANCY_CONSTRAINT2, subprog)

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

    def subsumed_or_covers_too_few(self, prog, check_coverage=False, check_subsumed=False, seen=set()):
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
            if any(frozenset((y.predicate, y.arguments) for y in x) in self.pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, seen)
                out.update(xs)
                continue

            if not has_valid_directions(new_rule):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, seen)
                out.update(xs)
                continue

            if tester.has_redundant_literal(new_prog):
                xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, seen)
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
                print(format_prog(new_prog))
                assert(False)

            if not prune:
                continue

            xs = self.subsumed_or_covers_too_few(new_prog, check_coverage, check_subsumed, seen)
            if len(xs) > 0:
                out.update(xs)
                continue

            # for each pruned program, add the variants to the list of pruned programs
            # doing so reduces the number of pointless checks
            for x in self.find_variants(functional_rename_vars(new_rule)):
                self.pruned2.add(x)

            out.add(new_prog)
        return out

    def prune_subsumed_backtrack2(self, pos_covered, prog_size, check_coverage):
        could_prune_later, tester = self.could_prune_later, self.tester
        to_prune = set()
        to_delete = set()
        seen = set()
        pruned2 = self.pruned2

        def get_zs():
            for x in could_prune_later:
                yield from x.items()

        for prog2, pos_covered2 in get_zs():

            should_prune = check_coverage and len(pos_covered2) == 1
            subsumed = pos_covered2.issubset(pos_covered)

            if not (should_prune or subsumed):
            # if not should_prune or subsumed:
                continue

            head, body = tuple(prog2)[0]

            seen.add(body)

            # If we have seen a subset of the body then ignore this program
            if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(body)):
                to_delete.add(prog2)
                continue

            pruned_subprog = False

            # We now enumerate the subsets of the body of this role to find the most general subsumed subset
            for new_body in non_empty_subset(body):
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

                if tester.has_redundant_literal(new_prog):
                    continue

                sub_prog_pos_covered = tester.get_pos_covered(new_prog)

                # prune if we have a solution and the subprogram only covers one example
                sub_covers_too_few = check_coverage and len(sub_prog_pos_covered) == 1
                sub_prog_subsumed = sub_prog_pos_covered == pos_covered2

                if sub_prog_subsumed:
                    if self.settings.showcons:
                        print('\t', format_prog(new_prog), '\t', 'subsumed_backtrack (generalisation)')

                if not sub_prog_subsumed and sub_covers_too_few:
                    if self.settings.showcons:
                        print('\t', format_prog(new_prog), '\t', 'COVERS TOO FEW')
                    assert(False)

                if sub_prog_subsumed or sub_covers_too_few:
                    to_prune.add(new_prog)
                    pruned_subprog = True
                    for x in self.find_variants(functional_rename_vars(new_rule)):
                        pruned2.add(x)
                    break

            to_delete.add(prog2)

            if pruned_subprog:
                continue

            for x in self.find_variants(functional_rename_vars((head, body))):
                pruned2.add(x)

            if self.settings.showcons:
                print('\t', format_prog(prog2), '\t', 'subsumed_backtrack')
            to_prune.add(prog2)

        for x in to_delete:
            del could_prune_later[calc_prog_size(x)][x]

        return to_prune

    def find_variants(self, rule):
        head, body = rule
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head.arity)
        subset = range(head.arity, self.settings.max_vars)
        for xs in permutations(subset, len(body_vars)):
            xs = head.arguments + xs
            new_body = []
            for atom in body:
                new_args = tuple(xs[arg] for arg in atom.arguments)
                new_literal = (atom.predicate, new_args)
                new_body.append(new_literal)
            yield frozenset(new_body)

    def build_test_prog(self, subprog):
        directions = self.settings.directions
        test_prog = []
        for head, body in subprog:
            if head:
                head_modes = tuple(directions[head.predicate][i] for i in range(head.arity))
                head_literal = Literal(head.predicate, head.arguments, head_modes)
            else:
                head_literal = False
            body_literals = set()
            for body_literal in body:
                body_modes = tuple(directions[body_literal.predicate][i] for i in range(body_literal.arity))
                body_literals.add(Literal(body_literal.predicate, body_literal.arguments, body_modes))
            rule = head_literal, body_literals
            test_prog.append(rule)
        return test_prog

    def explain_totally_incomplete(self, prog):
        return list(self.explain_totally_incomplete_aux2(prog, set(), set()))

    def explain_totally_incomplete_aux2(self, prog, sat=set(), unsat=set()):
        has_recursion = prog_is_recursive(prog)

        out = []
        for subprog in generalisations(prog, allow_headless=True, recursive=has_recursion):

            # print('---')
            # for rule in subprog:
            #     print('\t', 'A', format_rule(rule))

            # raw_prog2 = get_raw_prog2(subprog)

            # if raw_prog2 in self.seen_prog:
                # continue

            raw_prog = get_raw_prog(subprog)
            if raw_prog in self.seen_prog:
                continue

            self.seen_prog.add(raw_prog)
            # self.seen_prog.add(raw_prog2)


            # for rule in subprog:
                # print('\t', 'B', format_rule(rule))

            def should_skip():
                if len(subprog) > 0:
                    return False
                h_, b_ = list(subprog)[0]
                for x in non_empty_powerset(b_):
                    if get_raw_prog([(None,x)]) in self.unsat:
                        return True
                    if get_raw_prog([(h_,x)]) in self.unsat:
                        return True
                return False

            if should_skip():
                continue
                # pass

            if seen_more_general_unsat(raw_prog, unsat):
                continue
                # pass

            # if seen_more_general_unsat(raw_prog2, unsat):
                # continue
                # pass

            # for rule in subprog:
                # print('\t', 'C', format_rule(rule))


            if not prog_is_ok(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, sat, unsat)
                out.extend(xs)
                continue

            # for rule in subprog:
                # print('\t', 'D', format_rule(rule))

            if self.tester.has_redundant_literal(subprog):
                xs = self.explain_totally_incomplete_aux2(subprog, sat, unsat)
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
                body = test_prog[0][1]
                if self.tester.is_body_sat(body):
                    sat.add(raw_prog)
                    continue
            else:
                if self.tester.is_sat(test_prog, self.settings.noisy):
                    # print('\t\t\t SAT',format_prog(subprog))
                    sat.add(raw_prog)
                    continue
                # print('\t\t\t UNSAT',format_prog(subprog))

            unsat.add(raw_prog)
            # unsat.add(raw_prog2)
            self.unsat.add(raw_prog)
            # self.unsat.add(raw_prog2)

            xs = self.explain_totally_incomplete_aux2(subprog, sat, unsat)
            if len(xs):
                out.extend(xs)
            else:
                out.append((subprog, headless))
        return out

def popper(settings, tester, bkcons):
    x = Popper(settings, tester, bkcons)
    x.run()

def learn_solution(settings):
    t1 = time.time()
    settings.nonoise = not settings.noisy
    settings.solution_found = False

    with settings.stats.duration('load data'):
        tester = Tester(settings)

    bkcons = []

    settings.logger.debug(f'Loading recalls')
    with suppress_stdout_stderr():
        try:
            with settings.stats.duration('recalls'):
                bkcons.extend(deduce_recalls(settings))
            settings.datalog = True
        except:
            settings.datalog = False

    if not settings.datalog:
        settings.logger.debug(f'Loading recalls FAILURE')
    else:
        import signal

        def handler(signum, frame):
            raise TimeoutError()

        settings.logger.debug(f'Loading bkcons')
        with settings.stats.duration('bkcons'):
            signal.signal(signal.SIGALRM, handler)
            signal.alarm(settings.bkcons_timeout)
            try:
                bkcons.extend(deduce_bk_cons(settings, tester))
            except TimeoutError as _exc:
                settings.logger.debug(f'Loading bkcons FAILURE')
            finally:
                signal.alarm(0)
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


def prog_is_ok(prog):
    for rule in prog:
        head, body = rule
        if head and not head_connected(rule):
            return False

        if not head and not connected(body):
            return False

        if not has_valid_directions(rule):
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


    if needs_datalog(prog) and not tmp(prog):
        return False

    return True

def needs_datalog(prog):
    for rule in prog:
        rec_outputs = set()
        non_rec_inputs = set()
        head, body = rule
        for literal in body:
            if literal.predicate == head.predicate:
                rec_outputs.update(literal.outputs)
            else:
                # if any(x in xr)
                non_rec_inputs.update(literal.inputs)
        if any(x in rec_outputs for x in non_rec_inputs):
            return True
    return False

def tmp(prog):
    for rule in prog:
        head, body = rule
        body_args = set(x for atom in body for x in atom.arguments)
        if any(x not in body_args for x in head.arguments):
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

@cache
def has_valid_directions(rule):
    head, body = rule

    if head:
        if len(head.inputs) == 0:
            return True

        grounded_variables = head.inputs
        body_literals = set(body)

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                if not literal.inputs.issubset(grounded_variables):
                    continue
                if literal.predicate != head.predicate:
                    # find the first ground non-recursive body literal and stop
                    selected_literal = literal
                    break
                elif selected_literal == None:
                    # otherwise use the recursive body literal
                    selected_literal = literal

            if selected_literal == None:
                return False

            grounded_variables = grounded_variables.union(selected_literal.outputs)
            body_literals = body_literals.difference({selected_literal})
        return True
    else:
        if all(len(literal.inputs) == 0 for literal in body):
            return True

        body_literals = set(body)
        grounded_variables = set()

        while body_literals:
            selected_literal = None
            for literal in body_literals:
                if len(literal.outputs) == len(literal.arguments):
                    selected_literal = literal
                    break
                if literal.inputs.issubset(grounded_variables):
                    selected_literal = literal
                    break

            if selected_literal == None:
                return False

            grounded_variables = grounded_variables.union(selected_literal.arguments)
            body_literals = body_literals.difference({selected_literal})

        return True

def is_headless(prog):
    return any(head is None for head, body in prog)

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def non_empty_subset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)))