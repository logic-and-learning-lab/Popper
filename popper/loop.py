import time
from collections import defaultdict
from bitarray.util import subset, any_and, ones
from functools import cache
from itertools import chain, combinations, permutations
from . util import timeout, format_rule, rule_is_recursive, prog_is_recursive, prog_has_invention, calc_prog_size, format_literal, Constraint, mdl_score, suppress_stdout_stderr, get_raw_prog, Literal, remap_variables, format_prog, connected, head_connected, theory_subsumes, non_empty_powerset, generalisations
from . tester import Tester
# from . bkcons import deduce_bk_cons, deduce_recalls, deduce_type_cons, deduce_non_singletons, get_bk_cons
from . bkcons import get_bk_cons
from . combine import Combiner
from . unsat import UnsatCoreFinder
from . allsat import AllSatCoreFinder


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

def popper(settings, tester, bkcons):
    """
    Closure-style refactor of the former Popper class.
    Drop the old:
        def popper(...): Popper(...).run(...)
        class Popper: ...
    and replace with THIS single function.
    """

    unsatcore_finder = UnsatCoreFinder(settings, tester)
    allsatcore_finder = AllSatCoreFinder(settings, tester)

    # ----------------------------
    # Former Popper.__init__ state
    # ----------------------------
    pruned2 = set()

    tmp = {}


    # ----------------------------
    # Run-time state (former self.* created inside run)
    # ----------------------------
    num_pos, num_neg = tester.num_pos, tester.num_neg

    # pos_covered_bit_array -> prog_size
    # it only maintains success sets for programs where fp = 0
    success_sets = {}
    success_sets_aux = {}

    # (pos_covered_bit_array, neg_covered_bitarray) -> prog_size
    success_sets_noise = {}

    # pos_covered_bit_array -> prog_size+prog_size2
    # it only maintains success sets for pairs of programs where fp = 0
    paired_success_sets = defaultdict(set)

    covered_by_pos = defaultdict(set)
    covered_by_neg = defaultdict(set)

    # program_hash -> coverage (bit_arrary)
    coverage_pos = {}
    coverage_neg = {}

    cached_prog_size = {}

    prog_lookup = {}

    scores = {}

    to_combine = set()
    min_size = None
    generator = None

    # Only used in noisy mode (initialized later)
    seen_hyp_spec = None
    seen_hyp_gen = None



    def filter_combine_programs(combiner, to_combine_set):
        xs = combiner.saved_progs | to_combine_set
        min_sz = min(cached_prog_size[prog] for prog in xs)
        must_beat = settings.best_mdl - min_sz

        to_delete = set()
        for prog_hash in xs:
            size, tp, fp = scores[prog_hash]
            if fp + size >= must_beat:
                to_delete.add(prog_hash)

        for prog_hash in to_delete:
            if prog_hash in combiner.saved_progs:
                combiner.saved_progs.remove(prog_hash)
            else:
                to_combine_set.remove(prog_hash)

    def check_subsumed_by_two(pos_covered, prog_size):
        for i in range(2, prog_size + 2):
            if pos_covered in paired_success_sets[i]:
                return True
            for x in paired_success_sets[i]:
                if subset(pos_covered, x):
                    return True
        return False

    def check_subsumed_by_two_v2(prog_size, prog2_size, pos_covered, pos_covered2):
        nonlocal min_size
        space = prog2_size - prog_size

        if space < min_size:
            return False
        uncovered = pos_covered2 & ~pos_covered

        for xs, size in success_sets.items():
            if size > space:
                continue
            if subset(xs, uncovered):
                return True
        return False

    def check_covers_too_few(prog_size, pos_covered):
        k1 = hash((prog_size, pos_covered))
        if k1 in tmp:
            v = tmp[k1]
            if v:
                return True

        k2 = hash((prog_size, pos_covered, settings.max_literals, settings.search_depth))
        if k2 in tmp:
            return tmp[k2]

        v = check_covers_too_few_(prog_size, pos_covered)

        tmp[k1] = v
        tmp[k2] = v
        return v

    def check_covers_too_few_(prog_size, pos_covered):
        nonlocal min_size
        len_pos_covered = pos_covered.count(1)
        if len_pos_covered == num_pos:
            return False

        max_literals = settings.max_literals

        # MAX RULES = 1
        if (prog_size + min_size) > max_literals:
            return True

        # MAX RULES = 2
        if (prog_size + (min_size * 2)) > max_literals:
            space_remaining = max_literals - prog_size
            if space_remaining > settings.search_depth:
                return False

            uncovered = tester.pos_examples_ & ~pos_covered
            for pos_covered2, size2 in success_sets.items():
                if size2 > space_remaining:
                    continue
                if subset(uncovered, pos_covered2):
                    return False
            return True

        # MAX RULES = 3
        if (prog_size + (min_size * 3)) > max_literals:
            space_remaining = max_literals - prog_size

            if space_remaining - min_size > settings.search_depth:
                return False

            uncovered = tester.pos_examples_ & ~pos_covered
            success_sets_sorted = sorted(((pos_covered_, size) for (pos_covered_, size) in success_sets.items()),
                                         key=lambda x: x[1])
            n = len(success_sets_sorted)

            for i in range(n):
                pos_covered2, size2 = success_sets_sorted[i]
                if size2 > space_remaining:
                    break
                if subset(uncovered, pos_covered2):
                    return False
                space_remaining_ = space_remaining - size2
                if space_remaining_ < min_size:
                    continue
                uncovered2 = uncovered & ~pos_covered2
                for j in range(i + 1, n):
                    pos_covered3, size3 = success_sets_sorted[j]
                    if size3 > space_remaining_:
                        break
                    if subset(uncovered2, pos_covered3):
                        return False
            return True

        # MAX RULES = 4
        if prog_size + (min_size * 4) > max_literals:
            space_remaining = max_literals - prog_size
            space_remaining -= (min_size * 2)

            if space_remaining > settings.search_depth:
                return False

            missing = tester.pos_examples_ & ~pos_covered

            success_sets_sorted = sorted(((pos_covered_, size) for (pos_covered_, size) in success_sets.items()),
                                         key=lambda x: x[1])
            space_remaining = max_literals - prog_size

            n = len(success_sets_sorted)

            for i in range(n):
                pos_covered2, size2 = success_sets_sorted[i]
                if size2 > space_remaining:
                    break
                if subset(missing, pos_covered2):
                    return False
                space_remaining_ = space_remaining - size2
                if space_remaining_ < min_size:
                    continue
                missing2 = missing & ~pos_covered2
                for j in range(i + 1, n):
                    pos_covered3, size3 = success_sets_sorted[j]
                    if size3 > space_remaining_:
                        break
                    if subset(missing2, pos_covered3):
                        return False
                    space_remaining__ = space_remaining_ - size3
                    if space_remaining__ < min_size:
                        continue
                    missing3 = missing2 & ~pos_covered3
                    for k in range(j + 1, n):
                        pos_covered4, size4 = success_sets_sorted[k]
                        if size4 > space_remaining__:
                            break
                        if subset(missing3, pos_covered4):
                            return False
            return True

        return False


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

        return cons

    def subsumed_or_covers_too_few(prog, seen=None):
        if seen is None:
            seen = set()

        head, body = list(prog)[0]
        body = list(body)

        if len(body) == 0:
            return []

        out = set()
        head_vars = set(head.arguments)

        for i in range(len(body)):
            new_body = body[:i] + body[i + 1:]
            new_body = frozenset(new_body)

            if len(new_body) == 0:
                continue

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
            if any(hash(frozenset(x)) in pruned2 for x in non_empty_powerset(new_body)):
                continue

            if not head_connected(new_rule):
                xs = subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if not settings.has_valid_directions(new_rule):
                xs = subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            if tester.has_redundant_literal(new_prog):
                xs = subsumed_or_covers_too_few(new_prog, seen)
                out.update(xs)
                continue

            new_prog_size = calc_prog_size(new_prog)
            sub_prog_pos_covered = tester.get_pos_covered(new_prog)

            subsumed = sub_prog_pos_covered in success_sets or any(subset(sub_prog_pos_covered, xs) for xs in success_sets)
            subsumed_by_two = not subsumed and check_subsumed_by_two(sub_prog_pos_covered, new_prog_size)
            covers_too_few = not subsumed and not subsumed_by_two and check_covers_too_few(new_prog_size, sub_prog_pos_covered)

            if not (subsumed or subsumed_by_two or covers_too_few):
                continue

            xs = subsumed_or_covers_too_few(new_prog, seen)
            if len(xs) > 0:
                out.update(xs)
                continue

            for x in find_variants(remap_variables(new_rule)):
                pruned2.add(hash(x))

            if subsumed:
                out.add((new_prog, 'SUBSUMED (GENERALISATION)'))
            elif subsumed_by_two:
                out.add((new_prog, 'SUBSUMED BY TWO (GENERALISATION)'))
            elif covers_too_few:
                out.add((new_prog, 'COVERS TOO FEW (GENERALISATION)'))
            else:
                assert False

        return out

    def find_variants(rule):
        head, body = rule
        _head_pred, head_args = head
        head_arity = len(head_args)
        body_vars = frozenset(x for literal in body for x in literal.arguments if x >= head_arity)
        subset_vars = range(head_arity, settings.max_vars)
        for xs in permutations(subset_vars, len(body_vars)):
            xs = head_args + xs
            new_body = []
            for pred, args in body:
                new_args = tuple(xs[arg] for arg in args)
                new_literal = (pred, new_args)
                new_body.append(new_literal)
            yield frozenset(new_body)

    def needs_datalog(prog):
        if not settings.has_directions:
            return False
        for rule in prog:
            rec_outputs = set()
            non_rec_inputs = set()
            head, body = rule
            head_pred, _head_args = head
            for literal in body:
                pred, args = literal
                if pred == head_pred:
                    literal_outputs = settings.literal_outputs[(pred, args)]
                    rec_outputs.update(literal_outputs)
                else:
                    literal_inputs = settings.literal_inputs[(pred, args)]
                    non_rec_inputs.update(literal_inputs)
            if any(x in rec_outputs for x in non_rec_inputs):
                return True
        return False


    # ----------------------------
    # Former Popper.run body
    # ----------------------------
    uncovered = ones(num_pos)

    if settings.noisy:
        min_score = None
        saved_scores = dict()
        settings.best_prog_score = (0, num_pos, num_neg, 0, 0)
        settings.best_mdl = num_pos
        # save hypotheses for which we pruned spec / gen from a certain size only
        # once we update the best mdl score, we can prune spec / gen from a better size for some of these
        seen_hyp_spec, seen_hyp_gen = defaultdict(list), defaultdict(list)
        max_size = min((1 + settings.max_body) * settings.max_rules, num_pos)
    else:
        max_size = (1 + settings.max_body) * settings.max_rules

    settings.max_size = max_size

    # generator that builds programs
    with settings.stats.duration('init'):
        if settings.single_solve:
            from .gen2 import Generator
        elif settings.max_rules == 2 and not settings.pi_enabled:
            from .gen3 import Generator
        else:
            from .generate import Generator
        generator = Generator(settings, bkcons)

    combiner = load_solver(settings, tester, coverage_pos, coverage_neg, prog_lookup)

    last_size = None
    min_coverage = settings.min_coverage = 1

    for size in range(2, max_size + 1):
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
                    combiner.best_cost = mdl
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

            if tp > 0 and success_sets and (not settings.noisy or (settings.noisy and fp == 0)):
                with settings.stats.duration('check subsumed and covers_too_few'):
                    subsumed = pos_covered in success_sets or any(subset(pos_covered, xs) for xs in success_sets)
                    subsumed_by_two = (not subsumed) and check_subsumed_by_two(pos_covered, prog_size)
                    covers_too_few = (not subsumed) and (not subsumed_by_two) and (not settings.noisy) and check_covers_too_few(prog_size, pos_covered)

                if subsumed or subsumed_by_two or covers_too_few:
                    add_spec = True
                    noisy_subsumed = True

            if (not settings.noisy) and (not has_invention) and (not is_recursive) and (subsumed or subsumed_by_two or covers_too_few):
                subsumed_progs = []
                with settings.stats.duration('find most general subsumed/covers_too_few'):
                    subsumed_progs = subsumed_or_covers_too_few(prog, seen=set())
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
                    add_gen = True
                    if is_recursive:
                        combiner.add_inconsistent(prog)
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

            add_to_combiner = False

            if settings.noisy and (not skipped) and (not skip_early_neg) and (not is_recursive) and (not has_invention) and tp > prog_size + fp and fp + prog_size < settings.best_mdl and (not noisy_subsumed):
                local_delete = set()
                ignore_this_prog = (pos_covered, neg_covered) in success_sets_noise

                if not ignore_this_prog:
                    s_pos = set.intersection(*(covered_by_pos[ex] for ex, ex_cov in enumerate(pos_covered) if ex_cov == 1))
                    for prog1 in s_pos:
                        n1 = coverage_neg[prog1]
                        if subset(n1, neg_covered):
                            ignore_this_prog = True
                            break

                if not ignore_this_prog and (inconsistent or fp > 0):
                    s_neg = set.intersection(*(covered_by_neg[ex] for ex, ex_cov in enumerate(neg_covered) if ex_cov == 1))
                    for prog1 in s_neg:
                        if subset(coverage_pos[prog1], pos_covered):
                            size1, tp1, fp1 = scores[prog1]
                            if size1 == prog_size:
                                local_delete.add(prog1)
                                continue

                            if fp == fp1 and (tp - prog_size) < (tp1 - size1):
                                ignore_this_prog = True
                                break

                            if tp == tp1 and (fp + prog_size) >= (fp1 + size1):
                                ignore_this_prog = True
                                break

                            if (tp - fp - prog_size) <= (tp1 - fp1 - size1):
                                ignore_this_prog = True
                                break

                if not inconsistent:
                    not_covered = tester.pos_examples_ ^ pos_covered
                    progs_not_subsumed = set.union(*(covered_by_pos[ex] for ex, ex_cov in enumerate(not_covered) if ex_cov == 1))
                    all_progs = set.union(*(covered_by_pos[ex] for ex, ex_cov in enumerate(tester.pos_examples_) if ex_cov == 1))
                    s_pos2 = all_progs.difference(progs_not_subsumed)
                    for prog1 in s_pos2:
                        size1, tp1, fp1 = scores[prog1]
                        if size1 >= prog_size:
                            local_delete.add(prog1)

                for k in local_delete:
                    assert k in (combiner.saved_progs | to_combine)
                    if k in combiner.saved_progs:
                        combiner.saved_progs.remove(k)
                    elif k in to_combine:
                        to_combine.remove(k)

                    k_pos, k_neg = coverage_pos[k], coverage_neg[k]
                    del success_sets_noise[(k_pos, k_neg)]
                    for ex, ex_cov in enumerate(k_pos):
                        if ex_cov == 1:
                            covered_by_pos[ex].remove(k)
                    for ex, ex_cov in enumerate(k_neg):
                        if ex_cov == 1:
                            covered_by_neg[ex].remove(k)
                    del coverage_pos[k]
                    del coverage_neg[k]
                    del scores[k]
                    del cached_prog_size[k]
                    del prog_lookup[k]

                if not ignore_this_prog:
                    success_sets_noise[(pos_covered, neg_covered)] = (prog, prog_size, fn, fp, tp)
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
                    scores[k] = (prog_size, tp, fp)
                    prog_lookup[k] = prog

                    if fp == 0:
                        success_sets[pos_covered] = prog_size
                        for p, s in success_sets.items():
                            if p == pos_covered:
                                continue
                            paired_success_sets[s + prog_size].add(p | pos_covered)

            elif not settings.noisy:
                if (not inconsistent) and (not subsumed) and (not add_gen) and tp > 0 and (not pruned_more_general):
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
                                assert False
                            del success_sets[pos_covered2]
                            del success_sets_aux[pos_covered2]
                            del coverage_pos[prog2_hash]
                            del coverage_neg[prog2_hash]
                            del prog_lookup[prog2_hash]

                    k = hash(prog)
                    success_sets[pos_covered] = prog_size
                    success_sets_aux[pos_covered] = k
                    coverage_pos[k] = pos_covered
                    coverage_neg[k] = neg_covered
                    prog_lookup[k] = prog

                    for p, s in success_sets.items():
                        if p == pos_covered:
                            continue
                        paired_success_sets[s + prog_size].add(p | pos_covered)

                    if min_size is None:
                        min_size = prog_size

            if add_to_combiner:
                to_combine.add(hash(prog))

            call_combine = len(to_combine) > 0
            call_combine = call_combine and (settings.noisy or settings.solution_found)
            call_combine = call_combine and (len(to_combine) >= settings.batch_size or size_change)

            if add_to_combiner and (not settings.noisy) and (not settings.solution_found) and (not settings.recursion_enabled):
                if any_and(uncovered, pos_covered):
                    if settings.solution:
                        settings.solution = settings.solution | prog
                    else:
                        settings.solution = prog
                    uncovered = uncovered & ~pos_covered
                    tp2 = num_pos - uncovered.count(1)
                    fn2 = uncovered.count(1)
                    tn2 = num_neg
                    fp2 = 0
                    hypothesis_size = calc_prog_size(settings.solution)
                    settings.best_prog_score = (tp2, fn2, tn2, fp2, hypothesis_size)
                    settings.print_incomplete_solution2(settings.solution, tp2, fn2, tn2, fp2, hypothesis_size)

                    if not uncovered.any():
                        settings.solution_found = True
                        settings.max_literals = hypothesis_size - 1
                        min_coverage = settings.min_coverage = 2

                        for i in range(settings.max_literals + 1, max_size + 1):
                            generator.prune_size(i)

                    call_combine = not uncovered.any()

            if call_combine:
                if settings.noisy:
                    filter_combine_programs(combiner, to_combine)

                with settings.stats.duration('combine'):
                    is_new_solution_found = combiner.update_best_prog(to_combine)

                to_combine = set()

                new_hypothesis_found = is_new_solution_found is not None

                if new_hypothesis_found:
                    new_hypothesis, conf_matrix = is_new_solution_found
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
                            for i in range(best_score, max_size + 1):
                                generator.prune_size(i)

                    if (not settings.noisy) and fp3 == 0 and fn3 == 0:
                        settings.solution_found = True
                        settings.max_literals = hypothesis_size - 1
                        min_coverage = settings.min_coverage = 2

                        if size >= settings.max_literals:
                            print('POOPER')
                            return

                        for i in range(hypothesis_size, max_size + 1):
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
                is_new_solution_found = combiner.update_best_prog(to_combine)
            to_combine = set()

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

