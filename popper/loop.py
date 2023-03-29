import time
import numbers
from . hasse import Hasse
from . combine import Combiner
from . explain import Explainer, rule_hash, head_connected, find_subprogs, get_raw_prog, seen_more_general_unsat, seen_more_specific_sat, prog_hash, has_valid_directions
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, prog_has_invention, order_rule, prog_size, format_literal, theory_subsumes, rule_subsumes
from . tester import Tester
from . generate import Generator, Grounder, parse_model, atom_to_symbol, arg_to_symbol
from . bkcons import deduce_bk_cons, deduce_recalls

seen_cons = set()
cached_pos_covered2 = {}
seen_parse_handles = {}

def parse_handles(generator, new_handles):
    for rule in new_handles:
        head, body = rule
        for h, b in generator.get_ground_rules(rule):
            _, p, args = h
            out_h = (p, args)
            out_b = frozenset((b_pred, b_args) for _, b_pred, b_args in b)
            yield (out_h, out_b)

def explain_incomplete(settings, generator, explainer, prog, directions, new_cons, all_handles, bad_handles, new_ground_cons, hasse2):
    pruned_subprog = False

    for subprog, unsat_body in explainer.explain_totally_incomplete(prog, directions):
        pruned_subprog = True



        if unsat_body:
            _, body = subprog[0]
            con = generator.unsat_constraint(body)
            for h, b in generator.get_ground_deep_rules(con):
                new_ground_cons.add(b)
            continue

        # assert(not hasse2.generalises_child(subprog))

        new_rule_handles, con = generator.build_specialisation_constraint(subprog)
        new_cons.add(con)

        if not settings.single_solve:
            all_handles.update(parse_handles(generator, new_rule_handles))

        if not settings.recursion_enabled or settings.pi_enabled:
            continue

        if len(subprog) == 1:
            bad_handle, new_rule_handles, con = generator.redundancy_constraint1(subprog)
            bad_handles.add(bad_handle)
            new_cons.add(con)
            if not settings.single_solve:
                all_handles.update(parse_handles(generator, new_rule_handles))

        handles, cons = generator.redundancy_constraint2(subprog)
        new_cons.update(cons)
        if not settings.single_solve:
            all_handles.update(parse_handles(generator, handles))

    return pruned_subprog

def explain_inconsistent(settings, generator, tester, prog, rule_ordering, new_cons, all_handles):
    if len(prog) == 1 or not settings.recursion_enabled:
        return False

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
            new_rule_handles, con = generator.build_generalisation_constraint(subprog)
            new_cons.add(con)
            if not settings.single_solve:
                all_handles.update(parse_handles(generator, new_rule_handles))
            pruned_subprog = True

    if pruned_subprog:
        return True

    if len(rec) == 1:
        return False

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_inconsistent(subprog):
                new_rule_handles, con = generator.build_generalisation_constraint(subprog)
                new_cons.add(con)
                if not settings.single_solve:
                    all_handles.update(parse_handles(generator, new_rule_handles))
                pruned_subprog = True
    return pruned_subprog

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

# seen_can_be_reduced = {}
# def has_tautology(prog, tester, settings, generator, new_cons, d=1):
#     # return False
#     rule = list(prog)[0]
#     head, body = rule

#     k = prog_hash(prog)
#     if k in seen_can_be_reduced:
#         return seen_can_be_reduced[k]

#     if len(body) == 1:
#         return False

#     body = tuple(body)

#     head_vars = set(head.arguments)
#     rule_vars = set()
#     rule_vars.update(head_vars)

#     for lit in body:
#         rule_vars.update(lit.arguments)

#     fetched_thing = False

#     # pos_covered = tester.get_pos_covered(prog)
#      # = tester.is_inconsistent(prog)

#     found_smaller = False

#     for i in range(len(body)):
#         new_body = body[:i] + body[i+1:]

#         new_vars = set()
#         new_body_vars = set()

#         for lit in new_body:
#             new_body_vars.update(lit.arguments)

#         new_vars.update(head_vars)
#         new_vars.update(new_body_vars)

#         # if not new_vars.issubset(rule_vars):
#             # continue

#         if not head_vars.issubset(new_body_vars):
#             continue

#         # if len(new_body) < 2:
#             # continue

#         var_count = {x:1 for x in head_vars}

#         # for lit in new_body:
#         #     for x in lit.arguments:
#         #         if x in var_count:
#         #             var_count[x] += 1
#         #         else:
#         #             var_count[x] = 1

#         # if any(v == 1 for v in var_count.values()):
#             # continue

#         new_rule = (head, frozenset(new_body))
#         new_prog = frozenset([new_rule])
#         new_k = prog_hash(new_prog)

#         if not tester.is_inconsistent(new_prog):
#             # seen_can_be_reduced[new_k] = True
#             print('\t'*d, 'can prune', format_rule(new_rule))
#             if not check_can_be_reduced(new_prog, tester, settings, generator, new_cons, d+1):
#                 new_rule_handles, con = generator.build_specialisation_constraint(new_prog)
#                 ground_rules = generator.get_ground_rules((None, con))
#                 print('\t'*d, 'pruning', format_rule(new_rule))
#                 # for _,r in ground_rules:
#                     # print(r)
#                     # print(format_literal(x))
#                 new_cons.add(con)
#                 assert(con not in seen_cons)
#                 seen_cons.add(con)
#             found_smaller = True

#     seen_can_be_reduced[k] = found_smaller
#     return found_smaller


# seen_can_be_reduced = {}
# def check_can_be_reduced(prog, tester, settings, generator, new_cons, d=1):
#     # return False
#     rule = list(prog)[0]
#     head, body = rule

#     k = prog_hash(prog)
#     if k in seen_can_be_reduced:
#         return seen_can_be_reduced[k]

#     if len(body) == 1:
#         return False

#     body = tuple(body)

#     head_vars = set(head.arguments)
#     rule_vars = set()
#     rule_vars.update(head_vars)

#     for lit in body:
#         rule_vars.update(lit.arguments)

#     fetched_thing = False

#     # pos_covered = tester.get_pos_covered(prog)
#      # = tester.is_inconsistent(prog)

#     found_smaller = False

#     for i in range(len(body)):
#         new_body = body[:i] + body[i+1:]

#         new_vars = set()
#         new_body_vars = set()

#         for lit in new_body:
#             new_body_vars.update(lit.arguments)

#         new_vars.update(head_vars)
#         new_vars.update(new_body_vars)

#         # if not new_vars.issubset(rule_vars):
#             # continue

#         if not head_vars.issubset(new_body_vars):
#             continue

#         # if len(new_body) < 2:
#             # continue

#         var_count = {x:1 for x in head_vars}

#         # for lit in new_body:
#         #     for x in lit.arguments:
#         #         if x in var_count:
#         #             var_count[x] += 1
#         #         else:
#         #             var_count[x] = 1

#         # if any(v == 1 for v in var_count.values()):
#             # continue

#         new_rule = (head, frozenset(new_body))
#         new_prog = frozenset([new_rule])
#         new_k = prog_hash(new_prog)

#         if not tester.is_inconsistent(new_prog):
#             # seen_can_be_reduced[new_k] = True
#             print('\t'*d, 'can prune', format_rule(new_rule))
#             if not check_can_be_reduced(new_prog, tester, settings, generator, new_cons, d+1):
#                 new_rule_handles, con = generator.build_specialisation_constraint(new_prog)
#                 ground_rules = generator.get_ground_rules((None, con))
#                 print('\t'*d, 'pruning', format_rule(new_rule))
#                 # for _,r in ground_rules:
#                     # print(r)
#                     # print(format_literal(x))
#                 assert(con not in seen_cons)
#                 seen_cons.add(con)
#                 new_cons.add(con)
#             found_smaller = True

#     seen_can_be_reduced[k] = found_smaller
#     return found_smaller

seen_shit_subprog = set()
# @profile
def find_most_general_shit_subrule(prog, tester, settings, min_coverage, generator, new_cons, all_handles, d=1, seen_poo=set(), seen_ok=set()):

    rule = list(prog)[0]
    head, body = rule

    if len(body) == 0:
        return None

    body = tuple(body)

    pruned_subprog = None

    head_vars = set(head.arguments)
    rule_vars = set()
    rule_vars.update(head_vars)

    for lit in body:
        rule_vars.update(lit.arguments)

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_rule = (head, new_body)
        subprog = frozenset([(head, frozenset(new_body))])
        raw_subprog = get_raw_prog(subprog)

        if len(new_body) == 0:
            continue

        k = prog_hash(subprog)

        if k in seen_shit_subprog:
            # print('skip')
            continue
        seen_shit_subprog.add(k)

        new_vars = set()
        new_body_vars = set()

        for lit in new_body:
            new_body_vars.update(lit.arguments)

        new_vars.update(head_vars)
        new_vars.update(new_body_vars)

        if not head_connected(new_rule):
            continue

        # if not new_vars.issubset(rule_vars):
        #     pass

        # if not head_vars.issubset(new_body_vars):
        #     pass

        if not has_valid_directions(new_rule):
            continue

            # pass
                # TMP1

        # for rule in subprog:
        #     print('\tsub', format_rule(rule))

        pos_covered = tester.get_pos_covered(subprog)
        # for rule in subprog:
            # print(min_coverage, d, format_rule(rule), len(pos_covered))

        # var_count = {x:1 for x in head_vars}
        # for lit in new_body:
        #     for x in lit.arguments:
        #         if x in var_count:
        #             var_count[x] += 1
        #         else:
        #             var_count[x] = 1
        # if any(v == 1 for v in var_count.values()):
        #     pass

        if tester.has_redundant_literal(subprog):
            x = find_most_general_shit_subrule(subprog, tester, settings, min_coverage, generator, new_cons, all_handles, d+1, seen_poo, seen_ok)
            if x != None:
                pruned_subprog = x
            continue

        # SLOW BUT SHOULD HELP
        if seen_more_general_unsat(raw_subprog, seen_poo):
            continue

        # # SLOW BUT SHOULD HELP
        # if seen_more_specific_sat(raw_subprog, seen_ok):
        #     continue

        # print('\ttesting')
        pos_covered = tester.get_pos_covered(subprog)
        if len(pos_covered) <= min_coverage:
            pruned = True
            pruned_subprog = subprog
            seen_poo.add(raw_subprog)
            x = find_most_general_shit_subrule(subprog, tester, settings, min_coverage, generator, new_cons, all_handles, d+1, seen_poo, seen_ok)
            if x != None:
                pruned_subprog = x
            else:
                new_rule_handles, con = generator.build_specialisation_constraint(subprog)
                new_cons.add(con)
                # assert(con not in seen_cons)
                # seen_cons.add(con)
                # print('\t\tprune', format_rule(new_rule))
                # print(format_rule(new_rule))
                if not settings.single_solve:
                    all_handles.update(parse_handles(generator, new_rule_handles))
        else:
            seen_ok.add(raw_subprog)
    return pruned_subprog

def constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model, new_ground_cons):
    all_ground_cons.update(new_ground_cons)
    ground_bodies = set()
    ground_bodies.update(new_ground_cons)

    for con in new_cons:
        ground_rules = generator.get_ground_rules((None, con))
        for ground_rule in ground_rules:
            _ground_head, ground_body = ground_rule
            ground_bodies.add(ground_body)
            all_ground_cons.add(frozenset(ground_body))
    for ground_body in ground_bodies:
        nogood = []
        for sign, pred, args in ground_body:
            k = hash((sign, pred, args))
            if k in cached_clingo_atoms:
                nogood.append(cached_clingo_atoms[k])
            else:
                x = (atom_to_symbol(pred, args), sign)
                cached_clingo_atoms[k] = x
                nogood.append(x)
        model.context.add_nogood(nogood)

def get_min_pos_coverage(combiner, cached_pos_covered):
    min_coverage = 1
    for x in combiner.best_prog:
        # k = prog_hash(x)
        if x in cached_pos_covered:
            # assert(k in cached_pos_covered2)
            v = len(cached_pos_covered[x])
            if v > min_coverage:
                min_coverage = v
    return min_coverage

# def prune_smaller_backtrack(cached_pos_covered, combiner, could_prune_later, generator, new_cons, all_handles, settings, tester, seen_tmp):
#     # min_coverage = min(len(cached_pos_covered[x]) for x in combiner.best_prog)

#     min_coverage = get_min_pos_coverage(combiner, cached_pos_covered)

#     to_dump = set()
#     # print('most gen2')
#     seen_poo = set()
#     for prog2, num_covers2 in could_prune_later.items():
#         if num_covers2 <= min_coverage:
#             to_dump.add(prog2)
#             # print('prune_smaller_backtrack')
#             # for rule in order_prog(prog2):
#                 # print('\t',format_rule(order_rule(rule)))
#             has_smaller=False
#             with settings.stats.duration('most gen2'):
#                 has_smaller = find_most_general_shit_subrule(prog2, tester, settings, min_coverage, generator, new_cons, all_handles, seen_poo=seen_poo)
#                 # if has_smaller:
#                     # print(has_smaller)
#             if not has_smaller:
#                 new_handles, con = generator.build_specialisation_constraint(prog2)
#                 new_cons.add(con)
#                 assert(con not in seen_cons)
#                 seen_cons.add(con)
#                 if not settings.single_solve:
#                     parsed_handles = parse_handles(generator, new_handles)
#                     all_handles.update(parsed_handles)
#     for x in to_dump:
#         del could_prune_later[x]
#         if x in seen_tmp:
#             del seen_tmp[x]

def prune_smaller_backtrack2(cached_pos_covered, combiner, generator, new_cons, all_handles, settings, tester, hasse):
    # print('ASDA!!!!!!!')
    # hasse.print()
    # exit()

    min_coverage = get_min_pos_coverage(combiner, cached_pos_covered)
    to_dump = set()
    # print('most gen2')
    seen_poo = set()

    # print('A')
    # for node in hasse.children:
    #     print('min_coverage',min_coverage, 'num_covers2',len(node.pos_covered))
    # # print('B')

    pruned = set()
    def loop(nodes):
        # SKIP PRUNED!!!
        for node in nodes:
            h, b = list(node.raw_prog)[0]
            # print('loop node', sorted(b))
            if node.raw_prog in pruned:
                # print('SKIP')
                continue
            num_covers2 = len(node.pos_covered)
            if num_covers2 <= min_coverage:
                prog2 = hasse.node_to_prog[node]
                pruned.add(node)
                # hasse.remove_prog(prog2)
                pruned_smaller = None
                with settings.stats.duration('most gen2'):
                    pruned_smaller = find_most_general_shit_subrule(prog2, tester, settings, min_coverage, generator, new_cons, all_handles, seen_poo=seen_poo)
                    if pruned_smaller != None:
                        specialisation_raw_progs = hasse.prune_specialisations(pruned_smaller)
                        pruned.update(specialisation_raw_progs)
                        # print('pruned_smaller!!!')
                        print(f'prune_smaller_backtrack2 smaller')
                        for rule in pruned_smaller:
                            print('\t', format_rule(rule))
                        # for x in specialisation_raw_progs:
                        #     _, b = list(x)[0]
                        #     print('\t\t', 'specialisation', sorted(b))
                if pruned_smaller == None:
                    # hasse.remove_prog(prog2)
                    # print(f'prune_smaller_backtrack2 min_coverage:{min_coverage} num_covers2:{num_covers2}')
                    print(f'prune_smaller_backtrack2')
                    for rule in order_prog(prog2):
                        print('\t',format_rule(order_rule(rule)))
                    new_handles, con = generator.build_specialisation_constraint(prog2)
                    new_cons.add(con)
                    # assert(con not in seen_cons)
                    # seen_cons.add(con)
                    if not settings.single_solve:
                        parsed_handles = parse_handles(generator, new_handles)
                        all_handles.update(parsed_handles)
            else:
                loop(node.children)
    loop(hasse.children)
    # exit()
    # for prog2 in pruned:
        # y = hasse.remove_prog(prog2)

def prune_subsumed_backtrack(pos_covered, combiner, generator, new_cons, all_handles, settings, seen_tmp, could_prune_later):
    to_dump = set()
    # TODO: ONLY PRUNE THE MINIMAL ONE
    # print('a')
    # for rule in prog:
        # print('\t',format_rule(rule))
    print('seen_tmp.items',len(seen_tmp))
    for prog2, covered2 in seen_tmp.items():
        if covered2.issubset(pos_covered):
            # print('pruned subsumed backtrack')
            # for rule in order_prog(prog2):
            #     print('\t',format_rule(order_rule(rule)))
            to_dump.add(prog2)
            new_handles, con = generator.build_specialisation_constraint(prog2)
            new_cons.add(con)
            assert(con not in seen_cons)
            seen_cons.add(con)
            if not settings.single_solve:
                all_handles.update(parse_handles(generator, new_handles))
    for x in to_dump:
        del seen_tmp[x]
        if x in could_prune_later:
            del could_prune_later[x]

def prune_subsumed_backtrack2(pos_covered, combiner, generator, new_cons, all_handles, settings, tester, hasse):
    to_dump = set()
    seen_poo = set()

    def loop(nodes):
        for node in nodes:
            # num_covers2 = len(node.pos_covered)
            if node.pos_covered.issubset(pos_covered):
                prog2 = hasse.node_to_prog[node]
                to_dump.add(prog2)
                # print('prune_subsumed_backtrack2')
                # print(prog2)
                # for rule in order_prog(prog2):
                    # print('\t',format_rule(order_rule(rule)))
                has_smaller = None
                # with settings.stats.duration('most gen2'):
                    # has_smaller = find_most_general_shit_subrule(prog2, tester, settings, min_coverage, generator, new_cons, all_handles, seen_poo=seen_poo)
                #     # has_smaller = find_most_general_shit_subrule(prog2, tester, settings, min_coverage, generator, new_cons, all_handles, seen_poo=seen_poo)
                    # if has_smaller != None:
                        # print(has_smaller)
                #         assert(False)
                if has_smaller == None:
                    new_handles, con = generator.build_specialisation_constraint(prog2)
                    new_cons.add(con)
                    # assert(con not in seen_cons)
                    # seen_cons.add(con)
                    if not settings.single_solve:
                        parsed_handles = parse_handles(generator, new_handles)
                        all_handles.update(parsed_handles)
            else:
                loop(node.children)
    loop(hasse.children)
    for prog2 in to_dump:
        y = hasse.remove_prog(prog2)

# @profile
def build_constraints(settings, generator, new_cons, new_rule_handles, add_spec, add_gen, add_redund1, add_redund2, pruned_sub_incomplete, pruned_more_general_shit, pruned_sub_inconsistent, all_handles, bad_handles, prog, rule_ordering):
    # with settings.stats.duration('build_constraints'):
    # BUILD CONSTRAINTS
    if add_spec and not pruned_sub_incomplete and not pruned_more_general_shit:
        handles, con = generator.build_specialisation_constraint(prog, rule_ordering)
        if not settings.single_solve:
            new_rule_handles.update(handles)
        new_cons.add(con)
        assert(con not in seen_cons)
        seen_cons.add(con)

    if add_gen and not pruned_sub_inconsistent:
        if settings.recursion_enabled or settings.pi_enabled or not pruned_sub_incomplete:
            handles, con = generator.build_generalisation_constraint(prog, rule_ordering)
            if not settings.single_solve:
                new_rule_handles.update(handles)
            new_cons.add(con)
            assert(con not in seen_cons)
            seen_cons.add(con)

    if add_redund1 and not pruned_sub_incomplete:
        bad_handle, handles, con = generator.redundancy_constraint1(prog)
        bad_handles.add(bad_handle)
        if not settings.single_solve:
            new_rule_handles.update(handles)
        new_cons.add(con)
        assert(con not in seen_cons)
        seen_cons.add(con)

    if add_redund2 and not pruned_sub_incomplete:
        handles, cons = generator.redundancy_constraint2(prog, rule_ordering)
        if not settings.single_solve:
            new_rule_handles.update(handles)
        new_cons.update(cons)
        # for x in cons:
        #     if con in seen_cons
        #     seen_cons.add(con)

    # if pi or rec, save the constraints and handles for the next program size
    if not settings.single_solve:
        parsed_handles = list(parse_handles(generator, new_rule_handles))
        # print(len(all_handles), len(parsed_handles))
        all_handles.update(parsed_handles)

def popper(settings):
    tester = Tester(settings)
    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            deduce_bk_cons(settings, tester)
        with settings.stats.duration('recalls'):
            deduce_recalls(settings)

    explainer = Explainer(settings, tester)
    grounder = Grounder(settings)
    combiner = Combiner(settings, tester)

    settings.single_solve = not (settings.recursion_enabled or settings.pi_enabled)

    num_pos = len(settings.pos_index)

    # track the success sets of tested hypotheses
    success_sets = {}
    rec_success_sets = {}
    last_size = None

    # caching
    cached_clingo_atoms = {}

    # for micro-optimisations
    seen_covers_only_one_gen = set()
    seen_covers_only_one_spec = set()
    seen_incomplete_gen = set()
    seen_incomplete_spec = set()

    # constraints generated
    all_ground_cons = set()
    # messy stuff
    new_ground_cons = set()
    # new rules added to the solver, such as: seen(id):- head_literal(...), body_literal(...)
    all_handles = set()
    # handles for rules that are minimal and unsatisfiable
    bad_handles = set()

    # generator that builds programs
    generator = Generator(settings, grounder)

    # tmp_covered = set()
    cached_pos_covered = {}

    # could_prune_later = {}
    # seen_tmp = {}

    # hasse = Hasse()
    hasse2 = Hasse()

    count_check_redundant_literal2 = 0
    max_size = (1 + settings.max_body) * settings.max_rules
    maxtime = None
    for size in range(1, max_size+1):
        if size > settings.max_literals:
            break

        # code is odd/crap:
        # if there is no PI or recursion, we only add nogoods
        # otherwise we build constraints and add them as nogoods and then again as constraints to the solver
        if not settings.single_solve:
            settings.logger.info(f'SIZE: {size} MAX_SIZE: {settings.max_literals}')
            generator.update_number_of_literals(size)

            with settings.stats.duration('init'):
                generator.update_solver(size, all_handles, bad_handles, all_ground_cons)

        all_ground_cons = set()
        all_handles = set()
        # all_handles_seen = set()
        # all_handles = set()
        bad_handles = set()

        with generator.solver.solve(yield_ = True) as handle:
            # use iter so that we can measure running time
            handle = iter(handle)

            while True:
                new_cons = set()
                new_rule_handles = set()
                new_ground_cons = set()
                pruned_sub_incomplete = False
                pruned_sub_inconsistent = False
                pruned_more_general_shit = False

                add_spec = False
                add_gen = False
                add_redund1 = False
                add_redund2 = False

                # GENERATE A PROGRAM
                with settings.stats.duration('generate'):
                    # get the next model from the solver
                    model = next(handle, None)
                    if model is None:
                        break
                    atoms = model.symbols(shown = True)
                    prog, rule_ordering, directions = parse_model(atoms)

                if settings.debug:
                    settings.logger.debug(f'Program {settings.stats.total_programs}:')
                    for rule in order_prog(prog):
                        settings.logger.debug(format_rule(order_rule(rule)))

                is_recursive = settings.recursion_enabled and prog_is_recursive(prog)
                has_invention = settings.pi_enabled and prog_has_invention(prog)

                settings.stats.total_programs += 1

                # TEST A PROGRAM
                with settings.stats.duration('test'):
                    pos_covered, inconsistent = tester.test_prog(prog)

                num_pos_covered = len(pos_covered)

                if len(prog) == 1:
                    cached_pos_covered[list(prog)[0]] = pos_covered

                # FIND MUCS
                if not has_invention:
                    explainer.add_seen(prog)
                    if num_pos_covered == 0:
                        with settings.stats.duration('find mucs'):
                            pruned_sub_incomplete = explain_incomplete(settings, generator, explainer, prog, directions, new_cons, all_handles, bad_handles, new_ground_cons, hasse2)
                    elif combiner.solution_found:
                        pass
                        # min_coverage = get_min_pos_coverage(combiner, cached_pos_covered)
                        # # PUT BACK!!!!!!
                        # if len(pos_covered) <= min_coverage:
                        #     pruned_more_general_shit = find_most_general_shit_subrule(prog, tester, settings, min_coverage, generator, new_cons, all_handles)
                        #     print('MOOOO', pruned_more_general_shit)

                if inconsistent and is_recursive:
                    combiner.add_inconsistent(prog)

                # messy way to track program size
                if settings.single_solve:
                    k = prog_size(prog)
                    if last_size == None or k != last_size:
                        last_size = k
                        settings.logger.info(f'Searching programs of size: {k}')
                    if last_size > settings.max_literals:
                        return

                if inconsistent:
                    # if inconsistent, prune generalisations
                    add_gen = True
                    if is_recursive:
                        with settings.stats.duration('find sub inconsistent'):
                            pruned_sub_inconsistent = explain_inconsistent(settings, generator, tester, prog, rule_ordering, new_cons, all_handles)

                else:
                    # if consistent, prune specialisations
                    add_spec = True
                    # with settings.stats.duration('reduce consistent'):
                    #     # print(format_rule(list(prog)[0]))
                    #     check_can_be_reduced(prog, tester, settings, generator, new_cons)

                # if consistent and partially complete test whether functional
                if not inconsistent and settings.functional_test and num_pos_covered > 0 and tester.is_non_functional(prog):
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
                                new_handles, con = generator.build_generalisation_constraint([rule])
                                new_cons.add(con)
                                # assert(con not in seen_cons)
                                # seen_cons.add(con)
                                if not settings.single_solve:
                                    all_handles.update(parse_handles(generator, new_handles))

                # remove a subset of theta-subsumed rules when learning recursive programs with more than two rules
                if settings.max_rules > 2 and is_recursive:
                    for x in generator.andy_tmp_con(prog):
                        # assert(x not in seen_cons)
                        # seen_cons.add(x)
                        new_cons.add(x)

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
                        new_handles, con = generator.build_generalisation_constraint([r1,r2])
                        new_cons.add(con)
                        # assert(con not in seen_cons)
                        # seen_cons.add(con)
                        all_handles.update(parse_handles(generator, new_handles))

                # check whether subsumed by a seen program
                subsumed = False

                # WHY DO WE HAVE A RECURSIVE CHECK???
                if num_pos_covered > 0 and not is_recursive:
                    with settings.stats.duration('check_subsumed'):
                    # if num_pos_covered > 0:
                        # AC LOOK AT THIS!!!!!!
                        # assert(add_spec == FalsSe)
                        subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
                        # if so, prune specialisations
                        if subsumed:
                            add_spec = True

                # micro-optimisiations
                # if not settings.recursion_enabled:
                if True:

                    # if we already have a solution, a new rule must cover at least two examples
                    if not add_spec and combiner.solution_found and num_pos_covered == 1:
                        add_spec = True

                    # keep track of programs that only cover one example
                    # once we find a solution, we apply specialisation/generalisation constraints
                    if num_pos_covered == 1:
                        if not add_gen:
                            seen_covers_only_one_gen.add(prog)
                        if not add_spec:
                            seen_covers_only_one_spec.add(prog)

                    # keep track of programs that do not cover all the examples
                    if num_pos_covered != num_pos:
                        if not add_gen:
                            seen_incomplete_gen.add(prog)
                        if not add_spec:
                            seen_incomplete_spec.add(prog)

                    # if we find a solution, prune programs that only cover one example
                    # reset the sets to avoid adding duplicate constraints
                    if combiner.solution_found:
                        for x in seen_covers_only_one_gen:
                            new_handles, con = generator.build_generalisation_constraint(x)
                            new_cons.add(con)
                            # assert(con not in seen_cons)
                            # seen_cons.add(con)
                            if not settings.single_solve:
                                all_handles.update(parse_handles(generator, new_handles))

                            if hasse2.seen_prog(x):
                                hasse2.remove_prog(x)

                        seen_covers_only_one_gen = set()
                        for x in seen_covers_only_one_spec:
                            new_handles, con = generator.build_specialisation_constraint(x)
                            new_cons.add(con)
                            if not settings.single_solve:
                                all_handles.update(parse_handles(generator, new_handles))
                            if hasse2.seen_prog(x):
                                hasse2.remove_prog(x)
                        seen_covers_only_one_spec = set()

                        if len(combiner.best_prog) <= 2:
                            for x in seen_incomplete_gen:
                                new_handles, con = generator.build_generalisation_constraint(x)
                                new_cons.add(con)
                                # assert(con not in seen_cons)
                                # seen_cons.add(con)
                                if not settings.single_solve:
                                    all_handles.update(parse_handles(generator, new_handles))
                                if hasse2.seen_prog(x):
                                    hasse2.remove_prog(x)
                                # if x in seen_tmp:
                                    # print('asda4.2')
                            for x in seen_incomplete_spec:
                                new_handles, con = generator.build_specialisation_constraint(x)
                                new_cons.add(con)
                                # assert(con not in seen_cons)
                                # seen_cons.add(con)
                                if not settings.single_solve:
                                    all_handles.update(parse_handles(generator, new_handles))
                                if hasse2.seen_prog(x):
                                    hasse2.remove_prog(x)
                            seen_incomplete_gen = set()
                            seen_incomplete_spec = set()

                # track how many examples are covered
                # if a program does not cover enough examples then prune it
                # if not add_spec and settings.datalog:
                # if not add_spec and False:
                if not add_spec:
                    assert(pruned_sub_incomplete == False)
                    if combiner.solution_found:
                        min_coverage = get_min_pos_coverage(combiner, cached_pos_covered)
                        if num_pos_covered <= min_coverage:
                            add_spec=True
                            # print('pruned because program covers too few examples')
                            # for rule in order_prog(prog):
                            #     print('\t',format_rule(order_rule(rule)))
                            # with settings.stats.duration('most gen'):
                            #     pass
                            #     pruned_more_general_shit = find_most_general_shit_subrule(prog, tester, settings, min_coverage, generator, new_cons, all_handles)

                if not add_spec and False:
                # if not add_spec:
                    assert(pruned_sub_incomplete == False)
                    with settings.stats.duration('check_redundant_literal2'):
                        if check_redundant_literal2(prog, tester, settings):
                            add_spec = True
                            add_gen = True
                            add_redund1 = True
                            add_redund2 = True
                            count_check_redundant_literal2 +=1
                            print('count_check_redundant_literal2',count_check_redundant_literal2)
                            for rule in order_prog(prog):
                                print('\t',format_rule(order_rule(rule)))
                            # if not add_gen:
                            #     print('count_check_redundant_literal2',count_check_redundant_literal2)
                            #     for rule in prog:
                            #         print('\t',format_rule(rule))

                if not add_spec:
                    assert(pruned_sub_incomplete == False)
                    # could_prune_later[prog]=num_pos_covered
                    with settings.stats.duration('build hasse2'):
                        hasse2.add(prog, pos_covered)

                seen_better_rec = False
                with settings.stats.duration('seen_better_rec'):
                    if is_recursive and not inconsistent and not subsumed and not add_gen and num_pos_covered > 0:
                        seen_better_rec = pos_covered in rec_success_sets or any(pos_covered.issubset(xs) for xs in rec_success_sets)

                if not add_spec and seen_better_rec:
                    assert(False)

                # if add_spec:

                # if consistent, covers at least one example, is not subsumed, and has no redundancy, try to find a solution
                # if not inconsistent and not subsumed and not add_gen and num_pos_covered > 0:
                if not inconsistent and not subsumed and not add_gen and num_pos_covered > 0 and not seen_better_rec:

                    assert(pruned_sub_incomplete == False)
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
                        settings.max_literals = combiner.max_size-1
                        if size >= settings.max_literals:
                            return

                        # with settings.stats.duration('prune smaller backtrack'):
                            # prune_smaller_backtrack(cached_pos_covered, combiner, generator, new_cons, all_handles, settings, tester)

                        with settings.stats.duration('prune smaller backtrack2'):
                            prune_smaller_backtrack2(cached_pos_covered, combiner, generator, new_cons, all_handles, settings, tester, hasse2)

                        if settings.single_solve:
                            # AC: sometimes adding these size constraints can take longer
                            for i in range(combiner.max_size, max_size+1):
                                # print('mooo', i)
                                size_con = [(atom_to_symbol("size", (i,)), True)]
                                model.context.add_nogood(size_con)


                    # with settings.stats.duration('pruned subsumed backtrack'):

                    with settings.stats.duration('pruned subsumed backtrack 2'):
                        prune_subsumed_backtrack2(pos_covered, combiner, generator, new_cons, all_handles, settings, tester, hasse2)


                # if non-separable program covers all examples, stop
                if not inconsistent and num_pos_covered == num_pos:
                    return

                with settings.stats.duration('build_constraints'):
                    build_constraints(settings, generator, new_cons, new_rule_handles, add_spec, add_gen, add_redund1, add_redund2, pruned_sub_incomplete, pruned_more_general_shit, pruned_sub_inconsistent, all_handles, bad_handles, prog, rule_ordering)

                # CONSTRAIN
                with settings.stats.duration('constrain'):
                    constrain(settings, new_cons, generator, all_ground_cons, cached_clingo_atoms, model, new_ground_cons)

        # if not pi_or_rec:
        if settings.single_solve:
            break

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
