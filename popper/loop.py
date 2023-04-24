import time
import numbers
from itertools import permutations
from itertools import chain, combinations
# from chain import from_iterable
from collections import deque
from . explain import get_raw_prog as get_raw_prog2
from . combine import Combiner
from . explain import Explainer, rule_hash, head_connected, find_subprogs, get_raw_prog, seen_more_general_unsat, seen_more_specific_sat, prog_hash, has_valid_directions, prog_hash2, order_body, connected
from . util import timeout, format_rule, rule_is_recursive, order_prog, prog_is_recursive, prog_has_invention, order_rule, prog_size, format_literal, theory_subsumes, rule_subsumes, format_prog, format_prog2, order_rule2
from . core import Literal
from . tester import Tester
from . generate import Generator, Grounder, parse_model, atom_to_symbol, arg_to_symbol
from . bkcons import deduce_bk_cons, deduce_recalls

AGGRESSIVE = False
AGGRESSIVE = True

SHOW_PRUNED = True
# SHOW_PRUNED = False

WITH_OPTIMISATIONS = True
# WITH_OPTIMISATIONS = False

WITH_MOST_GEN_OPTIMISATIONS = True
# WITH_MOST_GEN_OPTIMISATIONS = False


pruned = set()
pruned2 = set()

savings = 0


from enum import Enum

# class syntax
class Constraint(Enum):
    GENERALISATION = 1
    SPECIALISATION = 2
    UNSAT = 3
    REDUNDANCY_CONSTRAINT1 = 4
    REDUNDANCY_CONSTRAINT2 = 5


def explain_incomplete(settings, explainer, prog, directions):
    pruned_subprog = False

    unsat_cores = list(explainer.explain_totally_incomplete(prog, directions))

    # with settings.stats.duration('B'):
        # unsat_cores = list(find_most_gen_unsat(prog, tester, settings))

    out_cons = []
    for subprog, unsat_body in unsat_cores:
        pruned_subprog = True

        if SHOW_PRUNED:
            print('\t', format_prog2(subprog), '\t', 'unsat2', unsat_body)

        if unsat_body:
            _, body = list(subprog)[0]
            out_cons.append((Constraint.UNSAT, body))
            continue

        # if unsat_body:
        #     _, body = list(subprog)[0]
        #     ys = find_variants2(settings, (None,body))

        #     my_new_cons = set()
        #     for head2, body2 in ys:
        #         ground_con = []
        #         for pred, args in body2:
        #             arity = len(args)
        #             args2 = tuple(var_to_int[x] for x in args)
        #             v = (True, 'body_literal', (0, pred, arity, args2))
        #             ground_con.append(v)
        #         my_new_con = tuple(sorted(ground_con))
        #         my_new_cons.add(my_new_con)

        #     for x in my_new_cons:
        #         new_ground_cons.add(x)
        #     continue

        out_cons.append((Constraint.SPECIALISATION, subprog))

        if not settings.recursion_enabled or settings.pi_enabled:
            continue

        if len(subprog) == 1:
            out_cons.append((Constraint.REDUNDANCY_CONSTRAINT1, subprog))

        out_cons.append((Constraint.REDUNDANCY_CONSTRAINT2, subprog))

    return pruned_subprog, out_cons


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
            out_cons.append((Constraint.GENERALISATION, subprog))
            pruned_subprog = True

    if pruned_subprog:
        return out_cons

    if len(rec) == 1:
        return out_cons

    for r1 in base:
        for r2 in rec:
            subprog = frozenset([r1,r2])
            if tester.is_inconsistent(subprog):
                out_cons.append((Constraint.GENERALISATION, subprog))
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
    min_coverage = None
    for x in best_prog:
        if x in cached_pos_covered:
            v = len(cached_pos_covered[x])
            if min_coverage == None or v < min_coverage:
                min_coverage = v
        else:
            assert(False)
    return min_coverage

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


def rename_variables(rule):
    head, body = rule
    head2 = head
    if head:
        head_vars = set(head.arguments)
        head = (head.predicate, head.arguments)
    else:
        head_vars = set()
    next_var = len(head_vars)
    new_body = []
    lookup = {}


    body2 = sorted(body, key=lambda x: x.predicate)

    def score(literal):
        if any(x in head_vars for x in literal.arguments):
            return (0, literal.arguments, literal.predicate)
        else:
            return (1, literal.arguments, literal.predicate)

    body3 = sorted(body, key=lambda x: score(x))
    rule2 = head2, body2

    for body_literal in body3:
        new_args = []
        for var in body_literal.arguments:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = chr(ord('A') + next_var)
                next_var+=1
            new_args.append(lookup[var])
        new_body.append((body_literal.predicate, tuple(new_args)))

    return (head, new_body)

def get_raw_prog(prog):
    xs = set()
    for rule in prog:
        h, b = rename_variables(rule)
        xs.add((h, frozenset(b)))
    return frozenset(xs)

GROUND_VARIANTS_ENCODING = """\
#show bind_var/2.
1 {bind_var(Var,Value): Value=0..max_vars-1}1:- var(Var), Var >= head_vars.
value_type(Value,Type):- bind_var(Var,Value), var_type(Var,Type).
:- value_type(Value,T1), value_type(Value,T2), T1 != T2.
:- Value=0..max_vars-1, #count{Var : bind_var(Var,Value)} > 1.
"""

GROUND_VARIANTS_ENCODING2 = """\
#show bind_var/2.
1 {bind_var(Var,Value): Value=0..max_vars-1, not type_mismatch(Var,Value)}1:- var(Var), Var >= head_vars.
:- Value=0..max_vars-1, #count{Var : bind_var(Var,Value)} > 1.
type_mismatch(Var,Value):- var_type(Var,T1), known_value_type(Value,T2), T1 != T2.
"""

# def var_to_int(var):
    # return ord(var) - ord('A')

MAX_VARS=100
int_to_var = {i:chr(ord('A') + i) for i in range(0,MAX_VARS)}
var_to_int = {v:i for i,v in int_to_var.items()}

import clingo

cached_find_variants = {}
# @profile
def find_variants_aux(settings, rule):
    head, body = rule

    if head:
        head_vars = frozenset(head.arguments)
    else:
        head_vars = frozenset()
    body_vars = frozenset(x for literal in body for x in literal.arguments)

    print('find_variants_aux',format_rule(rule))

    k = hash((head_vars, body_vars))
    if k in cached_find_variants:
        return cached_find_variants[k]

    head_types = settings.head_types
    body_types = settings.body_types

    encoding = set()
    encoding.add(GROUND_VARIANTS_ENCODING)
    encoding.add(f'#const max_vars={settings.max_vars}.')
    encoding.add(f'#const head_vars={len(head_vars)}.')

    if head_types:
        for i, head_type in enumerate(head_types):
            encoding.add(f'var_type({i},{head_type}).')
            encoding.add(f'value_type({i},{head_type}).')

    var_lookup = {}
    if head:
        for x in head.arguments:
            k = var_to_int[x]
            encoding.add(f'bind_var({k},{k}).')
            encoding.add(f'var({k}).')
            var_lookup[k] = x

    for literal in body:
        # map the letter to an int
        for i, x in enumerate(literal.arguments):

            if x in head_vars:
                continue

            k = var_to_int[x]
            var_lookup[k] = x
            encoding.add(f'var({k}).')
            if literal.predicate in body_types:
                var_type = body_types[literal.predicate][i]
                encoding.add(f'var_type({k},{var_type}).')


    encoding = '\n'.join(encoding)

    solver = clingo.Control(['-Wnone'])
    solver.configuration.solve.models = 0
    solver.add('base', [], encoding)
    solver.ground([("base", [])])

    out = []

    def on_model(m):
        xs = m.symbols(shown = True)
        assignment = {}
        for x in xs:
            args = x.arguments
            var_var_int = args[0].number
            value = args[1].number
            var_var = var_lookup[var_var_int]
            assignment[var_var] = value
        out.append((var_lookup, assignment))
    solver.solve(on_model=on_model)

    cached_find_variants[k] = out
    return out

# def find_variants_aux2(settings, rule, max_vars=6):
#     head, body = rule

#     if head:
#         head_vars = frozenset(head.arguments)
#     else:
#         head_vars = frozenset()
#     body_vars = frozenset(x for literal in body for x in literal.arguments)

#     k = hash((head_vars, body_vars))
#     # if k in cached_find_variants:
#         # return cached_find_variants[k]

#     head_types = settings.head_types
#     body_types = settings.body_types

#     encoding = set()
#     encoding.add(GROUND_VARIANTS_ENCODING2)
#     encoding.add(f'#const max_vars={max_vars}.')
#     encoding.add(f'#const head_vars={len(head_vars)}.')

#     if head_types:
#         for i, head_type in enumerate(head_types):
#             encoding.add(f'var_type({i},{head_type}).')
#             # encoding.add(f'value_type({i},{head_type}).')
#             encoding.add(f'known_value_type({i},{head_type}).')


#     var_lookup = {}
#     if head:
#         for x in head.arguments:
#             k = var_to_int[x]
#             encoding.add(f'bind_var({k},{k}).')
#             encoding.add(f'var({k}).')
#             var_lookup[k] = x

#     for literal in body:
#         # map the letter to an int
#         for i, x in enumerate(literal.arguments):

#             if x in head_vars:
#                 continue

#             k = var_to_int[x]
#             var_lookup[k] = x
#             encoding.add(f'var({k}).')
#             if literal.predicate in body_types:
#                 var_type = body_types[literal.predicate][i]
#                 encoding.add(f'var_type({k},{var_type}).')


#     encoding = '\n'.join(encoding)

#     # print(format_rule(rule))
#     # print(encoding)
#     # exit()

#     t1 = time.time()
#     solver = clingo.Control(['-Wnone'])
#     solver.configuration.solve.models = 0
#     solver.add('base', [], encoding)
#     solver.ground([("base", [])])

#     out = []

#     def on_model(m):
#         xs = m.symbols(shown = True)
#         assignment = {}
#         for x in xs:
#             args = x.arguments
#             var_var_int = args[0].number
#             value = args[1].number
#             var_var = var_lookup[var_var_int]
#             assignment[var_var] = value
#         out.append((var_lookup, assignment))
#     solver.solve(on_model=on_model)
#     # t2 = time.time()
#     # d1 = t2-t1

#     # global max_time
#     # if d1 > max_time:
#     #     print('*'*30)
#     #     print(d1)
#     #     print(format_rule(rule))
#     #     print('============')
#     #     print(encoding)
#     #     print('*'*30)
#     #     max_time = d1
#     cached_find_variants[k] = out
#     return out

# @profile
def find_variants2(settings, rule):
    head, body = rule
    assignments = find_variants_aux(settings, rule)

    new_rules = []

    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None

    for indexes, assignment in assignments:
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in assignment:
                    new_arg = int_to_var[assignment[arg]]
                    new_args.append(new_arg)
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)

    return new_rules


# @profile
def find_variants(rule, max_vars=6):
    all_vars = 'ABCDEFGHIJKLM'
    all_vars = all_vars[:max_vars]

    head, body = rule
    if head:
        head_arity = head.arity
        head_vars = set(head.arguments)
    else:
        head_arity = 0
        head_vars = set()

    body_vars = frozenset({x for literal in body for x in literal.arguments if x not in head_vars})
    num_body_vars = len(body_vars)
    if head:
        # subset = all_vars[head_arity:head_arity+num_body_vars+1]
        subset = all_vars[head_arity:]
        # subset = all_vars
    else:
        subset = all_vars
    indexes = {x:i for i, x in enumerate(body_vars)}
    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None
    new_rules = []
    # print(body_vars, subset, indexes)
    perms = list(permutations(subset, num_body_vars))
    for xs in perms:
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in indexes:
                    new_args.append(xs[indexes[arg]])
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)
    return new_rules



def find_variants3(settings, rule, max_vars=6):
    all_vars = 'ABCDEFGHIJKLM'
    all_vars = all_vars[:max_vars]

    head_types = settings.head_types
    body_types = settings.body_types

    val_to_type = {}
    if head_types:
        val_to_type = {k:head_type for i, head_type in enumerate(head_types)}

    head, body = rule
    if head:
        head_arity = head.arity
        head_vars = set(head.arguments)
    else:
        head_arity = 0
        head_vars = set()


    for literal in body:
        for i, x in enumerate(literal.arguments):
            if x in head_vars:
                continue
            if literal.predicate not in body_types:
                continue
            var_type = body_types[literal.predicate][i]
            encoding.add(f'var_type({k},{var_type}).')

    body_vars = frozenset({x for literal in body for x in literal.arguments if x not in head_vars})
    num_body_vars = len(body_vars)
    if head:
        # subset = all_vars[head_arity:head_arity+num_body_vars+1]
        subset = all_vars[head_arity:]
        # subset = all_vars
    else:
        subset = all_vars
    indexes = {x:i for i, x in enumerate(body_vars)}
    if head:
        new_head = (head.predicate, head.arguments)
    else:
        new_head = None
    new_rules = []
    # print(body_vars, subset, indexes)
    for xs in permutations(subset, num_body_vars):
        new_body = []
        for literal in body:
            new_args = []
            for arg in literal.arguments:
                if arg in indexes:
                    new_args.append(xs[indexes[arg]])
                else:
                    new_args.append(arg)
            new_body.append((literal.predicate, tuple(new_args)))
        new_rule = (new_head, frozenset(new_body))
        new_rules.append(new_rule)
    return new_rules

# @profile
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

def find_most_general_subsumed1(prog, tester, success_sets, settings):
    head, body = list(prog)[0]
    out = []
    for new_body in non_empty_powerset(body):

        if len(new_body) == len(body):
            continue

        new_rule = (head, new_body)
        new_prog = frozenset({new_rule})

        if not head_connected(new_rule):
            continue

        if not has_valid_directions(new_rule):
            continue

        if any(frozenset((y.predicate, y.arguments) for y in x) in pruned2 for x in non_empty_powerset(new_body)):
            continue

        # AC: THE CODE BELOW ALLOWS USE TO AVOID SOME PROLOG CALLS. HOWEVER, THE OVERHEAD IS RATHER HIGH SO I DOUBT IT IS WORTH INCLUDING
        # for x in non_empty_powerset(new_body):
        #     head2, body2 = functional_rename_vars((head, x))
        #     ys = frozenset((y.predicate, y.arguments) for y in body2)
        #     if ys in pruned2 and xs not in pruned2:
        #         BREAK + SKIP

        if tester.has_redundant_literal(new_prog):
            assert(False)

        sub_prog_pos_covered = tester.get_pos_covered(new_prog, ignore=True)
        if sub_prog_pos_covered in success_sets or any(sub_prog_pos_covered.issubset(xs) for xs in success_sets):
            out.append(new_prog)
            for _, x in find_variants(new_rule, settings.max_vars):
                pruned2.add(x)
    return out

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

        # if not has_valid_directions(new_rule):
        #     # print('b', format_rule(new_rule))
        #     print('\t'*2, 'c')
        #     xs = find_most_general_subsumed(new_prog, tester, success_sets, settings)
        #     out.update(xs)
        #     continue

        # if tester.has_redundant_literal(new_prog):
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


def generalisations(rule):
    head, body = rule
    if len(body) == 0:
        return
    body = list(body)

    if head:
        yield (None, frozenset(body))

    if len(body) == 1:
        return

    for i in range(len(body)):
        new_body = body[:i] + body[i+1:]
        new_body = frozenset(new_body)
        yield (head, new_body)


def all_generalisations(rule):
    out = set()
    head, body = rule
    if head:
        r = (None, body)
        out.add(r)

    for new_body in non_empty_powerset(body):
        if len(new_body) == len(body):
            continue
        r = (head, new_body)
        out.add(r)
        r = (None, new_body)
        out.add(r)
    return out



def get_my_key(rule):
    head, body = rule

    a = None
    if head:
        a = (head.predicate, head.arguments)
    b = frozenset((x.predicate, x.arguments) for x in body)
    return (a, b)


tmp_cache = set()
seen_sat2 = set()
seen_most_gen_sat = set()

def find_most_gen_unsat(prog, tester, settings):
    rule = list(prog)[0]
    head, body = rule

    if len(body) == 0:
        return []

    out = set()

    for new_rule in generalisations(rule):
        new_head, new_body = new_rule

        if len(new_body) == 0:
            continue

        if not new_head and len(new_body) == 1:
            continue

        if new_head:
            head_vars = set(new_head.arguments)
        else:
            head_vars = set()

        k1 = get_my_key(new_rule)
        if k1 in seen_most_gen_sat:
            continue
        seen_most_gen_sat.add(k1)

        if k1 in seen_sat2:
            continue

        if k1 in tmp_cache:
            continue

        if new_head and not any(x in head_vars for literal in new_body for x in literal.arguments):
            continue

        skip = False
        for gen in all_generalisations(new_rule):
            k2 = get_my_key(gen)
            if k1 == k2:
                continue
            if k2 in tmp_cache:
                skip = True
                break
        if skip:
            continue

        if not new_head and not connected(new_body):
            continue

        new_prog = frozenset({new_rule})

        if new_head and not head_connected(new_rule):
            xs = find_most_gen_unsat(new_prog, tester, settings)
            out.update(xs)
            continue

        sat = False

        if new_head:
            sat = tester.is_sat(new_prog)
        else:
            sat = tester.is_body_sat(order_body(new_body))

        if sat:
            variants = set(find_variants2(settings, new_rule))
            seen_sat2.update(variants)
            continue

        xs = find_most_gen_unsat(new_prog, tester, settings)
        if len(xs) > 0:
            out.update(xs)
            continue

        variants = set(find_variants2(settings, new_rule))
        # print('len(variants)',len(variants))
        # for x in variants:
            # print('\t'*6, 'var', x)
        tmp_cache.update(variants)
        out.add((new_prog, new_head == None))
    return out



def popper(settings):
    with settings.stats.duration('load data'):
        tester = Tester(settings)

    if settings.bkcons:
        with settings.stats.duration('bkcons'):
            deduce_bk_cons(settings, tester)
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

    # constraints generated
    all_ground_cons = set()
    # messy stuff
    new_ground_cons = set()
    # new rules added to the solver, such as: seen(id):- head_literal(...), body_literal(...)
    all_handles = set()
    # handles for rules that are minimal and unsatisfiable
    bad_handles = set()

    # generator that builds programs
    with settings.stats.duration('init'):
        generator = Generator(settings, grounder)

    # tmp_covered = set()
    cached_pos_covered = {}
    could_prune_later = {}


    good_checks = 0
    bad_checks = 0

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
            generator.update_number_of_literals(size)

            with settings.stats.duration('init'):
                generator.update_solver(size, all_handles, bad_handles, all_ground_cons)

        handle = iter(generator.solver.solve(yield_ = True))
        all_ground_cons = set()
        all_handles = set()
        bad_handles = set()

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
                        pruned_sub_incomplete, cons_ = explain_incomplete(settings, explainer, prog, directions)
                        tmp_new_cons.extend(cons_)

                elif combiner.solution_found and not is_recursive and not has_invention and WITH_OPTIMISATIONS:
                    min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                    if not AGGRESSIVE:
                        min_coverage = 2
                    # if we have a solution, any better solution must cover at least two examples
                    if len(pos_covered) < min_coverage:
                        add_spec = True
                        if WITH_MOST_GEN_OPTIMISATIONS:
                            with settings.stats.duration('find most gen incomplete'):
                                more_general_shit_progs = find_most_general_shit_subrule(prog, tester, settings, min_coverage)
                                if len(more_general_shit_progs):
                                    pruned_more_general_shit = True
                                for x in more_general_shit_progs:
                                    if SHOW_PRUNED:
                                        print('\t', format_prog2(x), '\t', 'pruned_more_general_shit', len(pos_covered))
                                    tmp_new_cons.append((Constraint.SPECIALISATION, x))

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
                            tmp_new_cons.append((Constraint.GENERALISATION,([rule])))

            # remove a subset of theta-subsumed rules when learning recursive programs with more than two rules
            if settings.max_rules > 2 and is_recursive:
                for x in generator.andy_tmp_con(prog):
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
                    tmp_new_cons.append((Constraint.GENERALISATION,[r1,r2]))

            # check whether subsumed by a seen program
            subsumed = False

            # WHY DO WE HAVE A RECURSIVE CHECK???
            if num_pos_covered > 0 and not is_recursive:
                with settings.stats.duration('check_subsumed'):
                    subsumed = pos_covered in success_sets or any(pos_covered.issubset(xs) for xs in success_sets)
                    # if so, prune specialisations
                    if subsumed:
                        add_spec = True
                        if not is_recursive and not has_invention and WITH_MOST_GEN_OPTIMISATIONS:
                            xs = find_most_general_subsumed(prog, tester, success_sets, settings)
                            for x in xs:
                                pruned_more_general_shit = True
                                if SHOW_PRUNED:
                                    print('\t', format_prog2(x), '\t', 'subsumed_0')
                                    pass
                                tmp_new_cons.append((Constraint.SPECIALISATION, x))


            # SPECIAL CASE FOR WHEN THE SOLUTION ONLY HAS AT MOST TWO RULES
            # TODO: IMPROVE!!!!
            # if combiner.solution_found and len(combiner.best_prog) <= 2 and WITH_OPTIMISATIONS and False:
            #     with settings.stats.duration('new check on min_coverage'):
            #         if two_rule_optimisation(prog, tester, generator, new_cons, all_handles, settings):
            #             pruned_more_general_shit = True

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

                    # x =

                    # print('GET_MIN_POS_COVERAGE', get_min_pos_coverage(combiner.best_prog, cached_pos_covered))
                    # # TMP!!
                    # combiner.min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                    # for x in combiner.best_prog:
                    #     v = len(cached_pos_covered[x])
                    #     print('\t', format_rule(x), v)

                    if not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                        min_coverage = get_min_pos_coverage(combiner.best_prog, cached_pos_covered)
                        with settings.stats.duration('prune smaller backtrack'):
                            xs = prune_smaller_backtrack4(min_coverage, cached_pos_covered, could_prune_later, settings, tester)
                            for x in xs:
                                tmp_new_cons.append((Constraint.SPECIALISATION, x))

                    if settings.single_solve:
                        # AC: sometimes adding these size constraints can take longer
                        for i in range(combiner.max_size, max_size+1):
                            # print('mooo', i)
                            size_con = [(atom_to_symbol("size", (i,)), True)]
                            model.context.add_nogood(size_con)

                if not has_invention and not is_recursive and WITH_OPTIMISATIONS:
                    with settings.stats.duration('prune subsumed backtrack'):
                        xs = prune_subsumed_backtrack2(pos_covered, settings, could_prune_later, tester)
                        for x in xs:
                            tmp_new_cons.append((Constraint.SPECIALISATION, x))

            # BUILD CONSTRAINTS
            with settings.stats.duration('build_constraints'):

                handles_ = []

                if add_spec and not pruned_sub_incomplete and not pruned_more_general_shit:
                    handles, con = generator.build_specialisation_constraint(prog, rule_ordering)
                    if not settings.single_solve:
                        new_rule_handles.update(handles)
                    new_cons.add(con)

                if add_gen and not pruned_sub_inconsistent:
                    if settings.recursion_enabled or settings.pi_enabled or not pruned_sub_incomplete:
                        handles, con = generator.build_generalisation_constraint(prog, rule_ordering)
                        if not settings.single_solve:
                            new_rule_handles.update(handles)
                        new_cons.add(con)

                if add_redund1 and not pruned_sub_incomplete:
                    bad_handle, handles, con = generator.redundancy_constraint1(prog)
                    bad_handles.add(bad_handle)
                    if not settings.single_solve:
                        new_rule_handles.update(handles)
                    new_cons.add(con)

                if add_redund2 and not pruned_sub_incomplete:
                    handles, cons = generator.redundancy_constraint2(prog, rule_ordering)
                    if not settings.single_solve:
                        new_rule_handles.update(handles)
                    new_cons.update(cons)

                # if pi or rec, save the constraints and handles for the next program size
                if not settings.single_solve:
                    parsed_handles = list(generator.parse_handles(new_rule_handles))
                    all_handles.update(parsed_handles)

                for con_type, con_prog in tmp_new_cons:
                    if con_type == Constraint.SPECIALISATION:
                        new_rule_handles, con = generator.build_specialisation_constraint(con_prog)
                        new_cons.add(con)
                        handles_.extend(new_rule_handles)
                    if con_type == Constraint.GENERALISATION:
                        new_rule_handles, con = generator.build_generalisation_constraint(con_prog)
                        new_cons.add(con)
                        handles_.extend(new_rule_handles)
                    elif con_type == Constraint.UNSAT:
                        con = generator.unsat_constraint(con_prog)
                        for h, b in generator.get_ground_deep_rules(con):
                            new_ground_cons.add(b)
                    elif con_type == Constraint.REDUNDANCY_CONSTRAINT1:
                        bad_handle, new_rule_handles, con = generator.redundancy_constraint1(con_prog)
                        bad_handles.add(bad_handle)
                        new_cons.add(con)
                        handles_.extend(new_rule_handles)
                    elif con_type == Constraint.REDUNDANCY_CONSTRAINT2:
                        handles, cons = generator.redundancy_constraint2(con_prog)
                        new_cons.update(cons)
                        handles_.extend(new_rule_handles)

                if not settings.single_solve:
                    all_handles.update(generator.parse_handles(handles_))

            # CONSTRAIN
            with settings.stats.duration('constrain'):
                generator.constrain(new_cons, all_ground_cons, model, new_ground_cons)

        # if not pi_or_rec:
        if settings.single_solve:
            break

def learn_solution(settings):
    timeout(settings, popper, (settings,), timeout_duration=int(settings.timeout),)
    return settings.solution, settings.best_prog_score, settings.stats
