import logging
import sys
from datetime import datetime
from . util import Settings, Stats, timeout, parse_settings
from . tester import Tester
from . constrain import Constrain
from collections import defaultdict
from . core import Clause, Literal
import clingo
import clingo.script
import multiprocessing

clingo.script.enable_python()

def dbg(*args):
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    print(current_time, *args)

def deduce_bk_cons(settings, bk):
    prog = []
    lookup1 = {1:'(A,)', 2:'(A,B)', 3:'(A,B,C)', 4:'(A,B,C,D)'}
    lookup2 = {1:'(A)', 2:'(A,B)', 3:'(A,B,C)', 4:'(A,B,C,D)'}
    for p,a in settings.body_preds:
        arg_str = lookup1[a]
        arg_str2 = lookup2[a]
        rule = f'holds({p},{arg_str}):- {p}{arg_str2}.'
        prog.append(rule)

    solver = clingo.Control()
    with open(settings.bias_file) as f:
        solver.add('base', [], f.read())
    solver.add('base', [], bk)
    solver.add('base', [], '\n'.join(prog) + '\n')
    with open('popper/lp/cons.pl') as f:
        solver.add('base', [], f.read())
    solver.ground([('base', [])])
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                if atom.name == 'prop':
                    print(atom)
    exit()

def gen_args(args):
    return tuple(chr(ord('A') + arg.number) for arg in args)

def parse_model(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    body_atoms = []
    for atom in model:
        pred = atom.arguments[0].name
        args = gen_args(atom.arguments[2].arguments)
        arity = len(args)
        modes = tuple(directions[pred][i] for i in range(arity))
        literal = Literal(pred, args, modes)
        if atom.name == 'body_literal':
            body_atoms.append(literal)
        elif atom.name == 'head_literal':
            head = literal
    return head, frozenset(body_atoms)

def test_prolog(tester, stats, rules):
    for rule in rules:
        inconsistent, pos_covered = tester.my_test(stats, rule)
        yield rule, inconsistent, pos_covered

def split(a, n):
    k, m = divmod(len(a), n)
    return (a[i*k+min(i, m):(i+1)*k+min(i+1, m)] for i in range(n))

def test_rules_clingo(tester, stats, rules, pos):
    k = 16
    splits = list(split(rules, k))
    # print('testing clingo')
    jobs = [(tester, splits[i], pos) for i in range(k)]
    # pool = multiprocessing.Pool()
    with stats.duration('testing'):
        # res = list(pool.map(test_rules_clingo_, jobs))
        res = list(map(test_rules_clingo_, jobs))
    return [x for xs in res for x in xs]

# def test_rules_clingo(tester, stats, rules, pos):
def test_rules_clingo_(job):
    tester, rules, pos = job
    if len(rules) == 0:
        return []

    # with stats.duration('test.build'):
    prog = []
    prog.append("""
    #defined pos_covers/2.
    #defined inconsistent/1.

    #show pos_covers/2.
    #show inconsistent/1.
    pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
    inconsistent(R):- neg(E,Atom), holds(R,Atom).
    """)

    for i in pos:
        prog.append(f'pos({i},{tester.pos_index[i]}).')
    for i, atom in zip(tester.neg, tester.neg_atoms):
        prog.append(f'neg({i},{atom}).')

    for i, rule in enumerate(rules):
        rule = format_rule(rule)
        rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
        rule = rule.replace('f(A)', f'holds({i},f(A))')
        rule = rule.replace('f(A,B)', f'holds({i},f(A,B))')
        rule = rule.replace('out(A,B,C)', f'holds({i},out(A,B,C))')
        rule = rule.replace('output(A,B,C)', f'holds({i},output(A,B,C))')
        rule = rule.replace('out(A,B,C,D)', f'holds({i},out(A,B,C,D))')
        rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
        rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
        rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
        rule = rule.replace('next_color(A,B,C)', f'holds({i},next_color(A,B,C))')
        prog.append(rule)

    prog = '\n'.join(prog)

    # print('HELLO WORLD!!!!!!!')
    # print(prog + tester.bk)

    solver = clingo.Control(["--single-shot", "-t16"])
    solver.add('bk', [], tester.bk)
    solver.add('rules', [], prog)

    # with stats.duration('test.ground.bk'):
    # print('test.ground.bk')
    solver.ground([('bk', [])])
    # with stats.duration('test.ground.rules'):
    # print('test.ground.rules')
    solver.ground([('rules', [])])

    # with open(f'test-prob-{len(rules)}.pl', 'w') as f:
        # f.write(prog_rules)
        # f.write('\n')
        # f.write(bk)

    atoms = []
    # with stats.duration('test.solve'):
    # print('test.solve')
    with solver.solve(yield_=True) as handle:
        for m in handle:
            atoms.extend(m.symbols(shown = True))

    # with stats.duration('test.parse'):
    # print('test.parse')
    inconsistent = {i:False for i in range(len(rules))}
    covers = {i:set() for i in range(len(rules))}

    for atom in atoms:
        if atom.name == 'pos_covers':
            i = atom.arguments[0].number
            example = atom.arguments[1].number
            covers[i].add(example)
        elif atom.name == 'inconsistent':
            i = atom.arguments[0].number
            inconsistent[i] = True

    return [(rule, inconsistent[i], covers[i]) for i, rule in enumerate(rules)]
        # yield

def test_coverage(tester, stats, rules, pos):
    assert(len(rules) > 0)
    prog = []
    prog.append("#show covers/2.")
    prog.append("covers(R,E):- example(E,Atom), holds(R,Atom).")
    for i in pos:
        prog.append(f'example({i},{tester.pos_index[i]}).')
    for i, atom in zip(tester.neg, tester.neg_atoms):
        prog.append(f'example({i},{atom}).')

    rule_to_index = {}
    index_to_rule = {}
    i = 0
    # for rule, size in rules:
        # print('C',format_rule(rule), size)
    for rule, size in rules:
        i += 1
        rule_to_index[rule] = i
        index_to_rule[i] = rule
        rule = format_rule(rule)
        rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
        rule = rule.replace('f(A)', f'holds({i},f(A))')
        rule = rule.replace('f(A,B)', f'holds({i},f(A,B))')
        rule = rule.replace('out(A,B,C)', f'holds({i},out(A,B,C))')
        rule = rule.replace('output(A,B,C)', f'holds({i},output(A,B,C))')
        rule = rule.replace('out(A,B,C,D)', f'holds({i},out(A,B,C,D))')
        rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
        rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
        rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
        rule = rule.replace('next_color(A,B,C)', f'holds({i},next_color(A,B,C))')
        prog.append(rule)

    # for rule, size in rules:
        # print('D',format_rule(rule), size)

    # print('RULE_TO_INDEX', rule_to_index)
    # print('INDEX_TO_RULE', index_to_rule)

    assert(i > 0)

    # print('\n'.join(prog))
    prog = '\n'.join(prog)

    solver = clingo.Control(["--single-shot", "-t16"])
    solver.add('base', [], tester.bk)
    solver.add('base', [], prog)

    # print(prog + '\n' + tester.bk)

    # exit()

    with stats.duration('test.ground'):
        # print('test.ground.bk')
        solver.ground([('base', [])])

    atoms = []
    with stats.duration('test.solve'):
        # print('test.solve')
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms.extend(m.symbols(shown = True))

    # out = {rule:set() for rule, size in rules}
    # out = [set()]*len(rules)

    # print('ATOMS',atoms)

    covers = {i:set() for i in index_to_rule.keys()}
    # print('COVERS',covers)

    with stats.duration('test.parse'):
        for atom in atoms:
            if atom.name == 'covers':
                i = atom.arguments[0].number
                example = atom.arguments[1].number
                covers[i].add(example)

    out = {index_to_rule[i]:xs for i, xs in covers.items()}
    # print('OUT', out)

    return out


# def test_rules_clingo(tester, stats, rules, pos):
#     # print(pos)
# # def test_rules_clingo(job):
#     # tester, stats, rules, pos = job
#     if len(rules) == 0:
#         return []

#     # with stats.duration('test.build'):
#     prog = """
#     #defined pos_covers/2.
#     #defined inconsistent/1.
#     #show pos_covers/2.
#     #show inconsistent/1.
#     pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
#     inconsistent(R):- neg(E,Atom), holds(R,Atom).
#     """
#     prog += '\n'
#     prog += '\n'.join(f'pos({i},{tester.pos_index[i]}).' for i in pos)
#     prog += '\n'
#     prog += '\n'.join(f'neg({i},{atom}).' for i, atom in zip(tester.neg, tester.neg_atoms))
#     prog += '\n'

#     prog_rules = ''

#     for i, rule in enumerate(rules):
#         rule = format_rule(rule)
#         # print(rule)
#         rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
#         rule = rule.replace('f(A)', f'holds({i},f(A))')
#         rule = rule.replace('f(A,B)', f'holds({i},f(A,B))')
#         rule = rule.replace('out(A,B,C)', f'holds({i},out(A,B,C))')
#         rule = rule.replace('output(A,B,C)', f'holds({i},output(A,B,C))')
#         rule = rule.replace('out(A,B,C,D)', f'holds({i},out(A,B,C,D))')
#         rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
#         rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
#         rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
#         rule = rule.replace('next_color(A,B,C)', f'holds({i},next_color(A,B,C))')
#         # print(rule)
#         prog_rules += rule + '\n'

#     solver = clingo.Control(["--single-shot", "-t16"])
#     # solver = clingo.Control(["--single-shot"])
#     solver.add('bk', [], tester.bk)
#     solver.add('rules', [], prog)
#     solver.add('rules', [], prog_rules)

#     with stats.duration('test.ground.bk'):
#         # print('test.ground.bk')
#         solver.ground([('bk', [])])
#     with stats.duration('test.ground.rules'):
#         # print('test.ground.rules')
#         solver.ground([('rules', [])])

#     # with open(f'test-prob-{len(rules)}.pl', 'w') as f:
#         # f.write(prog_rules)
#         # f.write('\n')
#         # f.write(bk)

#     atoms = []
#     with stats.duration('test.solve'):
#         # print('test.solve')
#         with solver.solve(yield_=True) as handle:
#             for m in handle:
#                 atoms.extend(m.symbols(shown = True))

#     with stats.duration('test.parse'):
#         # print('test.parse')
#         inconsistent = {i:False for i in range(len(rules))}
#         covers = {i:set() for i in range(len(rules))}

#         for atom in atoms:
#             if atom.name == 'pos_covers':
#                 i = atom.arguments[0].number
#                 example = atom.arguments[1].number
#                 covers[i].add(example)
#             elif atom.name == 'inconsistent':
#                 i = atom.arguments[0].number
#                 inconsistent[i] = True

#     return [(rule, inconsistent[i], covers[i]) for i, rule in enumerate(rules)]
#         # yield

def find_subset(examples, all_rules, covers):
    prog = []
    for x in examples:
        prog.append(f'example({x}).')
    i = 0
    rule_to_index = {}
    index_to_rule = {}
    for rule, xs in covers.items():
        # print(i, format_rule(rule), xs)
        prog.append('{rule(' + str(i) + ')}.')
        index_to_rule[i] = rule
        rule_to_index[rule] = i
        for x in xs:
            prog.append(f'covers({i},{x}).')
        i +=1
    for rule, size in all_rules:
        if rule not in rule_to_index:
            continue
        i = rule_to_index[rule]
        prog.append(f'size({i},{size}).')

    prog = '\n'.join(prog)

    prog += """
    covered(E):- covers(R,E), rule(R).
    :- example(E), not covered(E).
    #show rule/1.
    size(N):- #sum{K,R : rule(R), size(R,K)} == N.
    #minimize{X : size(X)}.
    """
    with open('sat-prob.pl', 'w') as f:
        f.write(prog)

    # solver = clingo.Control(["--single-shot"])
    solver = clingo.Control(["--single-shot", "-t16"])
    solver.add('base', [], prog)
    solver.ground([('base', [])])

    out = []
    with solver.solve(yield_=True) as handle:
        for m in handle:
            xs = m.symbols(shown = True)
            out = [atom.arguments[0].number for atom in xs]
            # print(xs)
    # print('out',out)
    return [index_to_rule[i] for i in out]

def format_rule(rule):
    # return Clause.to_code(rule) + '.\n'
    return Clause.to_code(rule) + '.'

def get_solver(stats, settings, cons = set()):

    check_cons(cons)

    solver = clingo.Control(["-t16"])
    solver.configuration.solve.models = 0
    with open('popper/lp/alan.pl') as f:
        solver.add('base', [], f.read())
    with open(settings.bias_file) as f:
        solver.add('base', [], f.read())
    # add bootstap constraints
    solver.add('base', [], '\n'.join(cons))

    solver.ground([('base', [])])

    # NUM_OF_LITERALS = (
    # """
    # %%% External atom for number of literals in the program %%%%%
    # #external size_in_literals(n).
    # :- size_in_literals(n), not body_size(n).
    # """)

    NUM_OF_LITERALS = (
    """
    %%% External atom for number of literals in the program %%%%%
    #external size_in_literals(n).
    :-
        size_in_literals(n),
        #count{P,Vars : body_literal(P,_,Vars)} != n.
    """)


    solver.add('number_of_literals', ['n'], NUM_OF_LITERALS)

    return solver

def update_number_of_literals(solver, tracker, size):
    # 1. Release those that have already been assigned
    for atom, truth_value in tracker.items():
        if atom[0] == 'size_in_literals' and truth_value:
            tracker[atom] = False
            symbol = clingo.Function('size_in_literals', [clingo.Number(atom[1])])
            solver.release_external(symbol)
    solver.ground([('number_of_literals', [clingo.Number(size)])])
    tracker[('size_in_literals', size)] = True
    symbol = clingo.Function('size_in_literals', [clingo.Number(size)])
    solver.assign_external(symbol, True)

def load_settings(settings):
    # load head/body preds
    solver = clingo.Control()
    with open(settings.bias_file) as f:
        solver.add('bias', [], f.read())
    solver.add('bias', [], """
        #defined body_literal/3.
        #defined clause_var/1.
        #defined var_type/2.
    """)
    solver.ground([('bias', [])])

    for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
        args = x.symbol.arguments
        symbol = args[0]
        arity = args[1].number
        settings.head_pred = (symbol, arity)

    settings.body_preds = set()
    for x in solver.symbolic_atoms.by_signature('body_pred', arity=2):
        args = x.symbol.arguments
        symbol = args[0]
        arity = args[1].number
        settings.body_preds.add((symbol, arity))

def gen_rules(settings, stats, size, cons, solver):

    if len(cons) > 0:
        with stats.duration('gen.ground.cons'):
            k = f'cons_{size}'
            dbg(f'gen.ground.cons {k} {len(cons)}')
            solver.add(k, [], '\n'.join(cons))
            solver.ground([(k, [])])

    with stats.duration('gen.solve'):
        # dbg('gen.solve')
        models = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                models.append(m.symbols(shown = True))

    with stats.duration('gen.build'):
        # dbg(f'gen.build:{len(models)}')
        # pool = multiprocessing.Pool()
        # return list(pool.map(parse_model, models))
        return [parse_model(model) for model in models]

def check_cons(cons):
    seen = []

    for r1 in cons:
        r1 = frozenset([x for x in r1.split(" ") if x.startswith('body')])


        to_pop = set()
        skip = False
        for i, r2 in enumerate(seen):
            if r2.issubset(r1):
                skip = True
                # print('r2 is poo')
                break
            if r1.issubset(r2):
                # print('r1 is poo')
                to_pop.add(i)

        seen = [x for i, x in enumerate(seen) if i not in to_pop]
        if skip:
            continue
        seen.append(r1)

    print('ASDA',len(cons), len(seen))

def find_rules(settings, stats, tester, pos, boostrap_cons, min_size=1, max_size=6):
    assert(len(pos) > 0)

    constrainer = Constrain()

    with stats.duration('get_solver'):
        solver = get_solver(stats, settings, boostrap_cons)

    # track the enabled/disabled size literals
    tracker = {}

    # complete rules found
    complete_rules = set()

    # with open('TMP-cons.pl', 'w') as f:
    #     f.write('\n'.join(boostrap_cons))

    # all news
    cons = set()

    # new sons
    new_cons = set()

    seen_ss = {}

    size = min_size
    while size < max_size:
        size += 1
        body_size = size-1

        # increase program size constraint
        update_number_of_literals(solver, tracker, body_size)

        dbg(f'size:{size} cons:{len(cons)} pos:{len(pos)}')

        # generate all rules of size body_size + 1 that satisfy the constraints
        rules = list(gen_rules(settings, stats, body_size, cons, solver))

        dbg(f'num_rules:{len(rules)}')

        # reset cons
        cons = set()

        # remove rules that contain redundant literals
        with stats.duration('check_redundant'):
            good_rules = []
            for rule in rules:
                if tester.rule_has_redundant_literal(rule):
                    # TODO: RULE OUT GENERALISATIONS
                    # gen_cons.add
                    pass
                else:
                    good_rules.append(rule)
            rules = good_rules

        # print(f'testing {len(rules)} rules')
        # for rule in rules:
            # print('A',format_rule(rule))

        # rules = ptest_rules_clingo(tester, stats, rules, pos)
        # print('ASDA')

        # test rules on the subset of examples
        rules = test_rules_clingo(tester, stats, rules, pos)

        for rule, inconsistent, coverage in rules:
            # if not inconsistent:
            # print('E', format_rule(rule), inconsistent, coverage)

            # if the rule does not cover the examples, eliminate specialisations
            if len(coverage) != len(pos):
                for con in constrainer.specialisation_constraint1([rule], {}, {}):
                    cons.add(constrainer.format_constraint(con))
                    new_cons.add(constrainer.format_constraint(con))
                continue

            assert(len(coverage) > 0)
            assert(len(coverage) == len(pos))

            if inconsistent:
                # TODO: RULE OUT GENERALISATIONS
                # NEED TO MOVE ABOVE AS TO NOT SKIP
                continue


            # with stats.duration('check_crap'):
            # is_crap = False
            # for i, xs in covers.items():
            #     if coverage.issubset(xs):
            #         is_crap = True
            #         for con in constrainer.specialisation_constraint([rule], {}, {}):
            #             cons.add(constrainer.format_constraint(con))
            #             bad_rules.append(rule)
            # if is_crap:
                # continue

            # rule must be complete and consistent, so we can prune specialisations
            for con in constrainer.specialisation_constraint1([rule], {}, {}):
                cons.add(constrainer.format_constraint(con))
                new_cons.add(constrainer.format_constraint(con))

            complete_rules.add((rule, size))

        if len(complete_rules) > 0:
            return complete_rules, new_cons

    return [], new_cons

def chunk_list(xs, size):
    for i in range(0, len(xs), size):
        yield xs[i:i+size]

def flatten(xs):
    return [item for sublist in xs for item in sublist]

def popper(settings, stats):
    tester = Tester(settings)
    tester.pos_index = {k:v for k,v in zip(tester.pos, tester.pos_atoms)}
    tester.neg_index = {k:v for k,v in zip(tester.neg, tester.neg_atoms)}

    with open(settings.bk_file, 'r') as f:
        tester.bk = f.read()

    load_settings(settings)
    # deduce_bk_cons(settings, tester.bk)

    all_pos = set(tester.pos)
    print(f'num_examples:{len(all_pos)}')

    # maintain specialisation constraints
    spec_cons = {x: set() for x in all_pos}

    pos = set(tester.pos)
    neg = set(tester.neg)

    # starting program size
    size = 0

    # maximum program size
    max_size = 8

    # best prog seen
    best_prog = []

    # chunk/partition size
    chunk_size = 1

    all_chunks = [[x] for x in pos]

    # all candidate rules
    all_rules = set()

    # map from rule to examples covered
    rule_example_coverage = {}

    # map from rules to sizes
    sizes = {}

    while chunk_size <= len(pos):
        chunks = list(chunk_list(all_chunks, chunk_size))

        # separate new rules to test
        all_new_rules = set()

        # examples already covered by rules already
        covered = set()

        for chunk_pos in chunks:
            chunk_pos = flatten(chunk_pos)
            print(f'chunk_size:{chunk_size} chunk_pos:{chunk_pos}')

            # if all examples are covered, stop
            if all(x in covered for x in chunk_pos):
                continue

            # retrieve specialisation cons from previous iterations
            boostrap_cons = set()
            for x in chunk_pos:
                boostrap_cons.update(spec_cons[x])

            # find new (i) complete and consistent rules, and (ii) cons (currently only specialisations)
            new_rules, new_cons = find_rules(settings, stats, tester, chunk_pos, boostrap_cons, max_size=max_size)

            # keep track of all rules
            all_new_rules.update(new_rules)

            # update the cons for each example
            for x in chunk_pos:
                spec_cons[x].update(new_cons)

            # no generalisation
            # TODO: update min rules
            if len(new_rules) == 0:
                # exit()
                return
                continue

            with stats.duration('coverage'):
                # calculate example coverage of the new rules
                covers = test_coverage(tester, stats, new_rules, all_pos)
                for rule, covered_examples in covers.items():
                    # update rule example coverage
                    rule_example_coverage[rule] = covered_examples

                    # print(format_rule(rule), covered_examples)

                    # update covered examples
                    covered.update(covered_examples)

        # after passing through all chunks, double chunk size
        chunk_size += chunk_size
        all_rules.update(all_new_rules)

        assert(len(all_rules) > 0)

        # if no new rules, skip to the next chunk size
        if len(new_rules) == 0:
            continue


        if all(len(rule_example_coverage[rule]) == 0 for rule, _size in new_rules):
            assert(False)
            continue

        with stats.duration('find_subset'):
            print(f'subset problem:{len(all_rules)}')
            # for rule, size in all_rules:
                # print(format_rule(rule))

            # find the minimal subset of rules that covers all the examples
            rules = list(find_subset(all_pos, all_rules, rule_example_coverage))

            # if there is no set, then continue to search
            if len(rules) == 0:
                continue

            print('Solution:')
            for rule in rules:
                print(format_rule(rule))

        # return

            # TODO: CALC BOUNDS

            # exit()

            # def perform_chunking(tracker):
            #     tmp_chunks = {}
            #     for ex in tracker.pos:
            #         prog = tracker.best_progs[ex]
            #         # IF NO SOLUTION THEN IGNORE
            #         if prog == None:
            #             dbg(f'NO SOLUTION FOR EX: {ex} SO SKIPPING')
            #         elif prog not in tmp_chunks:
            #             tmp_chunks[prog] = set([ex])
            #         else:
            #             tmp_chunks[prog].add(ex)
            #     return list(tmp_chunks.values())



            # return

def learn_solution(settings):
    stats = Stats(log_best_programs=settings.info)
    log_level = logging.DEBUG if settings.debug else logging.INFO
    logging.basicConfig(level=log_level, stream=sys.stderr, format='%(message)s')
    timeout(popper, (settings, stats), timeout_duration=int(settings.timeout))

    if stats.solution:
        prog_stats = stats.solution
    elif stats.best_programs:
        prog_stats = stats.best_programs[-1]
    else:
        return None, stats

    return prog_stats.code, stats
