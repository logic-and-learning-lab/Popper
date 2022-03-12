import logging
import sys
from datetime import datetime
from . util import Settings, Stats, timeout, parse_settings
from . tester import Tester
# from . constrain import Constrain
from collections import defaultdict
from . core import Clause, Literal, ConstVar
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

# def get_bk()
def gen_args(args):
    return tuple(chr(ord('A') + arg.number) for arg in args)

def parse_model(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    body_atoms = []
    # print('--')
    for atom in model:
        # print(atom)
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

# def split(a, n):
#     k, m = divmod(len(a), n)
#     return (a[i*k+min(i, m):(i+1)*k+min(i+1, m)] for i in range(n))

def test_rules_clingo(stats, bk, pos, neg, rules):
    # print(f'test_rules_clingo pos:{pos}')
    result_inconsistent = {}
    result_covers = {}

    for task, task_bk in bk.items():
        task_pos = []
        for ex_task, ex in pos:
            if ex_task == task:
                task_pos.append(ex)

        if len(task_pos) == 0:
            continue

        # print(f'testing task:{task} with pos:{task_pos}')

        # print(task_bk)

        with stats.duration('tmp1'):
            # TODO: ADD NEG HERE
            for rule, inconsistent, covers in test_rules_clingo_aux(stats, task_bk, task_pos, [], rules):
                covers = set((task, ex) for ex in covers)
                # print('tmp1', format_rule(rule), inconsistent, covers)
                if rule not in result_covers:
                    result_covers[rule] = set()
                result_covers[rule].update(covers)

    to_check_rules = set()
    for rule in rules:
        # print(format_rule(rule), len(pos), len(result_covers[rule]), len(result_covers[rule]) == len(pos))
        if len(result_covers[rule]) == len(pos):
            to_check_rules.add(rule)

    # print('to_check_rules')
    # for rule in to_check_rules:
        # print('CHECK', format_rule(rule))

    for task, task_bk in bk.items():
        task_neg = []
        for ex_task, ex in neg:
            if ex_task == task:
                task_neg.append(ex)

        with stats.duration('tmp2'):
            for rule, inconsistent, covers in test_rules_clingo_aux(stats, task_bk, [], task_neg, to_check_rules):
                # print('tmp2', format_rule(rule), inconsistent)
                # assert(rule not in result_inconsistent)
                result_inconsistent[rule] = inconsistent
                if inconsistent:
                    to_check_rules.remove(rule)

    for rule in rules:
        x = 'unknown'
        if rule in result_inconsistent:
            x  = result_inconsistent[rule]
        yield rule, x, result_covers[rule]

TEST_PROG = """
#defined pos_covers/2.
#defined inconsistent/1.
#defined pos/2.
#defined neg/2.
#show pos_covers/2.
#show inconsistent/1.
pos_covers(R,E):- pos(E,Atom), holds(R,Atom).
inconsistent(R):- neg(E,Atom), holds(R,Atom).
"""

def test_rules_clingo_aux(stats, bk, pos, neg, rules):
    if len(rules) == 0:
        return []

    hash_to_rule = {}
    hash_to_ex = {}

    with stats.duration('test.build'):
        prog = []
        prog.append(TEST_PROG)

        for ex in pos:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            prog.append(f'pos({k},{ex}).')
        for ex in neg:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            prog.append(f'neg({k},{ex}).')

        for rule in rules:
            i = f'"{hash(rule)}"'
            hash_to_rule[i] = rule
            rule = format_rule(rule)
            rule = rule.replace('next_value(A)', f'holds({i},next_value(A))')
            rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
            rule = rule.replace('f(A)', f'holds({i},f(A))')
            rule = rule.replace('f(A,B)', f'holds({i},f(A,B))')
            rule = rule.replace('out(A,B,C)', f'holds({i},out(A,B,C))')
            rule = rule.replace('output(A,B,C)', f'holds({i},output(A,B,C))')
            rule = rule.replace('out(A,B,C,D)', f'holds({i},out(A,B,C,D))')
            rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
            rule = rule.replace('next_score(A,B)', f'holds({i},next_score(A,B))')
            rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
            rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
            rule = rule.replace('next_color(A,B,C)', f'holds({i},next_color(A,B,C))')
            prog.append(rule)

        prog = '\n'.join(prog)
        # print(prog + '\n' + bk)

    # solver = clingo.Control(["--single-shot", "-t16"])
    solver = clingo.Control(["--single-shot"])
    solver.add('base', [], bk)
    solver.add('base', [], prog)

    with stats.duration('test.ground'):
        solver.ground([('base', [])])

    atoms = []
    with stats.duration('test.solve'):
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms.extend(m.symbols(shown = True))

    inconsistent = {rule:False for rule in rules}
    covers = {rule:set() for rule in rules}

    with stats.duration('test.parse'):
        for atom in atoms:
            if atom.name == 'pos_covers':
                rule_hash = str(atom.arguments[0])
                example_hash = str(atom.arguments[1])
                rule = hash_to_rule[rule_hash]
                example = hash_to_ex[example_hash]
                covers[rule].add(example)
            elif atom.name == 'inconsistent':
                rule_hash = str(atom.arguments[0])
                rule = hash_to_rule[rule_hash]
                inconsistent[rule] = True

    # print('inconsistent',inconsistent)
    # print('covers',covers)
    # for rule in rules:
        # print('a',format_rule(rule), covers[rule])
    return [(rule, inconsistent[rule], covers[rule]) for rule in rules]

        # yield

def test_coverage(stats, bk, pos, rules):
    out = {}
    for task, task_bk in bk.items():
        task_pos = []
        for ex_task, ex in pos:
            if ex_task == task:
                task_pos.append(ex)
        if len(task_pos) == 0:
            continue
        # print('<BK2>')
        # print(task_bk)
        # print('</BK2>')
        # print('<task_pos>')
        # print(task_pos)
        # print('</task_pos>')
        # for rule in rules:
            # print('X', format_rule(rule))
        res = test_coverage_aux(stats, task_bk, task_pos, rules)
        for rule, covers in res.items():
            covers = set((task, ex) for ex in covers)
            if rule not in out:
                out[rule] = set()
            out[rule].update(covers)
    return out

def test_coverage_aux(stats, bk, pos, rules):
    assert(len(rules) > 0)

    with stats.duration('test.build'):
        prog = []
        prog.append("#show covers/2.")
        prog.append("covers(R,E):- example(E,Atom), holds(R,Atom).")
        hash_to_rule = {}
        hash_to_ex = {}

        for ex in pos:
            k = f'"{hash(ex)}"'
            hash_to_ex[k] = ex
            prog.append(f'example({k},{ex}).')

        for rule in rules:
            i = f'"{hash(rule)}"'
            hash_to_rule[i] = rule
            rule = format_rule(rule)

            rule = rule.replace('next_value(A)', f'holds({i},next_value(A))')
            rule = rule.replace('next_value(A,B)', f'holds({i},next_value(A,B))')
            rule = rule.replace('f(A)', f'holds({i},f(A))')
            rule = rule.replace('f(A,B)', f'holds({i},f(A,B))')
            rule = rule.replace('out(A,B,C)', f'holds({i},out(A,B,C))')
            rule = rule.replace('output(A,B,C)', f'holds({i},output(A,B,C))')
            rule = rule.replace('out(A,B,C,D)', f'holds({i},out(A,B,C,D))')
            rule = rule.replace('next_score(A,B,C)', f'holds({i},next_score(A,B,C))')
            rule = rule.replace('next_score(A,B)', f'holds({i},next_score(A,B))')
            rule = rule.replace('next(A,B)', f'holds({i},next(A,B))')
            rule = rule.replace('next_cell(A,B,C)', f'holds({i},next_cell(A,B,C))')
            rule = rule.replace('next_color(A,B,C)', f'holds({i},next_color(A,B,C))')
            prog.append(rule)

        prog = '\n'.join(prog)
    # print(prog)

    # solver = clingo.Control(["--single-shot", "-t16"])
    solver = clingo.Control(["--single-shot"])
    solver.add('base', [], bk)
    solver.add('base', [], prog)

    with stats.duration('test.ground'):
        solver.ground([('base', [])])

    atoms = []
    with stats.duration('test.solve'):
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms.extend(m.symbols(shown = True))

    out = {}
    with stats.duration('test.parse'):
        for atom in atoms:
            # print(atom)
            rule_hash = str(atom.arguments[0])
            rule = hash_to_rule[rule_hash]

            example_hash = str(atom.arguments[1])
            example = hash_to_ex[example_hash]

            if rule not in out:
                out[rule] = set()
            out[rule].add(example)
    return out

def find_subset(examples, all_rules, sizes, covers):
    prog = []

    example_to_hash = {}
    for x in examples:
        k = f'"{hash(x)}"'
        example_to_hash[x] = k

    for x in examples:
        k = example_to_hash[x]
        prog.append(f'example({k}).')

    i = 0
    rule_to_index = {}
    index_to_rule = {}
    for rule, xs in covers.items():
        # print(i, format_rule(rule), xs)
        prog.append('{rule(' + str(i) + ')}.')
        index_to_rule[i] = rule
        rule_to_index[rule] = i
        for x in xs:
            k = example_to_hash[x]
            prog.append(f'covers({i},{k}).')
        i +=1
    for rule in all_rules:
        size = sizes[rule]
        if rule not in rule_to_index:
            continue
        i = rule_to_index[rule]
        prog.append(f'size({i},{size}).')

    prog = '\n'.join(prog)

    # print(prog)

    prog += """
    covered(E):- covers(R,E), rule(R).
    :- example(E), not covered(E).
    #show rule/1.
    size(N):- #sum{K,R : rule(R), size(R,K)} == N.
    #minimize{X : size(X)}.
    """
    # with open('sat-prob.pl', 'w') as f:
        # f.write(prog)

    # solver = clingo.Control(["--single-shot", "-t16"])
    solver = clingo.Control(["--single-shot"])
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

NUM_LITERALS = """
%%% External atom for number of literals in the program %%%%%
#external size_in_literals(n).
:-
    size_in_literals(n),
    #count{P,Vars : body_literal(P,_,Vars)} != n.
"""

def get_solver(stats, settings, cons = set()):

    check_cons(cons)

    solver = clingo.Control()
    # solver = clingo.Control(["-t16"])
    solver.configuration.solve.models = 0
    with open('popper/lp/alan.pl') as f:
        solver.add('base', [], f.read())
    with open(settings.bias_file) as f:
        solver.add('base', [], f.read())
    # add bootstap constraints
    solver.add('base', [], '\n'.join(cons))

    solver.ground([('base', [])])

    solver.add('number_of_literals', ['n'], NUM_LITERALS)

    return solver

def update_num_literals(solver, tracker, size):
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
    with stats.duration('gen.ground.cons'):
        if len(cons) > 0:
            k = f'cons_{size}'
            dbg(f'gen.ground.cons {k} {len(cons)}')
            solver.add(k, [], '\n'.join(cons))
            solver.ground([(k, [])])

    with stats.duration('gen.solve'):
        models = []
        with solver.solve(yield_=True) as handle:
            for m in handle:
                models.append(m.symbols(shown = True))

    with stats.duration('gen.build'):
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

    # print('ASDA',len(cons), len(seen))


def vo_variable(variable):
    return ConstVar(f'{variable}', 'Variable')

def format_constraint(con):
    head, body = con
    constraint_literals = []
    for constobj in body:
        if not constobj.meta:
            constraint_literals.append(str(constobj))
            continue
        arga, argb = constobj.arguments
        if isinstance(arga, ConstVar):
            arga = arga.name
        else:
            arga = str(arga)
        if isinstance(argb, ConstVar):
            argb = argb.name
        else:
            argb = str(argb)
        constraint_literals.append(f'{arga}{constobj.predicate}{argb}')

    x = f':- {", ".join(constraint_literals)}.'
    if head:
        x = f'{head} {x}'
    # print(x)
    return x

def specialisation_constraint(rule):
    literals = []
    head, body = rule
    literals.append(Literal('head_literal', (head.predicate, head.arity, tuple(vo_variable(v) for v in head.arguments))))
    for body_literal in body:
        literals.append(Literal('body_literal', (body_literal.predicate, body_literal.arity, tuple(vo_variable(v) for v in body_literal.arguments))))
    return format_constraint((None, tuple(literals)))

def find_rules(settings, stats, bk, pos, neg, boostrap_cons, max_size):
    assert(len(pos) > 0)

    with stats.duration('get_solver'):
        solver = get_solver(stats, settings, boostrap_cons)

    # track the enabled/disabled size literals
    tracker = {}

    # complete rules found
    complete_rules = set()

    # all news
    cons = set()

    # new cons
    new_cons = set()

    seen_ss = {}

    max_size=8

    size = 1
    while size < max_size:
        size += 1
        body_size = size-1

        # increase program size constraint
        update_num_literals(solver, tracker, body_size)

        dbg(f'size:{size} cons:{len(cons)} pos:{len(pos)}')

        # generate all rules of size body_size + 1 that satisfy the constraints
        rules = list(gen_rules(settings, stats, body_size, cons, solver))

        # print("HELLO")
        dbg(f'num_rules:{len(rules)}')
        # for rule in rules:
        #     print(format_rule(rule))

        # reset cons
        cons = set()

        # # remove rules that contain redundant literals
        # with stats.duration('check_redundant'):
        #     good_rules = []
        #     for rule in rules:
        #         if tester.rule_has_redundant_literal(rule):
        #             # TODO: RULE OUT GENERALISATIONS
        #             # gen_cons.add
        #             pass
        #         else:
        #             good_rules.append(rule)
        #     rules = good_rules

        # rules = ptest_rules_clingo(tester, stats, rules, pos)
        # print('ASDA')

        # test rules on the subset of examples
        rules = test_rules_clingo(stats, bk, pos, neg, rules)

        for rule, inconsistent, coverage in rules:
            # if not inconsistent and len(coverage) > 0:
                # print('E', format_rule(rule), coverage)

            # if the rule does not cover the examples, eliminate specialisations
            if len(coverage) != len(pos):
                con = specialisation_constraint(rule)
                cons.add(con)
                new_cons.add(con)
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
            con = specialisation_constraint(rule)
            cons.add(con)
            new_cons.add(con)

            # print('complete_rules.add(rule)',format_rule(rule))
            complete_rules.add((rule, size))

        # print('HERE?????', len(complete_rules))
        if len(complete_rules) > 0:
            # print('COMPLETE RULES RULEZ!')
            return complete_rules, new_cons


    return [], new_cons

def chunk_list(xs, size):
    for i in range(0, len(xs), size):
        yield xs[i:i+size]

def flatten(xs):
    return [item for sublist in xs for item in sublist]

def parse_exs(task, exs_txt):
    solver = clingo.Control()
    solver.add('base', [], exs_txt)
    solver.ground([('base', [])])
    with solver.solve(yield_=True) as handle:
        for m in handle:
            for atom in m.symbols(shown = True):
                yield atom.name, task, str(atom.arguments[0])

def parse_input(settings):
    with open(settings.bk_file.replace('bk','bk-all'), 'r') as f:
        all_bk = f.read()
        # print(all_bk)

    bk = {}

    with open(settings.bk_file, 'r') as f:
        x = f.read()
    txt = ''
    for line in x.split('\n'):
        if line.startswith('#T'):
            if txt != '':
                bk[task] = txt
                txt = ''
            task = int(line.strip()[2:])
        else:
            txt += line + '\n'
    if txt != '':
        bk[task] = txt

    for task in bk:
        bk[task] += '\n' + all_bk

    examples = {}
    with open(settings.ex_file, 'r') as f:
        x = f.read()
        if '#T' not in x:
            pass
            # parse file
        else:
            tasks = set()
            txt = ''
            for line in x.split('\n'):
                if line.startswith('#T'):
                    if txt != '':
                        examples[task] = txt
                        txt = ''
                    task = int(line.strip()[2:])
                else:
                    txt += line + '\n'
    if txt != '':
        examples[task] = txt

    pos = set()
    neg = set()
    for k, v in examples.items():
        for label, task, ex in parse_exs(k, v):
            if label == 'pos':
                pos.add((task, ex))
            elif label == 'neg':
                neg.add((task, ex))
    return bk, pos, neg

def popper(settings, stats):
    bk, all_pos, all_neg = parse_input(settings)

    # print(bk.keys())

    load_settings(settings)
    # exit()
    # deduce_bk_cons(settings, tester.bk)

    print(f'num_examples:{len(all_pos)}')

    # maintain specialisation constraints
    spec_cons = {x: set() for x in all_pos}

    pos = set(all_pos)
    # pos = set((task, ex) for task, ex in pos if task == 10)

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
            new_rules, new_cons = find_rules(settings, stats, bk, chunk_pos, all_neg, boostrap_cons, max_size=max_size)
            # exit()

            for rule, size in new_rules:
                sizes[rule] = size
            new_rules = [rule for rule, size in new_rules]

            # keep track of all rules
            all_new_rules.update(new_rules)

            # update the cons for each example
            for x in chunk_pos:
                spec_cons[x].update(new_cons)

            # no generalisation
            # TODO: update min rules
            if len(new_rules) == 0:
                return
                continue

            # print('NEW RULES')
            # for rule in new_rules:
            #     print(format_rule(rule))

            # print('CHECK COVERAGE')
            with stats.duration('coverage'):
                # print('CHECK COVERAGE')
                # print('CHUNK_POS',chunk_pos)
                # calculate example coverage of the new rules
                covers = test_coverage(stats, bk, all_pos, new_rules)
                for rule, covered_examples in covers.items():
                    # print('COVERED_EXAMPLES',covered_examples)

                    assert(set(chunk_pos).issubset(covered_examples))

                    # update rule example coverage
                    if rule in rule_example_coverage:
                        # print('xs', rule_example_coverage[rule])
                        # print('ys', covered_examples)
                        assert(rule_example_coverage[rule] == covered_examples)
                    rule_example_coverage[rule] = covered_examples


                    # print(format_rule(rule))
                    # for x in sorted(covered_examples):
                        # print(x)

                    # update covered examples
                    covered.update(covered_examples)

        # exit()
        # after passing through all chunks, double chunk size
        chunk_size += chunk_size
        all_rules.update(all_new_rules)

        assert(len(all_rules) > 0)

        # if no new rules, skip to the next chunk size
        if len(new_rules) == 0:
            continue


        if all(len(rule_example_coverage[rule]) == 0 for rule in new_rules):
            assert(False)
            continue

        with stats.duration('find_subset'):
            print(f'subset problem:{len(all_rules)}')

            # find the minimal subset of rules that covers all the examples
            rules = list(find_subset(all_pos, all_rules, sizes, rule_example_coverage))

            # if there is no set, then continue to search
            if len(rules) == 0:
                continue

            print('Solution:')
            for rule in rules:
                print(format_rule(rule))
            # exit()
            return
        return

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
