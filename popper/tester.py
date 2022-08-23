import os
import time
import numpy as np
import pkg_resources
from pyswip import Prolog
from contextlib import contextmanager
from . util import format_rule, order_rule, order_prog, prog_is_recursive, format_prog

import clingo
import clingo.script
import pkg_resources
from . core import Literal, ConstVar
from . generate import parse_model
from collections import defaultdict
# clingo.script.enable_python()
arg_lookup = {clingo.Number(i):chr(ord('A') + i) for i in range(100)}
# reverse_lookup = {clingo.ord('A') + i) for i in range(100)}

def literal_hash(literal):
    return hash((literal.predicate, literal.arguments))

# @profile
def parse_model3(literal_index, atoms):
    body = set()
    for atom in atoms:
        if atom.name == 'selected':
            body.add(literal_index[atom.arguments[0].number])
    return frozenset(body)

# @profile
def parse_model2(model):
    directions = defaultdict(lambda: defaultdict(lambda: '?'))
    rule_index_to_body = defaultdict(set)
    rule_index_to_head = {}
    rule_index_ordering = defaultdict(set)

    for atom in model:
        args = atom.arguments
        if atom.name == 'body_literal':
            rule_index = args[0].number
            predicate = args[1].name
            atom_args = args[3].arguments
            atom_args = tuple(arg_lookup[arg] for arg in atom_args)
            arity = len(atom_args)
            body_literal = (predicate, atom_args, arity)
            rule_index_to_body[rule_index].add(body_literal)
        elif atom.name == 'head_literal':
            rule_index = args[0].number
            predicate = args[1].name
            atom_args = args[3].arguments
            atom_args = tuple(arg_lookup[arg] for arg in atom_args)
            arity = len(atom_args)
            head_literal = (predicate, atom_args, arity)
            rule_index_to_head[rule_index] = head_literal

    prog = []
    rule_lookup = {}

    # rules = set(rule_index_to_head.keys()).union(set(rule_index_to_body.keys()))
    # for rule_index in rules:
    #     head = None
    #     if rule_index in rule_index_to_head:
    for rule_index in rule_index_to_head:
        head_pred, head_args, head_arity = rule_index_to_head[rule_index]
        head_modes = tuple(directions[head_pred][i] for i in range(head_arity))
        head = Literal(head_pred, head_args, head_modes)
        body = set()
        for (body_pred, body_args, body_arity) in rule_index_to_body[rule_index]:
            body_modes = tuple(directions[body_pred][i] for i in range(body_arity))
            body.add(Literal(body_pred, body_args, body_modes))
        body = frozenset(body)
        rule = head, body
        prog.append((rule))
        rule_lookup[rule_index] = rule

    rule_ordering = defaultdict(set)
    for r1_index, lower_rule_indices in rule_index_ordering.items():
        r1 = rule_lookup[r1_index]
        rule_ordering[r1] = set(rule_lookup[r2_index] for r2_index in lower_rule_indices)

    return frozenset(prog), rule_ordering

def vars_to_ints(vars):
    return tuple(ord(v) - 65 for v in vars)

def build_explain_encoding(prog, with_directions=False):
    literal_index = {}
    encoding = set()
    id_count = 0
    big_head = None
    for rule in prog:
        head, body = rule
        big_head = head
        rule_vars = set()
        rule_vars.add(vars_to_ints(head.arguments))
        # encoding.add(f'{{head_literal(0,{head.predicate},{head.arity},{vars_to_ints(head.arguments)})}}.')
        encoding.add(f'head_literal(0,{head.predicate},{head.arity},{vars_to_ints(head.arguments)}).')
        encoding.add(f'arity({head.predicate},{head.arity}).')
        for literal in body:
            id_count += 1
            rule_vars.add(vars_to_ints(literal.arguments))
            encoding.add(f'arity({literal.predicate},{literal.arity}).')
            encoding.add(f'{{body_literal(0,{literal.predicate},{literal.arity},{vars_to_ints(literal.arguments)})}}.')
            encoding.add(f'selected({id_count}):-body_literal(0,{literal.predicate},{literal.arity},{vars_to_ints(literal.arguments)}).')
            literal_index[id_count] = literal
        encoding.add(f':- size({len(body)}).')

        if with_directions:
            for xs in rule_vars:
                encoding.add(f'vars({len(xs)},{xs}).')
    return literal_index, encoding




    # direction(f,(in,)).
    # direction(has_car,(in,out)).
    # direction(has_load,(in,out)).
    # direction(short,(in,)).


class Tester():

    def query(self, query, key):
        return set(next(self.prolog.query(query))[key])

    def bool_query(self, query,):
        return len(list(self.prolog.query(query))) > 0

    def load_directions(self):
        enc = """
#defined clause/1.
#defined clause_var/2.
#defined var_type/3."""
        solver = clingo.Control()
        with open(self.settings.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], enc)
        solver.ground([('bias', [])])
        encoding = set()
        for x in solver.symbolic_atoms.by_signature('direction', arity=2):
            pred = x.symbol.arguments[0]
            directions = x.symbol.arguments[1]
            for i, arg in enumerate(directions.arguments):
                encoding.add(f'direction({pred},{i},{arg}).')
        return encoding

    # TODO: COULD PUSH TO CLINGO TO SAVE PROLOG FROM HAVING TO INDEX STUFF
    def get_examples(self):
        pos = set()
        neg = set()

        pos = self.query('findall(X,pos(X),Xs)', 'Xs')

        if self.bool_query('current_predicate(neg/1)'):
            neg = self.query('findall(X,neg(X),Xs)', 'Xs')

        self.settings.stats.logger.info(f'Num. pos examples: {len(pos)}')
        self.settings.stats.logger.info(f'Num. neg examples: {len(neg)}')

        if self.settings.max_examples < len(pos):
            self.settings.stats.logger.info(f'Sampling {self.settings.max_examples} pos examples')
            pos = np.random.choice(list(pos), self.settings.max_examples)
        if self.settings.max_examples < len(neg):
            self.settings.stats.logger.info(f'Sampling {self.settings.max_examples} neg examples')
            neg = np.random.choice(list(neg), self.settings.max_examples)

        self.cached_explain = set()
        self.seen_prog = set()

        return pos, neg

    def __init__(self, settings):
        self.settings = settings
        self.prolog = Prolog()

        bk_pl_path = self.settings.bk_file
        exs_pl_path = self.settings.ex_file
        test_pl_path = pkg_resources.resource_filename(__name__, "lp/test.pl")

        self.explain_encoding = pkg_resources.resource_string(__name__, "lp/explain.pl").decode()
        self.explain_dir_encoding = pkg_resources.resource_string(__name__, "lp/explain-dirs.pl").decode()
        self.directions = self.load_directions()
        self.has_directions = len(self.directions) > 0



        for x in [exs_pl_path, bk_pl_path, test_pl_path]:
            if os.name == 'nt': # if on Windows, SWI requires escaped directory separators
                x = x.replace('\\', '\\\\')
            self.prolog.consult(x)

        self.pos_index = {}
        self.neg_index = {}

        pos, neg = self.get_examples()
        self.num_pos = len(pos)
        self.num_neg = len(neg)

        for i, atom in enumerate(pos):
            k = i+1
            self.prolog.assertz(f'pos_index({k},{atom})')
            self.pos_index[k] = atom

        for i, atom in enumerate(neg):
            k = -(i+1)
            self.prolog.assertz(f'neg_index({k},{atom})')
            self.neg_index[k] = atom

        self.settings.pos = frozenset(self.pos_index.values())
        self.settings.neg = frozenset(self.neg_index.values())

        if self.settings.recursion_enabled:
            self.prolog.assertz(f'timeout({self.settings.eval_timeout})')


    # neg_covered = frozenset(next(self.prolog.query('neg_covered(Xs)'))['Xs'])
    # neg_covered = frozenset(self.neg_index[i] for i in neg_covered)

    def test_prog(self, prog):
        with self.using(prog):
            pos_covered = frozenset(self.query('pos_covered(Xs)', 'Xs'))
            pos_covered = frozenset(self.pos_index[i] for i in pos_covered)
            inconsistent = False
            if len(self.neg_index):
                inconsistent = len(list(self.prolog.query("inconsistent"))) > 0
        return pos_covered, inconsistent

    def is_inconsistent(self, prog):
        if len(self.neg_index) == 0:
            return False
        with self.using(prog):
            return len(list(self.prolog.query("inconsistent"))) > 0

    @contextmanager
    def using(self, prog):
        if self.settings.recursion_enabled:
            prog = order_prog(prog)
        current_clauses = set()
        try:
            for rule in prog:
                head, _body = rule
                x = format_rule(order_rule(rule))[:-1]
                self.prolog.assertz(x)
                current_clauses.add((head.predicate, head.arity))
            yield
        finally:
            for predicate, arity in current_clauses:
                args = ','.join(['_'] * arity)
                self.prolog.retractall(f'{predicate}({args})')

    def is_non_functional(self, prog):
        with self.using(prog):
            return self.bool_query('non_functional')

    def reduce_inconsistent(self, program):
        if len(program) < 3:
            return program
        for i in range(len(program)):
            subprog = program[:i] + program[i+1:]
            if not prog_is_recursive(subprog):
                continue
            with self.using(subprog):
                if self.is_inconsistent(subprog):
                    return self.reduce_inconsistent(subprog)
        return program

    def explain_totally_incomplete(self, prog):
        for head, body in prog:
            ps = powerset(body)
            for subbody in ps:
                subbody = frozenset(subbody)
                if len(subbody) == 0 or len(subbody) == len(body):
                    continue
                k = hash(frozenset(literal_hash(x) for x in subbody))
                if k in self.cached_explain:
                    continue
                self.cached_explain.add(k)
                new_rule = (head, subbody)
                try:
                    new_rule = order_rule(new_rule)
                except:
                    continue
                sub_prog = [new_rule]
                with self.using(sub_prog):
                    if not self.bool_query('sat'):
                        yield sub_prog


    def add_seen_prog(self, prog):
        subbody = list(prog)[0][1]
        k = hash(frozenset(literal_hash(x) for x in subbody))
        self.seen_prog.add(k)

    # @profile
    # TODO: PRUNE PROGRAMS KNOWN TO BE PARTIALLY COMPLETE
    # TODO: PRUNE PROGRAMS KNOWN TO BE TOTALLY INCOMPLETE
    # TODO: ADD CONSTRAINTS THE SOLVER WHILST ENUMERATING UNSAT RULES
    # TODO: TRY RULES WITHOUT HEAD LITERALS
    # TODO: CHECK SUBSUMPTION
    #   f(A):-x(A)
    #   f(A):-x(B)
    #   f(A):-x(C)
    #   the above should all fail
    # TODO: RENAME VARIABLES
    #   f(A):- has_car(A,B).
    #   f(A):- has_car(A,C).



    def explain_totally_incomplete2(self, prog):
        encoding = set()
        encoding.add(self.explain_encoding)
        if self.has_directions:
            encoding.add(self.explain_dir_encoding)
        literal_index, prog_encoding = build_explain_encoding(prog, self.has_directions)
        encoding.update(prog_encoding)




        with open('DBG-explain.pl','w') as f:
            tmp = '\n'.join(sorted(list(encoding)))
            f.write(tmp)


        encoding = '\n'.join(encoding)

        big_head = None
        for rule in prog:
            head, body = rule
            big_head = head

        with self.settings.stats.duration('explain_clingo'):
            subprogs = tuple(self.find_subprogs(big_head, literal_index, encoding))

        with self.settings.stats.duration('explain_py'):
            subprogs2 = list(powerset(body))

        # for rule in prog:
            # print('trying', format_rule(rule))

        with self.settings.stats.duration('explain_prolog'):
            for subprog in subprogs:
                try:
                    with self.using(subprog):
                        if not self.bool_query('sat'):
                            yield subprog
                            print('\tUNSAT')
                            for rule in subprog:
                                print('\t', format_rule(rule))
                except:
                    pass


                    # for nogood in nogoods:
                    # model.context.add_nogood(nogood)

    def find_subprogs(self, head, literal_index, encoding):
        # with open('dbg-clingo-explain.pl', 'w') as f:
            # f.write(encoding)
        solver = clingo.Control(["--heuristic=Domain"])
        solver.configuration.solve.models = 0
        solver.add('base', [], encoding)
        solver.ground([('base', [])])
        with solver.solve(yield_=True) as handle:
            for m in handle:
                atoms = m.symbols(shown = True)
                # sub_prog, _ordering = parse_model2(atoms)
                subbody = parse_model3(literal_index, atoms)
                sub_prog = [(head, subbody)]

                k = hash(frozenset(literal_hash(x) for x in subbody))
                if k in self.cached_explain:
                    continue
                self.cached_explain.add(k)

                if k in self.seen_prog:
                    continue

                yield sub_prog


from itertools import chain, combinations
def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))