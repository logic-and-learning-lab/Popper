from functools import cache
import clingo
import clingo.script
import signal
import argparse
import os
import logging
from itertools import permutations, chain, combinations
from collections import defaultdict
from typing import NamedTuple
from time import perf_counter
from contextlib import contextmanager

class Literal(NamedTuple):
    predicate: str
    arguments: tuple

clingo.script.enable_python()

TIMEOUT=1200
EVAL_TIMEOUT=0.001
MAX_LITERALS=40
MAX_SOLUTIONS=1
CLINGO_ARGS=''
MAX_RULES=2
MAX_VARS=6
MAX_BODY=6
MAX_EXAMPLES=10000
BATCH_SIZE=20000
ANYTIME_TIMEOUT=10
BKCONS_TIMEOUT=10

class Constraint:
    GENERALISATION = 1
    SPECIALISATION = 2
    UNSAT = 3
    REDUNDANCY_CONSTRAINT1 = 4
    REDUNDANCY_CONSTRAINT2 = 5
    TMP_ANDY = 6
    BANISH = 7

def parse_args():
    parser = argparse.ArgumentParser(description='Popper is an ILP system based on learning from failures')

    parser.add_argument('kbpath', help='Path to files to learn from')
    parser.add_argument('--noisy', default=False, action='store_true', help='tell Popper that there is noise')
    # parser.add_argument('--bkcons', default=False, action='store_true', help='deduce background constraints from Datalog background (EXPERIMENTAL!)')
    parser.add_argument('--timeout', type=float, default=TIMEOUT, help=f'Overall timeout in seconds (default: {TIMEOUT})')
    parser.add_argument('--max-literals', type=int, default=MAX_LITERALS, help=f'Maximum number of literals allowed in program (default: {MAX_LITERALS})')
    parser.add_argument('--max-body', type=int, default=None, help=f'Maximum number of body literals allowed in rule (default: {MAX_BODY})')
    parser.add_argument('--max-vars', type=int, default=None, help=f'Maximum number of variables allowed in rule (default: {MAX_VARS})')
    parser.add_argument('--max-rules', type=int, default=None, help=f'Maximum number of rules allowed in a recursive program (default: {MAX_RULES})')
    parser.add_argument('--eval-timeout', type=float, default=EVAL_TIMEOUT, help=f'Prolog evaluation timeout in seconds (default: {EVAL_TIMEOUT})')
    parser.add_argument('--stats', default=True, action='store_true', help='Print statistics at end of execution')
    parser.add_argument('--quiet', '-q', default=False, action='store_true', help='Hide information during learning')
    parser.add_argument('--debug', default=False, action='store_true', help='Print debugging information to stderr')
    parser.add_argument('--showcons', default=False, action='store_true', help='Show constraints deduced during the search')
    parser.add_argument('--solver', default='rc2', choices=['clingo', 'rc2', 'uwr', 'wmaxcdcl'], help='Select a solver for the combine stage (default: rc2)')
    parser.add_argument('--anytime-solver', default=None, choices=['wmaxcdcl', 'nuwls'], help='Select an anytime MaxSAT solver (default: None)')
    parser.add_argument('--anytime-timeout', type=int, default=ANYTIME_TIMEOUT, help=f'Maximum timeout (seconds) for each anytime MaxSAT call (default: {ANYTIME_TIMEOUT})')
    parser.add_argument('--batch-size', type=int, default=BATCH_SIZE, help=f'Combine batch size (default: {BATCH_SIZE})')
    parser.add_argument('--functional-test', default=False, action='store_true', help='Run functional test')
    # parser.add_argument('--datalog', default=False, action='store_true', help='EXPERIMENTAL FEATURE: use recall to order literals in rules')
    # parser.add_argument('--no-bias', default=False, action='store_true', help='EXPERIMENTAL FEATURE: do not use language bias')
    # parser.add_argument('--order-space', default=False, action='store_true', help='EXPERIMENTAL FEATURE: search space ordered by size')

    return parser.parse_args()

def timeout(settings, func, args=(), kwargs={}, timeout_duration=1):
    result = None
    class TimeoutError(Exception):
        pass

    def handler(signum, frame):
        raise TimeoutError()

    # set the timeout handler
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout_duration)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as _exc:
        settings.logger.warn(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
        return result
    except AttributeError as moo:
        if '_SolveEventHandler' in str(moo):
            settings.logger.warn(f'TIMEOUT OF {int(settings.timeout)} SECONDS EXCEEDED')
            return result
        raise moo
    finally:
        signal.alarm(0)

    return result

def load_kbpath(kbpath):
    def fix_path(filename):
        full_filename = os.path.join(kbpath, filename)
        return full_filename.replace('\\', '\\\\') if os.name == 'nt' else full_filename
    return fix_path("bk.pl"), fix_path("exs.pl"), fix_path("bias.pl")

class Stats:
    def __init__(self, info = False, debug = False):
        self.exec_start = perf_counter()
        self.total_programs = 0
        self.durations = {}

    def total_exec_time(self):
        return perf_counter() - self.exec_start

    def show(self):
        message = f'Num. programs: {self.total_programs}\n'
        total_op_time = sum(summary.total for summary in self.duration_summary())

        for summary in self.duration_summary():
            percentage = int((summary.total/total_op_time)*100)
            message += f'{summary.operation}:\n\tCalled: {summary.called} times \t ' + \
                       f'Total: {summary.total:0.2f} \t Mean: {summary.mean:0.4f} \t ' + \
                       f'Max: {summary.maximum:0.3f} \t Percentage: {percentage}%\n'
        message += f'Total operation time: {total_op_time:0.2f}s\n'
        message += f'Total execution time: {self.total_exec_time():0.2f}s'
        print(message)

    def duration_summary(self):
        summary = []
        stats = sorted(self.durations.items(), key = lambda x: sum(x[1]), reverse=True)
        for operation, durations in stats:
            called = len(durations)
            total = sum(durations)
            mean = sum(durations)/len(durations)
            maximum = max(durations)
            summary.append(DurationSummary(operation.title(), called, total, mean, maximum))
        return summary

    @contextmanager
    def duration(self, operation):
        start = perf_counter()
        try:
            yield
        finally:
            end = perf_counter()
            duration = end - start

            if operation not in self.durations:
                self.durations[operation] = [duration]
            else:
                self.durations[operation].append(duration)


# def format_prog2(prog):
    # return '\n'.join(format_rule(order_rule2(rule)) for rule in order_prog(prog))

def format_literal(literal):
    pred, args = literal
    args = ','.join(f'V{i}' for i in args)
    return f'{pred}({args})'

def format_rule(rule):
    head, body = rule
    head_str = ''
    if head:
        head_str = format_literal(head)
    body_str = ','.join(format_literal(literal) for literal in body)
    return f'{head_str}:- {body_str}.'

def calc_prog_size(prog):
    return sum(calc_rule_size(rule) for rule in prog)

def calc_rule_size(rule):
    head, body = rule
    return 1 + len(body)

def reduce_prog(prog):
    reduced = {}
    for rule in prog:
        head, body = rule
        k = head, frozenset(body)
        reduced[k] = rule
    return reduced.values()

def order_prog(prog):
    return sorted(list(prog), key=lambda rule: (rule_is_recursive(rule), len(rule[1])))

def rule_is_recursive(rule):
    head, body = rule
    head_pred, _head_args = head
    if not head:
        return False
    return any(head_pred == pred for pred, _args in body)

def prog_is_recursive(prog):
    if len(prog) < 2:
        return False
    return any(rule_is_recursive(rule) for rule in prog)

def prog_has_invention(prog):
    if len(prog) < 2:
        return False
    return any(rule_is_invented(rule) for rule in prog)

def rule_is_invented(rule):
    head, body = rule
    if not head:
        return False
    head_pred, _head_arg = head
    return head_pred.startswith('inv')

def mdl_score(fn, fp, size):
    return fn + fp + size

import numpy as np
import math

class Binomial_MML: 
    #this class calculates the Exact MML score for the binomial distribution
    #it should only be called once - once computed, the theta and q values can be determined by table lookup
    def __init__(self, N):
        self.N = N
        self.nlogn = lambda n: 0 if n>=1.0 or n <= 0.0 else n*np.log2(n)
        self.log_comb = lambda n: 0. if n==self.N or n == 0 else (math.lgamma(self.N) - math.lgamma(self.N-n) - math.lgamma(n)) * np.log2(np.e)
        self.run()
        self.int_to_theta = {i:j for i,j,_ in self.partition}
        self.int_to_q = {i:j for i,_,j in self.partition}

    def binomial_func(self,theta, n):
        return math.comb(self.N, n) * theta**n * (1-theta)**(self.N-n)
    def get_group_estimate(self,q,k,m):
        return (1/(self.N*q)) * sum(n/(self.N+1) for n in range(k,m+1))
    def get_contribution(self,k,m):
        r_n = 1/(self.N+1)
        q = r_n*(m-k+1)
        theta = self.get_group_estimate(q,k,m)
        fp = -q*np.log2(q)
        sp = - sum(r_n*self.log_comb(n) for n in range(k,m+1)) - self.N*q*(self.nlogn(theta) + self.nlogn(1-theta) )
        return fp + sp, theta, q

    def run(self):
        T = {self.N+1:0}
        Q = {self.N+1:((),)}
        for m in range(self.N,-1,-1):
            #options are m, m+1 .. m+3
            bst_q, bst_t = None, 0
            for k in range(m,self.N+1): #try each k from m..N
                #print(m,k)
                l, theta, q = self.get_contribution(m,k)
                Q_m = (((m,k), round(theta,3), round(-np.log2(q),2)),) + Q[k+1]
                T_m = l + T[k+1]
                #print(Q_m)
                #print(T_m)
                if k == m: #first iteration
                    bst_t = T_m
                    bst_q = Q_m
                elif T_m < bst_t:
                    #print('triggered', T_m, )
                    bst_t = T_m
                    bst_q = Q_m
            #print(bst_q, bst_t)
            T[m] = bst_t
            Q[m] = bst_q
        self.partition = Q[0][:-1]
        return self.partition
    
    def get_theta(self, a):
        for mn,mx in self.int_to_theta.keys():
            if a >= mn and a <= mx:
                return self.int_to_theta[(mn,mx)]
    def get_q(self,a):
        for mn,mx in self.int_to_q.keys():
            if a >= mn and a <= mx:
                return self.int_to_q[(mn,mx)]

log_comb2 = lambda N, n: 0. if n>=N or n <= 0 else (math.lgamma(N+1) - math.lgamma(N-n+1) - math.lgamma(n+1)) * np.log2(np.e) #helper function to compute large log(nCr)
def logstar(N):
        #this is an approximation to the logstar code
        if N == 0:
            raise ValueError('N must be greater than 0')
        if N < 2:
            return np.log2(N)
        else:
            return np.log2(N) + np.log2(np.log2(N))
class MML_Score:
    def __init__(self, prog, head_arity, body_arities, train_res, pos_bin_obj, neg_bin_obj):
        self.prog = prog
        self.head_arity = head_arity
        self.body_arities = body_arities
        self.head_bits = {1: 0.0,
                            2: 1.0,
                            3: 2.32,
                            4: 3.91,
                            5: 5.7,
                            6: 7.67,
                            7: 9.78,
                            8: 12.02,
                            9: 14.37,
                            10: 16.82,
                            11: 19.37,
                            12: 22.01,
                            13: 24.72,
                            14: 27.51,
                            15: 30.37,
                            16: 33.29,
                            17: 36.27,
                            18: 39.31,
                            19: 42.41,
                            20: 45.56,
                            21: 48.75,
                            22: 52.0,
                            23: 55.29,
                            24: 58.63,
                            25: 62.01} #bits required to specify the particular head configuration for of a particular arity computed up to head arity 25 
        self.spec_num_new_vars = lambda n: np.log2(2**(n+1)) #bits required to specify number of new variables: p(0) = 1/2, p(1) = 1/4 ...
        self.tp, self.fn, self.tn, self.fp = train_res
        self.pos_bin_obj, self.neg_bin_obj = pos_bin_obj, neg_bin_obj
    
    def spec_num_body(self, head_arity, n_new_vars, pred_arities):
        #calculate bits required to specify number of body variables
        total = 0
        for ar in pred_arities:
            total += ar*(head_arity+n_new_vars)
        total -= math.factorial(n_new_vars)
        return total, np.log2(total)
    def spec_body(self,num_total, num_body):
        #calculate bits required to specify the particular body variable configuration
        return log_comb2(num_total, num_body)
        
    def compute_theory_nums(self):
        #helper function to compute relevant numbers for calculating the mesage length
        n_clauses = len(self.prog) #len(self.theory)
        arity_head = len(list(self.prog)[0][0].arguments)
        clause_nums = []
        for rule in self.prog:
            new_vars = set()
            head, body = rule
            mx_head = max(head.arguments)
            for body_lit in body:
                #[new_vars.add(j) for j in body_literal.arguments if j not in head.arguments]
                [new_vars.add(j) for j in body_lit.arguments if j > mx_head]
            clause_nums.append([len(new_vars), len(body)])
        return n_clauses, arity_head, clause_nums
    def MML_first_part(self):
        n_clauses, arity_head, clause_nums = self.compute_theory_nums()
        codeword_length = logstar(n_clauses+1) - math.lgamma(n_clauses)/np.log(2)
        for n_new, n_body in clause_nums:
            head_bits_cwd = self.head_bits[arity_head]
            n_new_vars_cwd = self.spec_num_new_vars(n_new)
            total, num_body_cwd = self.spec_num_body(self.head_arity, n_new, self.body_arities)
            spec_body_cwd = self.spec_body(total, n_body)
            codeword_length += (head_bits_cwd + n_new_vars_cwd + num_body_cwd + spec_body_cwd)
        return codeword_length
    def second_part(self, N,e,theta,n_ent,n_not_ent):
        if 1-theta <= 0.0:
            return -log_comb2(N,e) - e*np.log2(theta) + log_comb2(n_ent, e) + log_comb2(n_not_ent, N-e)
        elif theta <= 0.0:
            return -log_comb2(N,e) - (N-e)*np.log2(1-theta) + log_comb2(n_ent, e) + log_comb2(n_not_ent, N-e)
        else:
            return -log_comb2(N,e) - e*np.log2(theta) - (N-e)*np.log2(1-theta) + log_comb2(n_ent, e) + log_comb2(n_not_ent, N-e)
    def MML_total(self):
        fst = self.MML_first_part()
        snd1, snd2 = 0,0
        if self.pos_bin_obj != None:
            N_p, theta_pos, q_pos = self.pos_bin_obj.N, self.pos_bin_obj.get_theta(self.tp), self.pos_bin_obj.get_q(self.tp)
            snd1 = q_pos + self.second_part(N_p, self.tp, theta_pos, self.tp, N_p) #change last 2 arguments to calculations of n_entailed etc.
        if self.neg_bin_obj != None:
            N_n, theta_neg, q_neg = self.neg_bin_obj.N, self.neg_bin_obj.get_theta(self.tn), self.neg_bin_obj.get_q(self.tn)
            snd2 = q_neg + self.second_part(N_p, self.tn, theta_neg, self.tn, N_n) #change last 2 arguments to calculations of n_entailed etc.
        return fst + snd1 + snd2
    


class DurationSummary:
    def __init__(self, operation, called, total, mean, maximum):
        self.operation = operation
        self.called = called
        self.total = total
        self.mean = mean
        self.maximum = maximum

def flatten(xs):
    return [item for sublist in xs for item in sublist]

class Settings:
    def __init__(self, cmd_line=False, info=True, debug=False, show_stats=True, max_literals=MAX_LITERALS, timeout=TIMEOUT, quiet=False, eval_timeout=EVAL_TIMEOUT, max_examples=MAX_EXAMPLES, max_body=None, max_rules=None, max_vars=None, functional_test=False, kbpath=False, ex_file=False, bk_file=False, bias_file=False, showcons=False, no_bias=False, order_space=False, noisy=False, batch_size=BATCH_SIZE, solver='rc2', anytime_solver=None, anytime_timeout=ANYTIME_TIMEOUT):

        if cmd_line:
            args = parse_args()
            self.bk_file, self.ex_file, self.bias_file = load_kbpath(args.kbpath)
            quiet = args.quiet
            debug = args.debug
            show_stats = args.stats
            # bkcons = args.bkcons
            max_literals = args.max_literals
            timeout = args.timeout
            eval_timeout = args.eval_timeout
            max_examples = MAX_EXAMPLES
            max_body = args.max_body
            max_vars = args.max_vars
            max_rules = args.max_rules
            functional_test = args.functional_test
            # datalog = args.datalog
            showcons = args.showcons
            # no_bias = args.no_bias
            # order_space = args.order_space
            noisy = args.noisy
            batch_size = args.batch_size
            solver = args.solver
            anytime_solver = args.anytime_solver
            anytime_timeout = args.anytime_timeout
        else:
            if kbpath:
                self.bk_file, self.ex_file, self.bias_file = load_kbpath(kbpath)
            else:
                self.ex_file = ex_file
                self.bk_file = bk_file
                self.bias_file = bias_file

        self.logger = logging.getLogger("popper")

        if quiet:
            pass
        elif debug:
            log_level = logging.DEBUG
            # logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')
            logging.basicConfig(format='%(message)s', level=log_level, datefmt='%H:%M:%S')
        elif info:
            log_level = logging.INFO
            logging.basicConfig(format='%(asctime)s %(message)s', level=log_level, datefmt='%H:%M:%S')

        self.info = info
        self.debug = debug
        self.stats = Stats(info=info, debug=debug)
        self.stats.logger = self.logger
        self.show_stats = show_stats
        self.showcons = showcons
        self.max_literals = max_literals
        self.functional_test = functional_test
        self.timeout = timeout
        self.eval_timeout = eval_timeout
        self.max_examples = max_examples
        self.max_body = max_body
        self.max_vars = max_vars
        self.max_rules = max_rules
        self.no_bias = no_bias
        self.order_space = order_space
        self.noisy = noisy
        self.batch_size = batch_size
        self.solver = solver
        self.anytime_solver = anytime_solver
        self.anytime_timeout = anytime_timeout
        self.bkcons_timeout = BKCONS_TIMEOUT

        self.recall = {}
        self.solution = None
        self.best_prog_score = None

        solver = clingo.Control(['-Wnone'])
        with open(self.bias_file) as f:
            solver.add('bias', [], f.read())
        solver.add('bias', [], """
            #defined body_literal/4.
            #defined clause/1.
            #defined clause_var/2.
            #defined var_type/3.
            #defined body_size/2.
            #defined recursive/0.
            #defined var_in_literal/4.
        """)
        solver.ground([('bias', [])])

        # determine whether recursion enabled
        self.recursion_enabled = False
        for x in solver.symbolic_atoms.by_signature('enable_recursion', arity=0):
            self.recursion_enabled = True

        # determine whether pi enabled
        self.pi_enabled = False
        for x in solver.symbolic_atoms.by_signature('enable_pi', arity=0):
            self.pi_enabled = True

        # determine whether non_datalog flag is enabled
        self.non_datalog_flag = False
        for x in solver.symbolic_atoms.by_signature('non_datalog', arity=0):
            self.non_datalog_flag = True



        # read directions from bias file when there is no PI
        # if not self.pi_enabled:
        self.directions = directions = defaultdict(dict)
        self.has_directions = False
        for x in solver.symbolic_atoms.by_signature('direction', arity=2):
            self.has_directions = True
            pred = x.symbol.arguments[0].name
            for i, y in enumerate(x.symbol.arguments[1].arguments):
                y = y.name
                if y == 'in':
                    arg_dir = '+'
                elif y == 'out':
                    arg_dir = '-'
                directions[pred][i] = arg_dir


        self.max_arity = 0
        for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
            self.max_arity = max(self.max_arity, x.symbol.arguments[1].number)
            head_pred = x.symbol.arguments[0].name
            head_arity = x.symbol.arguments[1].number
            head_args = tuple(range(head_arity))
            self.head_literal = Literal(head_pred, head_args)

        if self.max_body is None:
            for x in solver.symbolic_atoms.by_signature('max_body', arity=1):
                self.max_body = x.symbol.arguments[0].number

        if self.max_body is None:
            self.max_body = MAX_BODY

        if self.max_vars is None:
            for x in solver.symbolic_atoms.by_signature('max_vars', arity=1):
                self.max_vars = x.symbol.arguments[0].number
        if self.max_vars is None:
            self.max_vars = MAX_VARS

        if self.max_rules is None:
            for x in solver.symbolic_atoms.by_signature('max_clauses', arity=1):
                self.max_rules = x.symbol.arguments[0].number
        if self.max_rules is None:
            if self.pi_enabled or self.recursion_enabled:
                self.max_rules = MAX_RULES
            else:
                self.max_rules = 1

        # find all body preds
        self.body_preds = set()
        for x in solver.symbolic_atoms.by_signature('body_pred', arity=2):
            pred = x.symbol.arguments[0].name
            arity = x.symbol.arguments[1].number
            self.body_preds.add((pred, arity))
            self.max_arity = max(self.max_arity, arity)

        # check that directions are all given
        if self.has_directions:
            for pred, arity in self.body_preds:
                if len(directions[pred]) != arity:
                    print(f'ERROR: missing directions for {pred}/{arity}')
                    exit()
                # self.body_modes[pred] = tuple(directions[pred][i] for i in range(arity))


        # TODO: EVENTUALLY

        # print(directions)

        self.cached_atom_args = {}
        for i in range(1, self.max_arity+1):
            for args in permutations(range(0, self.max_vars), i):
                k = tuple(clingo.Number(x) for x in args)
                self.cached_atom_args[k] = args

        self.cached_literals = {}
        self.literal_inputs = {}
        self.literal_outputs = {}

        if self.has_directions:
            head_pred, head_args = self.head_literal
            # print('head_args', head_args)
            for head_args in permutations(range(self.max_vars), len(head_args)):
                head_inputs = frozenset(arg for i, arg in enumerate(head_args) if directions[head_pred][i] == '+')
                head_outputs = frozenset(arg for i, arg in enumerate(head_args) if directions[head_pred][i] == '-')
                self.literal_inputs[(head_pred, head_args)] = head_inputs
                self.literal_outputs[(head_pred, head_args)] = head_outputs

        for pred, arity in self.body_preds:
            for k, args in self.cached_atom_args.items():
                if len(args) != arity:
                    continue
                literal = Literal(pred, args)
                self.cached_literals[(pred, k)] = literal
                if self.has_directions:
                    self.literal_inputs[(pred, args)] = frozenset(arg for i, arg in enumerate(args) if directions[pred][i] == '+')
                    self.literal_outputs[(pred, args)] = frozenset(arg for i, arg in enumerate(args) if directions[pred][i] == '-')

        # for k, vs in self.literal_inputs.items():
            # print(k, vs)
        # print('head_inputs', head_inputs)
        # print('head_outputs', head_outputs)
        # exit()

        pred = self.head_literal.predicate
        arity = len(self.head_literal.arguments)

        for k, args in self.cached_atom_args.items():
            if len(args) != arity:
                continue
            literal = Literal(pred, args)
            self.cached_literals[(pred, k)] = literal

        if self.max_rules == None:
            if self.recursion_enabled or self.pi_enabled:
                self.max_rules = max_rules
            else:
                self.max_rules = 1

        self.head_types, self.body_types = load_types(self)


        if len(self.body_types) > 0 or not self.head_types is None:
            if self.head_types is None:
                print('WARNING: MISSING HEAD TYPE')
                # exit()
            for p,a in self.body_preds:
                if p not in self.body_types:
                    print(f'WARNING: MISSING BODY TYPE FOR {p}')
                    # exit()



        self.single_solve = not (self.recursion_enabled or self.pi_enabled)

        self.logger.debug(f'Max rules: {self.max_rules}')
        self.logger.debug(f'Max vars: {self.max_vars}')
        self.logger.debug(f'Max body: {self.max_body}')

        self.single_solve = not (self.recursion_enabled or self.pi_enabled)

    def print_incomplete_solution2(self, prog, tp, fn, tn, fp, size):
        self.logger.info('*'*20)
        self.logger.info('New best hypothesis:')
        if self.noisy:
            self.logger.info(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size} mdl:{size+fn+fp}')
        else:
            self.logger.info(f'tp:{tp} fn:{fn} tn:{tn} fp:{fp} size:{size}')
        for rule in order_prog(prog):
            self.logger.info(format_rule(self.order_rule(rule)))
        self.logger.info('*'*20)

    def print_prog_score(self, prog, score):
        tp, fn, tn, fp, size = score
        precision = 'n/a'
        if (tp+fp) > 0:
            precision = f'{tp / (tp+fp):0.2f}'
        recall = 'n/a'
        if (tp+fn) > 0:
            recall = f'{tp / (tp+fn):0.2f}'
        print('*'*10 + ' SOLUTION ' + '*'*10)
        if self.noisy:
            print(f'Precision:{precision} Recall:{recall} TP:{tp} FN:{fn} TN:{tn} FP:{fp} Size:{size} MDL:{size+fn+fp}')
        else:
          print(f'Precision:{precision} Recall:{recall} TP:{tp} FN:{fn} TN:{tn} FP:{fp} Size:{size}')
        # print(self.format_prog(order_prog(prog)))
        for rule in order_prog(prog):
            print(format_rule(self.order_rule(rule)))
        # print(self.format_prog(order_prog(prog)))
        print('*'*30)

    def order_rule(self, rule):
        head, body = rule

        if self.datalog:
            return self.order_rule_datalog(head, frozenset(body))

        if not self.has_directions:
            return rule


        ordered_body = []
        grounded_variables = set()

        if head:
            head_pred, head_args = head
            head_inputs = self.literal_inputs[(head_pred, head_args)]
            if head_inputs == []:
                return rule
            grounded_variables.update(head_inputs)

        body_literals = set(body)


        while body_literals:
            selected_literal = None
            for literal in body_literals:
                pred, args = literal
                literal_outputs = self.literal_outputs[(pred, args)]

                if len(literal_outputs) == len(args):
                    selected_literal = literal
                    break

                literal_inputs = self.literal_inputs[(pred, args)]
                if not literal_inputs.issubset(grounded_variables):
                    continue

                if head and pred != head.predicate:
                    # find the first ground non-recursive body literal and stop
                    selected_literal = literal
                    break
                elif selected_literal == None:
                    # otherwise use the recursive body literal
                    selected_literal = literal

            if selected_literal == None:
                message = f'{selected_literal} in clause {format_rule(rule)} could not be grounded'
                raise ValueError(message)

            ordered_body.append(selected_literal)
            pred, args = selected_literal
            selected_literal_outputs = self.literal_outputs[(pred, args)]
            grounded_variables = grounded_variables.union(selected_literal_outputs)
            body_literals = body_literals.difference({selected_literal})

        return head, tuple(ordered_body)

    @cache
    def order_rule_datalog(self, head, body):
        def tmp_score(seen_vars, literal):
            pred, args = literal
            key = []
            for x in args:
                if x in seen_vars:
                    key.append('1')
                else:
                    key.append('0')
            key = ''.join(key)
            k = (pred, key)
            if k in self.recall:
                return self.recall[k]
            return 1000000



        # head, body = rule
        ordered_body = []
        seen_vars = set()

        if head:
            seen_vars.update(head.arguments)
        body_literals = set(body)
        while body_literals:
            selected_literal = None
            for literal in body_literals:
                if set(literal.arguments).issubset(seen_vars):
                    selected_literal = literal
                    break

            if selected_literal == None:
                xs = sorted(body_literals, key=lambda x: tmp_score(seen_vars, x))
                selected_literal = xs[0]

            ordered_body.append(selected_literal)
            seen_vars = seen_vars.union(selected_literal.arguments)
            body_literals = body_literals.difference({selected_literal})

        return head, tuple(ordered_body)

def non_empty_powerset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)+1))

def non_empty_subset(iterable):
    s = tuple(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(1, len(s)))

def load_types(settings):
    enc = """
#defined clause/1.
#defined clause_var/2.
#defined var_type/3."""
    # solver = clingo.Control()
    solver = clingo.Control(['-Wnone'])
    with open(settings.bias_file) as f:
        solver.add('bias', [], f.read())
    solver.add('bias', [], enc)
    solver.ground([('bias', [])])

    for x in solver.symbolic_atoms.by_signature('head_pred', arity=2):
        head_pred = x.symbol.arguments[0].name
        head_arity = x.symbol.arguments[1].number

    head_types = None
    body_types = {}
    for x in solver.symbolic_atoms.by_signature('type', arity=2):
        pred = x.symbol.arguments[0].name
        # xs = (str(t) for t in )
        xs = [y.name for y in x.symbol.arguments[1].arguments]
        if pred == head_pred:
            head_types = xs
        else:
            body_types[pred] = xs

    return head_types, body_types

def bias_order(settings, max_size):

    if not (settings.no_bias or settings.order_space):
        return [(size_literals, settings.max_vars, settings.max_rules, None) for size_literals in range(1, max_size+1)]

    # if settings.search_order is None:
    ret = []
    predicates = len(settings.body_preds) + 1
    arity = settings.max_arity
    min_rules = settings.max_rules
    if settings.no_bias:
        min_rules = 1
    for size_rules in range(min_rules, settings.max_rules+1):
        max_size = (1 + settings.max_body) * size_rules
        for size_literals in range(1, max_size+1):
            # print(size_literals)
            minimum_vars = settings.max_vars
            if settings.no_bias:
                minimum_vars = 1
            for size_vars in range(minimum_vars, settings.max_vars+1):
                # FG We should not search for configurations with more variables than the possible variables for the number of litereals considered
                # There must be at least one variable repeated, otherwise all the literals are disconnected
                max_possible_vars = (size_literals * arity) - 1
                # print(f'size_literals:{size_literals} size_vars:{size_vars} size_rules:{size_rules} max_possible_vars:{max_possible_vars}')
                if size_vars > max_possible_vars:
                    break

                hspace = comb(predicates * pow(size_vars, arity), size_literals)

                # AC @ FG: handy code to skip pointless unsat calls
                if hspace == 0:
                    continue
                if size_rules > 1 and size_literals < 5:
                    continue
                ret.append((size_literals, size_vars, size_rules, hspace))

    if settings.order_space:
        ret.sort(key=lambda tup: (tup[3],tup[0]))

    #for x in ret:
    #    print(x)

    settings.search_order = ret
    return settings.search_order

def is_headless(prog):
    return any(head is None for head, body in prog)

@cache
def head_connected(rule):
    head, body = rule
    _head_pred, head_args = head
    head_connected_vars = set(head_args)
    body_literals = set(body)

    if not any(x in head_connected_vars for _pred, args in body for x in args):
        return False

    while body_literals:
        changed = False
        for literal in body_literals:
            pred, args = literal
            if any (x in head_connected_vars for x in args):
                head_connected_vars.update(args)
                body_literals = body_literals.difference({literal})
                changed = True
        if changed == False and body_literals:
            return False

    return True

import os
# AC: I do not know what this code below really does, but it works
class suppress_stdout_stderr(object):
    '''
    A context manager for doing a "deep suppression" of stdout and stderr in
    Python, i.e. will suppress all print, even if the print originates in a
    compiled C/Fortran sub-function.
       This will not suppress raised exceptions, since exceptions are printed
    to stderr just before a script exits, and after the context manager has
    exited (at least, I think that is why it lets exceptions through).

    '''
    def __init__(self):
        # Open a pair of null files
        self.null_fds =  [os.open(os.devnull,os.O_RDWR) for x in range(2)]
        # Save the actual stdout (1) and stderr (2) file descriptors.
        self.save_fds = [os.dup(1), os.dup(2)]

    def __enter__(self):
        # Assign the null pointers to stdout and stderr.
        os.dup2(self.null_fds[0],1)
        os.dup2(self.null_fds[1],2)

    def __exit__(self, *_):
        # Re-assign the real stdout/stderr back to (1) and (2)
        os.dup2(self.save_fds[0],1)
        os.dup2(self.save_fds[1],2)
        # Close all file descriptors
        for fd in self.null_fds + self.save_fds:
            os.close(fd)

def rename_variables(rule):
    head, body = rule
    if head:
        head_vars = set(head.arguments)
    else:
        head_vars = set()
    next_var = len(head_vars)
    new_body = []
    lookup = {}
    for pred, args in sorted(body, key=lambda x: x.predicate):
        new_args = []
        for var in args:
            if var in head_vars:
                new_args.append(var)
                continue
            elif var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_body.append((pred, tuple(new_args)))
    return (head, new_body)

def get_raw_prog(prog):
    xs = set()
    for rule in prog:
        h, b = rename_variables(rule)
        xs.add((h, frozenset(b)))
    return frozenset(xs)

def prog_hash(prog):
    new_prog = get_raw_prog(prog)
    return hash(new_prog)

def remap_variables(rule):
    head, body = rule
    head_vars = frozenset()

    if head:
        head_vars = frozenset(head.arguments)

    next_var = len(head_vars)
    lookup = {i:i for i in head_vars}

    new_body = []
    for pred, args in body:
        new_args = []
        for var in args:
            if var not in lookup:
                lookup[var] = next_var
                next_var+=1
            new_args.append(lookup[var])
        new_atom = Literal(pred, tuple(new_args))
        new_body.append(new_atom)

    return head, frozenset(new_body)

def format_prog(prog):
    return '\n'.join(format_rule(rule) for rule in prog)