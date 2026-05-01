from collections import defaultdict
import time
from . util import print_incomplete_solution, mdl_score

class SearchState:
    def __init__(self):
        # pos_covered_bit_array -> prog_size
        # only save success sets for programs where fp = 0
        self.success_sets = {}
        self.success_sets_version = 0

        # (pos_covered_bit_array, neg_covered_bitarray) pairs seen for noisy programs
        self.success_sets_noise = set()

        self.search_depth = None

        # save hypotheses for which we pruned spec / gen from a certain size only,
        # once we update the best mdl score, we can prune spec / gen from a better
        # size for some of these
        self.seen_hyp_spec = defaultdict(list)
        self.seen_hyp_gen = defaultdict(list)

        self.best_hypothesis = None
        self.best_hypothesis_size = None
        self.best_hypothesis_score = None
        self.best_hypothesis_mdl = None
        self.solution_found = False

        self.min_pos_coverage = 1
        self.min_size = None
        self.max_literals = 1000
        self._start_time = None

    def start_time(self):
        self._start_time = time.time()

    def time_remaining(self, timeout):
        time_now = time.time()
        time_spent = time_now - self._start_time
        return max(int(timeout-time_spent), 1)

def initialise_noisy_best_hypothesis(state, num_pos, num_neg):
    state.best_hypothesis_score = (0, num_pos, num_neg, 0)
    state.best_hypothesis_mdl = num_pos

def _update_search_bounds(settings, state, hypothesis_size, conf_matrix, mdl):
    _, fn, _, fp = conf_matrix

    if settings.noisy:
        state.max_literals = mdl - 1
        return

    if fp != 0 or fn != 0:
        return

    state.solution_found = True
    state.max_literals = hypothesis_size - 1
    # if we use joiner, then we do not learn rules in increasing size order, so skip min coverage pruning
    if not settings.joiner:
        state.min_pos_coverage = 2

def _is_better_hypothesis(settings, state, hypothesis_size, conf_matrix, mdl):
    if state.best_hypothesis_score is None:
        return True

    tp, _fn, _tn, fp = conf_matrix
    best_tp, best_fn, _best_tn, best_fp = state.best_hypothesis_score

    if settings.noisy:
        is_perfect = conf_matrix[1] == 0 and fp == 0
        current_is_perfect = best_fn == 0 and best_fp == 0

        # A perfect noisy hypothesis is always better than any imperfect one,
        # even if the initial seeded MDL bound is tighter than its size.
        if is_perfect and not current_is_perfect:
            return True

        if state.best_hypothesis_mdl is None:
            return True

        return mdl < state.best_hypothesis_mdl

    if tp != best_tp:
        return tp > best_tp

    if fp != best_fp:
        return fp < best_fp

    if state.best_hypothesis_size is None:
        return True

    return hypothesis_size < state.best_hypothesis_size

def update_best_hypothesis(settings, state, hypothesis, hypothesis_size, conf_matrix):
    _, fn, _, fp = conf_matrix

    mdl = None
    if settings.noisy:
        mdl = mdl_score(fn, fp, hypothesis_size)

    if not _is_better_hypothesis(settings, state, hypothesis_size, conf_matrix, mdl):
        return

    print_incomplete_solution(hypothesis, hypothesis_size, conf_matrix, settings, settings.noisy)
    state.best_hypothesis_score = conf_matrix
    state.best_hypothesis_size = hypothesis_size
    state.best_hypothesis = hypothesis

    if settings.noisy:
        state.best_hypothesis_mdl = mdl

    _update_search_bounds(settings, state, hypothesis_size, conf_matrix, mdl)
