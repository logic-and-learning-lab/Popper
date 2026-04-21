from collections import defaultdict
import time
from . util import print_incomplete_solution2, mdl_score

class SearchState:
    def __init__(self):
        # pos_covered_bit_array -> prog_size
        # only save success sets for programs where fp = 0
        self.success_sets = {}
        self.success_sets_version = 0

        # (pos_covered_bit_array, neg_covered_bitarray) -> prog_size
        self.success_sets_noise = {}

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

def update_best_hypothesis(settings, state, hypothesis, hypothesis_size, conf_matrix):
    _, fn, _, fp = conf_matrix

    if settings.noisy:
        mdl = mdl_score(fn, fp, hypothesis_size)
        if state.best_hypothesis_mdl is not None and mdl >= state.best_hypothesis_mdl:
            return

    if hypothesis != state.best_hypothesis and conf_matrix != state.best_hypothesis_score:
        print_incomplete_solution2(hypothesis, hypothesis_size, conf_matrix)
    state.best_hypothesis_score = conf_matrix
    state.best_hypothesis_size = hypothesis_size
    state.best_hypothesis = hypothesis

    if settings.noisy:
        state.best_hypothesis_mdl = mdl
        state.max_literals = mdl - 1
    elif fp == 0 and fn == 0:
        state.solution_found = True
        state.max_literals = hypothesis_size - 1
        # if we use joiner, then we do not learn rules in increasing size order, so skip min coverage pruning
        if not settings.joiner:
            state.min_pos_coverage = 2
