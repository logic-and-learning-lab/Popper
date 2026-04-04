from collections import defaultdict
import time
from . util import print_incomplete_solution2, mdl_score

class SearchState:
    # pos_covered_bit_array -> prog_size+prog_size2
    # it only maintains success sets for pairs of programs where fp = 0
    paired_success_sets = defaultdict(set)

    # pos_covered_bit_array -> prog_size
    # it only maintains success sets for programs where fp = 0
    success_sets = {}

    # (pos_covered_bit_array, neg_covered_bitarray) -> prog_size
    success_sets_noise = {}

    search_depth = None

    # save hypotheses for which we pruned spec / gen from a certain size only, once we update the best mdl score, we can prune spec / gen from a better size for some of these
    seen_hyp_spec, seen_hyp_gen = defaultdict(list), defaultdict(list)

    best_hypothesis = None
    best_hypothesis_size = None
    best_hypothesis_score = None
    best_hypothesis_mdl = None
    solution_found = False

    min_pos_coverage = 1

    min_size = None

    max_literals=1000

    start_time = None

    def start_time(self):
        self.start_time = time.time()

    def time_remaining(self, timeout):
        time_now = time.time()
        time_spent = time_now - self.start_time
        return max(int(timeout-time_spent), 1)

def update_best_hypothesis(settings, state, hypothesis, hypothesis_size, conf_matrix):
    if hypothesis != state.best_hypothesis and conf_matrix != state.best_hypothesis_score:
        print_incomplete_solution2(hypothesis, hypothesis_size, conf_matrix)
    state.best_hypothesis_score = conf_matrix
    state.best_hypothesis_size = hypothesis_size
    state.best_hypothesis = hypothesis
    _, fn, _, fp = conf_matrix

    if settings.noisy:
        mdl = mdl_score(fn, fp, hypothesis_size)
        state.best_hypothesis_mdl = mdl
        state.max_literals = mdl - 1
    elif fp == 0 and fn == 0:
        state.solution_found = True
        state.max_literals = hypothesis_size - 1
        # if we use joiner, then we do not learn rules in increasing size order, so skip min coverage pruning
        if not settings.joiner:
            state.min_pos_coverage = 2