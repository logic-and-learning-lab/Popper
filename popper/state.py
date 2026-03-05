
from collections import defaultdict
import time

class SearchState:
    # def __init__(self):
    # pos_covered_bit_array -> prog_size+prog_size2
    # it only maintains success sets for pairs of programs where fp = 0
    paired_success_sets = defaultdict(set)

    # pos_covered_bit_array -> prog_size
    # it only maintains success sets for programs where fp = 0
    success_sets = {}
    success_sets_aux = {}

    # (pos_covered_bit_array, neg_covered_bitarray) -> prog_size
    success_sets_noise = {}

    last_size = None

    # save hypotheses for which we pruned spec / gen from a certain size only, once we update the best mdl score, we can prune spec / gen from a better size for some of these
    seen_hyp_spec, seen_hyp_gen = defaultdict(list), defaultdict(list)


    best_hypothesis = None
    best_hypothesis_size = None
    best_hypothesis_score = None
    solution_found = False


    min_pos_coverage = 1


    start_time = None

    def start_time(self):
        self.start_time = time.time()

    def time_remaining(self, timeout):
        time_now = time.time()
        time_spent = time_now - self.start_time
        return max(int(timeout-time_spent), 1)
