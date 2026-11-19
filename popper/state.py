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
        # self.seen_hyp_spec = defaultdict(list)
        # self.seen_hyp_gen = defaultdict(list)

        self.best_hypothesis = None
        self.best_hypothesis_size = None
        self.best_hypothesis_score = None
        self.best_hypothesis_mdl = None
        self.solution_found = False

        self.min_pos_coverage = 1
        self.min_size = None
        self.max_literals = 1000
        self._start_time = time.time()

        # absolute time (as time.time()) by which Popper must finish; None means no deadline
        self.deadline = None
        # set when the search loop stops because it reached the timeout
        self.search_timed_out = False

    def time_left(self):
        if self.deadline is None:
            return float('inf')
        return self.deadline - time.time()

def solver_budget(settings, state, last_combine_stage):
    """Time limit in seconds for one solver call in the combine stage, or None for no limit.

    The final combine may use all the time left before the deadline. Every other combine is
    also capped at settings.anytime_timeout. A budget <= 0 means there is no time left, so
    the caller should skip the solve and keep the current best hypothesis.
    """
    left = state.time_left()
    if not last_combine_stage:
        left = min(left, settings.anytime_timeout)
    if left == float('inf'):
        return None
    return left

def out_of_time(budget):
    return budget is not None and budget <= 0

FINAL_COMBINE_TIMEOUT_MSG = 'FINAL COMBINE STOPPED AT THE TIMEOUT: HYPOTHESIS MAY NOT BE OPTIMAL'
ALL_OPT_TIMEOUT_MSG = 'ALL-OPT ENUMERATION STOPPED AT THE TIMEOUT: THE LIST MAY BE INCOMPLETE'
ALL_OPT_SKIPPED_MSG = 'SKIPPING ALL-OPT ENUMERATION: THE SEARCH TIMED OUT, SO OPTIMALITY IS NOT PROVEN'

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

        if state.best_hypothesis_mdl is None:
            return True

        # MDL is primary; prefer a perfect hypothesis only on equal cost.
        if mdl != state.best_hypothesis_mdl:
            return mdl < state.best_hypothesis_mdl
        return is_perfect and not current_is_perfect

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
