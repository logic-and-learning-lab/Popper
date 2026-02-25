from collections import defaultdict

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