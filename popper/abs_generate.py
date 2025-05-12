import abc


class Generator(abc.ABC):

    # @profile
    def get_prog(self):
        pass

    def gen_symbol(self, literal, backend):
        pass

    def update_solver(self, size):
        pass
        self.update_number_of_literals(size)

    def update_number_of_literals(self, size):
        pass

    def update_number_of_vars(self, size):
        pass

    def update_number_of_rules(self, size):
        pass

    def prune_size(self, size):
        pass

    # @profile
    def get_ground_rules(self, rule):
        pass

    def parse_handles(self, new_handles):
        pass

    def constrain(self, tmp_new_cons):
        pass