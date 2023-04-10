import time
from collections import deque
from . util import format_rule, order_rule, format_prog, prog_size
from . explain import prog_hash
from . explain import get_raw_prog as get_raw_prog2


def print_raw_prog(raw_prog):
    print(frawprog(raw_prog))

def frawprog(raw_prog):
    _, b = list(raw_prog)[0]
    b = tuple(sorted(b))
    return b

class Hasse:

    def print(self, depth=0):
        for child in self.children:
            print('\t'*depth, frawprog(child.raw_prog), child.pos_covered)
            # print('\t'*depth, frawprog(child.raw_prog))
            child.print(depth+1)

    def __init__(self):
        self.children = set()
        self.descendents = set()
        self.savings = 0

        self.literal_mapping = {}

    def add(self, prog, pos_covered):
        self.add_(prog, pos_covered)
        # print('*'*50)
        # self.print()

    # add the node/program to the lattice
    # we want to add it below any programs that generalise it
    # @profile
    def add_(self, prog, pos_covered):
        added_to_child = False
        moo_count = 0

        node = Node(prog, pos_covered)
        to_remove = set()
        for child in self.children:
            if child.pos_covered.issubset(pos_covered) and child.pos_covered  != pos_covered:
                moo_count +=1
                to_remove.add(child)
                node.children.add(child)
                # if moo_count > 1:
                    # assert(False)
                # print('MOO1')
                # print('child',format_prog(child.prog))
                # print('\t',child.pos_covered)
                # print('new:',format_prog(prog))
                # print('\t',pos_covered)
                # node = Node(prog, pos_covered)
                # # remove child from child's parent
                # child.parent.children.remove(node)
                # #
                # child.parent = node
                # node.children.add

            if pos_covered.issubset(child.pos_covered):
                if moo_count > 0:
                    assert(False)
                added_to_child = True
                if pos_covered == child.pos_covered:
                    child.progs.add(prog)
                else:
                    child.add_child(prog, pos_covered)
        for x in to_remove:
            self.children.remove(x)
        if not added_to_child:
            # node = Node(prog, pos_covered)
            self.children.add(node)

    def seen_prog(self, prog):
        return False

    # def prune_specialisations2(self, node):
    #     to_explore = deque()
    #     to_explore.extend(self.children)

    #     to_delete = set()

    #     while len(to_explore) > 0:
    #         child = to_explore.popleft()

    #         if child in to_delete:
    #             continue

    #         if child.pos_covered.issubset(node.pos_covered):
    #             self.remove_node(child)
    #             to_delete.add(child)
    #             continue

    #         to_explore.extend(child.children)

    #     out = set()
    #     for x in to_delete:
    #         out.add(x.raw_prog)
    #     return out

    def remove_node(self, node):
        # print('delete:', id(node), frawprog(node.raw_prog))
        # print('\t parents:', [id(x) for x in node.parents])
        # print('\t children:', [id(x) for x in node.children])

        if node not in self.descendents:
            # print('shitmypants', id(node), format_prog(node.prog))
            return False

        # remove children
        tmp_xs = set(x for x in node.children)
        for child in tmp_xs:
            self.remove_node(child)


        # special case for root nodes
        if len(node.parents) == 0:
            self.children.remove(node)
        else:
            # remove node from parents
            for parent in node.parents:
                if node in parent.children:
                    parent.children.remove(node)
                parent.descendents.remove(node)
                for x in node.descendents:
                    parent.descendents.remove(x)

        self.descendents.remove(node)
        for x in node.descendents:
            self.descendents.remove(node)

class Node:
    def __init__(self, prog, pos_covered):
        self.prog = prog
        self.pos_covered = pos_covered
        self.raw_prog = get_raw_prog(prog)
        self.descendents = set()
        self.parent = None
        self.children = set()
        # self.literal_mapping = {}
        # self.prog_size = prog_size(prog)
        # self.num_pos_covered = len(pos_covered)
        self.progs = set([prog])



    # def __hash__(self):
    #     return self.hash_value

    # def __eq__(self, other):
    #     self.hash_value == other.hash_value

    def print(self, depth=0):
        for child in self.children:
            print('\t'*depth, frawprog(child.raw_prog), child.pos_covered)
            child.print(depth+1)

    def remove_node(self, node):
        if node not in self.descendents:
            # print('shitmypants', id(node), format_prog(node.prog))
            return False

        # remove children
        for child in node.children:
            self.remove_node(child)

        # special case for root nodes
        if len(node.parents) == 0:
            assert(False)
            self.children.remove(node)
        else:
            # remove node from parents
            for parent in node.parents:
                parent.children.remove(node)
                parents.descendents.remove(node)
                for x in node.descendents:
                    parent.descendents.remove(x)

        self.descendents.remove(node)
        for x in node.descendents:
            self.descendents.remove(node)

    def add_parent(self, node):
        self.parents.add(node)

    # @profile
    def add_child(self, prog, pos_covered):
        added_to_child = False

        node = Node(prog, pos_covered)

        moo_count = 0
        to_remove = set()
        for child in self.children:
            if child.pos_covered.issubset(pos_covered) and child.pos_covered  != pos_covered:
                moo_count += 1
                # print('MOO2')
                # print('child',format_prog(child.prog))
                # print('new:',format_prog(prog))
                to_remove.add(child)
                node.children.add(child)
            if pos_covered.issubset(child.pos_covered):
                if moo_count > 0:
                    assert(False)
                added_to_child = True
                if pos_covered == child.pos_covered:
                    child.progs.add(prog)
                else:
                    child.add_child(prog, pos_covered)
        for x in to_remove:
            self.children.remove(x)
        if not added_to_child:
            # node = Node(prog, pos_covered)
            self.children.add(node)


    # # @profile
    # def add_child(self, node):
    #     print('loop',frawprog(node.raw_prog), node.pos_covered)
    #     if node in self.descendents:
    #         return False
    #     self.descendents.add(node)
    #     added_to_child = False

    #     for child in self.children:
    #         if child == node:
    #             print('moo2')
    #             continue
    #         if node.pos_covered == child.pos_covered:
    #             node.progs.add(node.prog)
    #             added_to_child = True
    #         elif node.pos_covered.issubset(child.pos_covered):
    #             node.add_parent(child)
    #             child.add_child(node)
    #             added_to_child = True

    #     if not added_to_child:
    #         self.children.add(node)

# def rule_subsumes(r1, r2):
#     # r1 subsumes r2 if r1 is a subset of r2
#     h1, b1 = r1
#     h2, b2 = r2
#     if h1 != None and h2 == None:
#         return False
#     return b1.issubset(b2)

# def theory_subsumes(prog1, prog2):
#     return all(any(rule_subsumes(r1, r2) for r1 in prog1) for r2 in prog2)

def literal_to_tuple(literal):
    return literal.predicate, literal.arguments

def get_raw_prog(prog):
    xs = set()
    for h ,b in prog:
        h = literal_to_tuple(h)
        b = frozenset(literal_to_tuple(x) for x in b)
        xs.add((h, b))
    return frozenset(xs)

# def generalises(p1, p2):
#     return theory_subsumes(p1.raw_prog, p2.raw_prog)

# def generalises2(p1, p2):
#     h1, b1 = list(p1.raw_prog)[0]
#     h2, b2 = list(p2.raw_prog)[0]
#     return b1.issubset(b2)

# def generalises3(p1, p2):
#     h1, b1 = list(p1.raw_prog)[0]
#     h2, b2 = list(p2.raw_prog)[0]

#     for x in b1:
#         if x not in b2:
#             return False
#     return True
