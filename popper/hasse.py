# import time
# from collections import deque
# from . util import format_rule, order_rule, format_prog
# from . explain import prog_hash
# from . explain import get_raw_prog as get_raw_prog2


# def print_raw_prog(raw_prog):
#     print(frawprog(raw_prog))

# def frawprog(raw_prog):
#     _, b = list(raw_prog)[0]
#     b = tuple(sorted(b))
#     return b

# class Hasse:

#     def print(self, depth=0):
#         for child in self.children:
#             print('\t\t'*depth, id(child), frawprog(child.raw_prog), child.num_pos_covered)
#             child.print(depth+1)

#     def __init__(self):
#         self.children = set()
#         self.descendents = set()
#         self.savings = 0

#         self.literal_mapping = {}

#     def add(self, prog, pos_covered):
#         node = Node(prog, pos_covered)
#         self.add_(node)


#     # add the node/program to the lattice
#     # we want to add it below any programs that generalise it
#     # @profile
#     def add_(self, node):
#         self.descendents.add(node)
#         added_to_child = False

#         t1 = time.time()
#         _, b = list(node.raw_prog)[0]

#         b = list(b)
#         if b[0] in self.literal_mapping:
#             maybes = self.literal_mapping[b[0]]
#             for x in b[1:]:
#                 if x in self.literal_mapping:
#                     maybes = maybes.union(self.literal_mapping[x])
#         else:
#             maybes = set()

#         maybes = maybes.intersection(self.children)
#         for child in maybes:
#             if child.num_pos_covered >= node.num_pos_covered:
#                 if generalises3(child, node):
#                     node.add_parent(child)
#                     child.add_child(node)
#                     added_to_child = True

#         if not added_to_child:
#             self.children.add(node)
#             _, b = list(node.raw_prog)[0]
#             for x in b:
#                 if x not in self.literal_mapping:
#                     self.literal_mapping[x] = set([node])
#                 else:
#                     self.literal_mapping[x].add(node)

#     def seen_prog(self, prog):
#         return False
#         # raw_prog = get_raw_prog(prog)
#         # if raw_prog not in self.raw_prog_to_node:
#         #     return False
#         # node = self.raw_prog_to_node[raw_prog]
#         # return node in self.descendents

#     def prune_specialisations2(self, prog, pos_covered):
#         node = Node(prog, pos_covered)


#         # print('*'*50)
#         # print(self.print())
#         # print('*'*50)

#         # print('prune_specialisations2', frawprog(node.raw_prog))

#         to_explore = deque()
#         to_explore.extend(self.children)

#         to_delete = set()
#         # to_delete2 = []


#         while len(to_explore) > 0:
#             child = to_explore.popleft()

#             if child in to_delete:
#                 continue

#             if node.num_pos_covered >= child.num_pos_covered and generalises(node, child):
#                 # print('\t\prune node:', id(child), frawprog(child.raw_prog))
#                 # print('\t\t\t child:', id(child))
#                 # print('\t\t\t parents:', [id(x) for x in child.parents])
#                 # print('\t\t\t children:', [id(x) for x in child.children])
#                 self.remove_node(child)
#                 to_delete.add(child)
#                 # to_delete2.append(child)
#                 continue

#             to_explore.extend(child.children)

#         # print(len(to_delete), len(to_delete2))
#         out = set()
#         for x in to_delete:
#             out.add(x.raw_prog)
#             # self.remove_node(x)
#         return out


#     def remove_node(self, node):
#         # print('delete:', id(node), frawprog(node.raw_prog))
#         # print('\t parents:', [id(x) for x in node.parents])
#         # print('\t children:', [id(x) for x in node.children])

#         if node not in self.descendents:
#             # print('shitmypants', id(node), format_prog(node.prog))
#             return False

#         # remove children
#         tmp_xs = set(x for x in node.children)
#         for child in tmp_xs:
#             self.remove_node(child)


#         # special case for root nodes
#         if len(node.parents) == 0:
#             self.children.remove(node)
#         else:
#             # remove node from parents
#             for parent in node.parents:
#                 if node in parent.children:
#                     parent.children.remove(node)
#                 parent.descendents.remove(node)
#                 for x in node.descendents:
#                     parent.descendents.remove(x)

#         self.descendents.remove(node)
#         for x in node.descendents:
#             self.descendents.remove(node)


# class Node:
#     def __init__(self, prog, pos_covered):
#         self.prog = prog
#         self.pos_covered = pos_covered
#         self.num_pos_covered = len(pos_covered)
#         self.raw_prog = get_raw_prog(prog)
#         self.descendents = set()
#         self.parents = set()
#         self.children = set()
#         self.literal_mapping = {}


#     # def __hash__(self):
#     #     return self.hash_value

#     # def __eq__(self, other):
#     #     self.hash_value == other.hash_value

#     def print(self, depth=0):
#         for child in self.children:
#             # h1, b1 = list(child.raw_prog)[0]
#             # h2, b2 = list(get_raw_prog2(child.prog))[0]
#             # b1 = tuple(sorted(b1))
#             # b2 = tuple(sorted(b2))
#             # # if b1 != b2:
#             #     # print('mooo')
#             print('\t\t'*depth, id(child), frawprog(child.raw_prog), child.num_pos_covered)
#             # print('\t\t'*depth, 'y', tuple(sorted(b2)))

#     def remove_node(self, node):
#         if node not in self.descendents:
#             # print('shitmypants', id(node), format_prog(node.prog))
#             return False

#         # remove children
#         for child in node.children:
#             self.remove_node(child)

#         # special case for root nodes
#         if len(node.parents) == 0:
#             assert(False)
#             self.children.remove(node)
#         else:
#             # remove node from parents
#             for parent in node.parents:
#                 parent.children.remove(node)
#                 parents.descendents.remove(node)
#                 for x in node.descendents:
#                     parent.descendents.remove(x)

#         self.descendents.remove(node)
#         for x in node.descendents:
#             self.descendents.remove(node)

#     def add_parent(self, node):
#         self.parents.add(node)

#     # @profile
#     def add_child(self, node):
#         if node in self.descendents:
#             return False
#         self.descendents.add(node)
#         added_to_child = False

#         _, b = list(node.raw_prog)[0]

#         b = list(b)
#         if b[0] in self.literal_mapping:
#             maybes = self.literal_mapping[b[0]]
#             for x in b[1:]:
#                 if x in self.literal_mapping:
#                     maybes = maybes.union(self.literal_mapping[x])
#         else:
#             maybes = set()

#         maybes = maybes.intersection(self.children)

#         for child in maybes:
#             if child.num_pos_covered >= node.num_pos_covered:
#                 if generalises3(child, node):
#                     node.add_parent(child)
#                     child.add_child(node)
#                     added_to_child = True

#         if not added_to_child:
#             self.children.add(node)
#             _, b = list(node.raw_prog)[0]
#             for x in b:
#                 if x not in self.literal_mapping:
#                     self.literal_mapping[x] = set([node])
#                 else:
#                     self.literal_mapping[x].add(node)

# def rule_subsumes(r1, r2):
#     # r1 subsumes r2 if r1 is a subset of r2
#     h1, b1 = r1
#     h2, b2 = r2
#     if h1 != None and h2 == None:
#         return False
#     return b1.issubset(b2)

# def theory_subsumes(prog1, prog2):
#     return all(any(rule_subsumes(r1, r2) for r1 in prog1) for r2 in prog2)

# def literal_to_tuple(literal):
#     return literal.predicate, literal.arguments

# def get_raw_prog(prog):
#     xs = set()
#     for h ,b in prog:
#         h = literal_to_tuple(h)
#         b = frozenset(literal_to_tuple(x) for x in b)
#         xs.add((h, b))
#     return frozenset(xs)

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
