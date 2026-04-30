# cython: language_level=3

from itertools import permutations
from ._gen_norec_clingo_capi cimport NativeNoRecControl


cpdef tuple body_to_literal_ids(object body, object literal_to_id):
    cdef list out = []
    cdef object lit

    for lit in body:
        out.append(literal_to_id[lit])
    return tuple(out)


cpdef tuple prog_to_id_rule(object prog, object literal_to_id):
    cdef object rule = next(iter(prog))
    return rule[0], body_to_literal_ids(rule[1], literal_to_id)


cpdef tuple remap_body_literal_ids(
    object body_ids,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
):
    cdef dict lookup = {}
    cdef list remapped = []
    cdef list new_args
    cdef int next_var = 0
    cdef int lit_id
    cdef int var

    for lit_id in body_ids:
        new_args = []
        for var in literal_id_to_args[lit_id]:
            if var not in lookup:
                lookup[var] = next_var
                next_var += 1
            new_args.append(lookup[var])
        remapped.append(literal_id_by_pred_args[(literal_id_to_pred[lit_id], tuple(new_args))])
    return tuple(remapped)


def find_variants_ids(
    object head_args,
    object body_ids,
    int max_vars,
    bint max_rule_vars,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
    object body_literal_id_to_clingo,
):
    cdef int head_arity = len(head_args)
    cdef set body_vars = set()
    cdef object lit_id
    cdef object args
    cdef int var
    cdef object var_range
    cdef tuple perm
    cdef object pred
    cdef object plan
    cdef object item
    cdef tuple new_args
    cdef object literal_id
    cdef object clingo_literal
    cdef object literal_id_get = literal_id_by_pred_args.get
    cdef list body_list = []
    cdef list new_body
    cdef list tmp
    cdef list tmp_bufs
    cdef dict body_var_to_perm_index
    cdef Py_ssize_t body_literal_count = len(body_literal_id_to_clingo)
    cdef Py_ssize_t literal_index
    cdef int arg_pos
    cdef int j

    for lit_id in body_ids:
        args = literal_id_to_args[lit_id]
        pred = literal_id_to_pred[lit_id]
        body_list.append((pred, args))
        for var in args:
            if var >= head_arity:
                body_vars.add(var)

    if max_rule_vars:
        var_range = range(head_arity, head_arity + len(body_vars))
    else:
        var_range = range(head_arity, max_vars)

    body_var_to_perm_index = {var: i for i, var in enumerate(sorted(body_vars))}
    body_list = []
    tmp_bufs = []
    for lit_id in body_ids:
        args = literal_id_to_args[lit_id]
        pred = literal_id_to_pred[lit_id]
        tmp = [None] * len(args)
        plan = []
        for arg_pos, var in enumerate(args):
            if var < head_arity:
                tmp[arg_pos] = head_args[var]
            else:
                plan.append((arg_pos, body_var_to_perm_index[var]))
        body_list.append((pred, plan))
        tmp_bufs.append(tmp)

    for perm in permutations(var_range, len(body_vars)):
        new_body = []
        j = 0
        for pred, plan in body_list:
            tmp = tmp_bufs[j]
            j += 1
            for item in plan:
                tmp[item[0]] = perm[item[1]]
            new_args = tuple(tmp)
            literal_id = literal_id_get((pred, new_args))
            if literal_id is None:
                break
            literal_index = <Py_ssize_t>literal_id
            if literal_index >= body_literal_count:
                break
            clingo_literal = body_literal_id_to_clingo[literal_index]
            if clingo_literal is None:
                break
            new_body.append(clingo_literal)
        else:
            yield new_body


def find_deep_bindings_ids(
    object body_ids,
    int max_vars,
    object head_args,
    object head_types,
    object body_types,
    object literal_id_to_pred,
    object literal_id_to_args,
):
    cdef int num_vars
    cdef int i
    cdef object lit_id
    cdef object pred
    cdef object args
    cdef int var
    cdef object xs
    cdef int x
    cdef int y
    cdef int v
    cdef dict assignment
    cdef dict var_type_lookup
    cdef dict lookup
    cdef set head_vars
    cdef set body_vars
    cdef set bad_type_matching

    if len(body_types) == 0 or head_types is None:
        num_vars = len({var for lit_id in body_ids for var in literal_id_to_args[lit_id]})
        for xs in permutations(range(max_vars), num_vars):
            yield {i: xs[i] for i in range(num_vars)}
        return

    var_type_lookup = {i: t for i, t in enumerate(head_types)}
    head_vars = set(range(len(head_args)))
    body_vars = set()

    for lit_id in body_ids:
        pred = literal_id_to_pred[lit_id]
        args = literal_id_to_args[lit_id]
        for i, x in enumerate(args):
            body_vars.add(x)
            if x not in head_vars and pred in body_types:
                var_type_lookup[x] = body_types[pred][i]

    bad_type_matching = {
        (x, y)
        for x in body_vars if x in var_type_lookup
        for y in head_vars if y in var_type_lookup
        if var_type_lookup[x] != var_type_lookup[y]
    }

    lookup = {x: i for i, x in enumerate(body_vars)}
    for xs in permutations(range(max_vars), len(lookup)):
        assignment = {}
        for x in body_vars:
            v = xs[lookup[x]]
            if (x, v) in bad_type_matching:
                break
            assignment[x] = v
        else:
            yield assignment


def unsat_constraint_ids(
    object body_ids,
    int max_vars,
    object head_args,
    object head_types,
    object body_types,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
    object body_literal_id_to_clingo,
):
    cdef object assignment
    cdef object lit_id
    cdef object pred
    cdef object args
    cdef tuple args2
    cdef object literal_id
    cdef object clingo_literal
    cdef object literal_id_get = literal_id_by_pred_args.get
    cdef list rule
    cdef list tmp
    cdef list body_pre
    cdef Py_ssize_t body_literal_count = len(body_literal_id_to_clingo)
    cdef Py_ssize_t literal_index
    cdef int k, n_args

    if len(body_types) == 0:
        body_ids = remap_body_literal_ids(
            body_ids,
            literal_id_to_pred,
            literal_id_to_args,
            literal_id_by_pred_args,
        )

    body_pre = []
    for lit_id in body_ids:
        pred = literal_id_to_pred[lit_id]
        args = literal_id_to_args[lit_id]
        body_pre.append((pred, args, [None] * len(args)))

    for assignment in find_deep_bindings_ids(
        body_ids,
        max_vars,
        head_args,
        head_types,
        body_types,
        literal_id_to_pred,
        literal_id_to_args,
    ):
        rule = []
        for pred, args, tmp in body_pre:
            n_args = len(tmp)
            for k in range(n_args):
                tmp[k] = assignment[args[k]]
            args2 = tuple(tmp)
            literal_id = literal_id_get((pred, args2))
            if literal_id is None:
                break
            literal_index = <Py_ssize_t>literal_id
            if literal_index >= body_literal_count:
                break
            clingo_literal = body_literal_id_to_clingo[literal_index]
            if clingo_literal is None:
                break
            rule.append(clingo_literal)
        else:
            yield rule

def build_specialisation_constraint_ids(
    object rule,
    object size,
    int max_vars,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
    object body_literal_id_to_clingo,
    object cached_clingo_atoms,
):
    cdef object body
    cdef object size_literal
    cdef object head_args = rule[0].arguments
    cdef object body_ids = rule[1]

    if not size:
        yield from find_variants_ids(
            head_args,
            body_ids,
            max_vars,
            False,
            literal_id_to_pred,
            literal_id_to_args,
            literal_id_by_pred_args,
            body_literal_id_to_clingo,
        )
        return

    size_literal = cached_clingo_atoms[(True, "program_size_at_least", (size,))]
    for body in find_variants_ids(
        head_args,
        body_ids,
        max_vars,
        False,
        literal_id_to_pred,
        literal_id_to_args,
        literal_id_by_pred_args,
        body_literal_id_to_clingo,
    ):
        yield body + [size_literal]


def build_generalisation_constraint_ids(
    object rule,
    object size,
    int max_vars,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
    object body_literal_id_to_clingo,
    object cached_clingo_atoms,
):
    cdef object body
    cdef object head_args = rule[0].arguments
    cdef object body_ids = rule[1]

    for body in find_variants_ids(
        head_args,
        body_ids,
        max_vars,
        True,
        literal_id_to_pred,
        literal_id_to_args,
        literal_id_by_pred_args,
        body_literal_id_to_clingo,
    ):
        body.append(cached_clingo_atoms[(True, "body_size", (0, len(body)))])
        if size:
            body.append(cached_clingo_atoms[(True, "program_size_at_least", (size,))])
        yield body


def constrain(
    object cons,
    NativeNoRecControl native_control,
    int max_vars,
    object head_args,
    object head_types,
    object body_types,
    object literal_to_id,
    object literal_id_to_pred,
    object literal_id_to_args,
    object literal_id_by_pred_args,
    object body_literal_id_to_clingo,
    object cached_clingo_atoms,
    int specialisation,
    int unsat,
    int banish,
    int generalisation,
):
    cdef object xs
    cdef int con_type
    cdef object con_prog
    cdef object con_size
    cdef object rule
    cdef object ground_body

    if not cons:
        return
    native_control.begin_clause_batch()
    try:
        for xs in cons:
            con_type = xs[0]
            con_prog = xs[1]
            con_size = xs[2] if len(xs) > 2 else None

            if con_type == specialisation:
                rule = prog_to_id_rule(con_prog, literal_to_id)
                for ground_body in build_specialisation_constraint_ids(
                    rule,
                    con_size,
                    max_vars,
                    literal_id_to_pred,
                    literal_id_to_args,
                    literal_id_by_pred_args,
                    body_literal_id_to_clingo,
                    cached_clingo_atoms,
                ):
                    native_control.add_nogood_c(ground_body)
            elif con_type == unsat:
                for ground_body in unsat_constraint_ids(
                    body_to_literal_ids(con_prog, literal_to_id),
                    max_vars,
                    head_args,
                    head_types,
                    body_types,
                    literal_id_to_pred,
                    literal_id_to_args,
                    literal_id_by_pred_args,
                    body_literal_id_to_clingo,
                ):
                    native_control.add_nogood_c(ground_body)
            elif con_type == banish or con_type == generalisation:
                rule = prog_to_id_rule(con_prog, literal_to_id)
                for ground_body in build_generalisation_constraint_ids(
                    rule,
                    con_size,
                    max_vars,
                    literal_id_to_pred,
                    literal_id_to_args,
                    literal_id_by_pred_args,
                    body_literal_id_to_clingo,
                    cached_clingo_atoms,
                ):
                    native_control.add_nogood_c(ground_body)
    finally:
        native_control.end_clause_batch()
