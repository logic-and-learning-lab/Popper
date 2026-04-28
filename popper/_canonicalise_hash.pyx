# cython: language_level=3

from libc.string cimport memset
from cpython.exc cimport PyErr_Occurred
from cpython.long cimport PyLong_AsLong
from cpython.object cimport PyObject_Hash
from cpython.tuple cimport PyTuple_GET_ITEM, PyTuple_GET_SIZE


cdef inline unsigned long long mix_hash(
    unsigned long long seed,
    unsigned long long value,
) noexcept:
    return seed ^ (
        value
        + 0x9e3779b97f4a7c15ULL
        + (seed << 6)
        + (seed >> 2)
    )


cdef inline unsigned long long final_mix(unsigned long long value) noexcept:
    value ^= value >> 16
    value *= 0x85ebca6bULL
    value ^= value >> 13
    value *= 0xc2b2ae35ULL
    value ^= value >> 16
    return value


cdef object _canonicalise_v4_python(object rule, int max_vars):
    head, body = rule
    head_vars = set(head.arguments) if head else set()
    next_var = len(head_vars)
    lookup = {v: v for v in head_vars}
    new_body = []

    for lit in sorted(body):
        new_args = []
        for var in lit.arguments:
            if var not in lookup:
                lookup[var] = next_var
                next_var += 1
            new_args.append(lookup[var])
        new_body.append((lit.predicate, tuple(new_args)))

    return head, frozenset(new_body)


cdef unsigned long long _canonicalise_rule_hash(object rule, int max_vars) except? 0:
    cdef int i, v, mapped, next_var, arity
    cdef int lookup[128]
    cdef Py_ssize_t body_len
    cdef Py_hash_t py_hash
    cdef unsigned long long h = 0
    cdef unsigned long long body_hash = 0
    cdef unsigned long long lit_hash
    cdef object head
    cdef object body
    cdef object sorted_body
    cdef object lit
    cdef object head_tuple
    cdef object lit_tuple
    cdef object pred_obj
    cdef object args_obj
    cdef object var_obj

    head, body = rule

    if max_vars > 128 or max_vars < 0:
        return <unsigned long long>hash(_canonicalise_v4_python(rule, max_vars))

    memset(lookup, -1, max_vars * sizeof(int))

    if head is not None:
        head_tuple = head
        pred_obj = <object>PyTuple_GET_ITEM(head_tuple, 0)
        args_obj = <object>PyTuple_GET_ITEM(head_tuple, 1)

        py_hash = PyObject_Hash(pred_obj)
        if py_hash == -1 and PyErr_Occurred():
            raise
        h = mix_hash(h, <unsigned long long>py_hash)

        arity = <int>PyTuple_GET_SIZE(args_obj)
        for i in range(arity):
            var_obj = <object>PyTuple_GET_ITEM(args_obj, i)
            v = <int>PyLong_AsLong(var_obj)
            if v == -1 and PyErr_Occurred():
                raise
            if v < 0 or v >= max_vars:
                return <unsigned long long>hash(_canonicalise_v4_python(rule, max_vars))
            lookup[v] = v
            h = mix_hash(h, <unsigned long long>v)
        next_var = arity
    else:
        next_var = 0

    body_len = len(body)
    sorted_body = sorted(body) if body_len > 1 else body
    for lit in sorted_body:
        lit_tuple = lit
        pred_obj = <object>PyTuple_GET_ITEM(lit_tuple, 0)
        args_obj = <object>PyTuple_GET_ITEM(lit_tuple, 1)
        arity = <int>PyTuple_GET_SIZE(args_obj)

        py_hash = PyObject_Hash(pred_obj)
        if py_hash == -1 and PyErr_Occurred():
            raise
        lit_hash = <unsigned long long>py_hash

        for i in range(arity):
            var_obj = <object>PyTuple_GET_ITEM(args_obj, i)
            v = <int>PyLong_AsLong(var_obj)
            if v == -1 and PyErr_Occurred():
                raise
            if v < 0 or v >= max_vars:
                return <unsigned long long>hash(_canonicalise_v4_python(rule, max_vars))
            mapped = lookup[v]

            if mapped == -1:
                lookup[v] = next_var
                mapped = next_var
                next_var += 1

            lit_hash = mix_hash(lit_hash, <unsigned long long>mapped)

        body_hash ^= final_mix(lit_hash)

    return mix_hash(h, body_hash)


def canonicalise_rule_hash_cython(rule, int max_vars):
    return _canonicalise_rule_hash(rule, max_vars)


def canonicalise_prog_hash_cython(prog, int max_vars):
    cdef Py_ssize_t rule_count = 0
    cdef unsigned long long rule_hash
    cdef unsigned long long mixed
    cdef unsigned long long hash_xor = 0
    cdef unsigned long long hash_sum = 0
    cdef object rule

    for rule in prog:
        rule_hash = _canonicalise_rule_hash(rule, max_vars)
        mixed = final_mix(rule_hash)
        hash_xor ^= mixed
        hash_sum += mixed
        rule_count += 1

    return final_mix(mix_hash(mix_hash(hash_sum, hash_xor), <unsigned long long>rule_count))
