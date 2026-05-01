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
        raise ValueError(f"max_vars must be between 0 and 128, got {max_vars}")

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
                raise ValueError(f"variable {v} out of range for max_vars={max_vars}")
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
                raise ValueError(f"variable {v} out of range for max_vars={max_vars}")
            mapped = lookup[v]

            if mapped == -1:
                lookup[v] = next_var
                mapped = next_var
                next_var += 1

            lit_hash = mix_hash(lit_hash, <unsigned long long>mapped)

        body_hash = mix_hash(body_hash, final_mix(lit_hash))

    body_hash = mix_hash(body_hash, <unsigned long long>body_len)
    return final_mix(mix_hash(h, body_hash))


def canonicalise_rule_hash_cython(rule, int max_vars):
    return _canonicalise_rule_hash(rule, max_vars)


def canonicalise_prog_hash_cython(prog, int max_vars):
    cdef unsigned long long prog_hash = 0
    cdef unsigned long long rule_hash
    cdef object rule
    cdef object rule_hashes
    cdef object h

    rule_hashes = []
    for rule in prog:
        rule_hash = _canonicalise_rule_hash(rule, max_vars)
        rule_hashes.append(rule_hash)

    rule_hashes.sort()

    for h in rule_hashes:
        prog_hash = mix_hash(prog_hash, <unsigned long long>h)

    prog_hash = mix_hash(prog_hash, <unsigned long long>len(rule_hashes))
    return final_mix(prog_hash)
