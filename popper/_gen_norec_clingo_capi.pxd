from libc.stddef cimport size_t
from libc.stdint cimport int32_t, uint64_t


cdef extern from "clingo.h":
    ctypedef int32_t clingo_literal_t
    ctypedef uint64_t clingo_symbol_t

    ctypedef struct clingo_control_t:
        pass
    ctypedef struct clingo_model_t:
        pass
    ctypedef struct clingo_solve_handle_t:
        pass
    ctypedef struct clingo_solve_control_t:
        pass


cdef class NativeNoRecControl:
    cdef clingo_control_t *_control
    cdef clingo_solve_handle_t *_handle
    cdef const clingo_model_t *_model
    cdef bint _needs_resume
    cdef clingo_solve_control_t *_batch_control
    cdef clingo_literal_t *_clause_buffer
    cdef size_t _clause_capacity
    cdef clingo_symbol_t *_model_symbols
    cdef size_t _model_symbols_capacity
    cdef dict _symbol_to_literal_id

    cdef void _close_handle(self) except *
    cdef void _ensure_handle(self) except *
    cdef list _model_body_ids(self, const clingo_model_t *model)
    cdef void _ensure_model_symbol_capacity(self, size_t size) except *
    cdef void _ensure_clause_capacity(self, size_t size) except *
    cdef void begin_clause_batch(self) except *
    cdef void add_nogood_c(self, object literals) except *
    cdef void end_clause_batch(self) noexcept
