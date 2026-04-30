# cython: language_level=3

from libc.stddef cimport size_t
from libc.stdint cimport int32_t, uint64_t
from libc.stdlib cimport free, malloc, realloc
from libc.string cimport strcmp


ctypedef unsigned char cbool


cdef extern from "clingo.h":
    ctypedef int clingo_symbol_type_t
    ctypedef int clingo_warning_t
    ctypedef unsigned clingo_show_type_bitset_t
    ctypedef unsigned clingo_solve_mode_bitset_t
    ctypedef unsigned clingo_solve_result_bitset_t
    ctypedef void (*clingo_logger_t)(clingo_warning_t code, const char *message, void *data)
    ctypedef bint (*clingo_ground_callback_t)(
        const void *location,
        const char *name,
        const clingo_symbol_t *arguments,
        size_t arguments_size,
        void *data,
        void *symbol_callback,
        void *symbol_callback_data,
    )
    ctypedef bint (*clingo_solve_event_callback_t)(unsigned type, void *event, void *data, bint *goon)

    ctypedef struct clingo_control_t:
        pass
    ctypedef struct clingo_model_t:
        pass
    ctypedef struct clingo_solve_handle_t:
        pass
    ctypedef struct clingo_solve_control_t:
        pass
    ctypedef struct clingo_symbolic_atoms_t:
        pass
    ctypedef uint64_t clingo_symbolic_atom_iterator_t
    ctypedef uint64_t clingo_signature_t
    ctypedef struct clingo_part_t:
        const char *name
        const clingo_symbol_t *params
        size_t size

    int clingo_symbol_type_number
    int clingo_symbol_type_function
    unsigned clingo_show_type_shown
    unsigned clingo_solve_mode_yield

    const char *clingo_error_message()

    bint clingo_control_new(
        const char *const *arguments,
        size_t arguments_size,
        clingo_logger_t logger,
        void *logger_data,
        unsigned message_limit,
        clingo_control_t **control,
    )
    void clingo_control_free(clingo_control_t *control)
    bint clingo_control_add(
        clingo_control_t *control,
        const char *name,
        const char *const *parameters,
        size_t parameters_size,
        const char *program,
    )
    bint clingo_control_ground(
        clingo_control_t *control,
        const clingo_part_t *parts,
        size_t parts_size,
        clingo_ground_callback_t ground_callback,
        void *ground_callback_data,
    )
    bint clingo_control_solve(
        clingo_control_t *control,
        clingo_solve_mode_bitset_t mode,
        const clingo_literal_t *assumptions,
        size_t assumptions_size,
        clingo_solve_event_callback_t notify,
        void *data,
        clingo_solve_handle_t **handle,
    )
    bint clingo_solve_handle_model(clingo_solve_handle_t *handle, const clingo_model_t **model)
    bint clingo_solve_handle_resume(clingo_solve_handle_t *handle)
    bint clingo_solve_handle_close(clingo_solve_handle_t *handle)
    bint clingo_model_context(const clingo_model_t *model, clingo_solve_control_t **control)
    bint clingo_solve_control_add_clause(
        clingo_solve_control_t *control,
        const clingo_literal_t *clause,
        size_t size,
    )
    bint clingo_control_symbolic_atoms(const clingo_control_t *control, const clingo_symbolic_atoms_t **atoms)
    bint clingo_symbolic_atoms_begin(
        const clingo_symbolic_atoms_t *atoms,
        const clingo_signature_t *signature,
        clingo_symbolic_atom_iterator_t *iterator,
    )
    bint clingo_symbolic_atoms_end(
        const clingo_symbolic_atoms_t *atoms,
        clingo_symbolic_atom_iterator_t *iterator,
    )
    bint clingo_symbolic_atoms_next(
        const clingo_symbolic_atoms_t *atoms,
        clingo_symbolic_atom_iterator_t iterator,
        clingo_symbolic_atom_iterator_t *next,
    )
    bint clingo_symbolic_atoms_symbol(
        const clingo_symbolic_atoms_t *atoms,
        clingo_symbolic_atom_iterator_t iterator,
        clingo_symbol_t *symbol,
    )
    bint clingo_symbolic_atoms_literal(
        const clingo_symbolic_atoms_t *atoms,
        clingo_symbolic_atom_iterator_t iterator,
        clingo_literal_t *literal,
    )
    bint clingo_model_symbols_size(
        const clingo_model_t *model,
        clingo_show_type_bitset_t show,
        size_t *size,
    )
    bint clingo_model_symbols(
        const clingo_model_t *model,
        clingo_show_type_bitset_t show,
        clingo_symbol_t *symbols,
        size_t size,
    )
    clingo_symbol_type_t clingo_symbol_type(clingo_symbol_t symbol)
    bint clingo_symbol_name(clingo_symbol_t symbol, const char **name)
    bint clingo_symbol_number(clingo_symbol_t symbol, int *number)
    bint clingo_symbol_arguments(
        clingo_symbol_t symbol,
        const clingo_symbol_t **arguments,
        size_t *arguments_size,
    )


cdef extern from *:
    """
    #include <stdbool.h>
    #include "clingo.h"
    static inline bool popper_clingo_symbolic_atoms_iterator_is_equal_to(
        clingo_symbolic_atoms_t const *atoms,
        clingo_symbolic_atom_iterator_t a,
        clingo_symbolic_atom_iterator_t b,
        unsigned char *equal
    ) {
        bool c_equal = false;
        bool ok = clingo_symbolic_atoms_iterator_is_equal_to(atoms, a, b, &c_equal);
        *equal = c_equal ? 1 : 0;
        return ok;
    }
    """
    bint popper_clingo_symbolic_atoms_iterator_is_equal_to(
        const clingo_symbolic_atoms_t *atoms,
        clingo_symbolic_atom_iterator_t a,
        clingo_symbolic_atom_iterator_t b,
        cbool *equal,
    )


cdef inline void _check(bint ok):
    if not ok:
        raise RuntimeError(clingo_error_message().decode("utf-8", "replace"))


cdef object _body_literal_id(clingo_symbol_t symbol, object literal_id_by_pred_args):
    cdef const char *name
    cdef const clingo_symbol_t *args
    cdef const clingo_symbol_t *tuple_args
    cdef size_t args_size = 0
    cdef size_t tuple_size = 0
    cdef size_t i
    cdef int number
    cdef list variables
    cdef object pred

    if clingo_symbol_type(symbol) != clingo_symbol_type_function:
        return None

    _check(clingo_symbol_name(symbol, &name))
    if strcmp(name, b"body_literal") != 0:
        return None

    _check(clingo_symbol_arguments(symbol, &args, &args_size))
    if args_size != 4:
        return None

    _check(clingo_symbol_name(args[1], &name))
    pred = name.decode("utf-8", "replace")

    _check(clingo_symbol_arguments(args[3], &tuple_args, &tuple_size))
    variables = []
    for i in range(tuple_size):
        if clingo_symbol_type(tuple_args[i]) != clingo_symbol_type_number:
            return None
        _check(clingo_symbol_number(tuple_args[i], &number))
        variables.append(number)

    return literal_id_by_pred_args.get((pred, tuple(variables)))


cdef object _special_atom_key(clingo_symbol_t symbol):
    cdef const char *name
    cdef const clingo_symbol_t *args
    cdef size_t args_size = 0
    cdef int number

    if clingo_symbol_type(symbol) != clingo_symbol_type_function:
        return None
    _check(clingo_symbol_name(symbol, &name))
    _check(clingo_symbol_arguments(symbol, &args, &args_size))

    if strcmp(name, b"size") == 0:
        if args_size != 1 or clingo_symbol_type(args[0]) != clingo_symbol_type_number:
            return None
        _check(clingo_symbol_number(args[0], &number))
        return ("size", (number,))

    if strcmp(name, b"body_size") == 0:
        if args_size != 2 or clingo_symbol_type(args[0]) != clingo_symbol_type_number or clingo_symbol_type(args[1]) != clingo_symbol_type_number:
            return None
        _check(clingo_symbol_number(args[0], &number))
        rule_id = number
        _check(clingo_symbol_number(args[1], &number))
        return ("body_size", (rule_id, number))

    if strcmp(name, b"program_size_at_least") == 0:
        if args_size != 1 or clingo_symbol_type(args[0]) != clingo_symbol_type_number:
            return None
        _check(clingo_symbol_number(args[0], &number))
        return ("program_size_at_least", (number,))

    return None


cdef class NativeNoRecControl:
    def __cinit__(self):
        self._control = NULL
        self._handle = NULL
        self._model = NULL
        self._needs_resume = False
        self._batch_control = NULL
        self._clause_buffer = NULL
        self._clause_capacity = 0
        self._model_symbols = NULL
        self._model_symbols_capacity = 0
        self._symbol_to_literal_id = {}

    def __init__(self, str encoding):
        cdef const char *args[3]
        cdef clingo_part_t part
        cdef bytes encoding_bytes = encoding.encode("utf-8")

        # The clingo wheel loads its C symbols globally in clingo._internal.
        import clingo._internal  # noqa: F401

        args[0] = b"--heuristic=Domain"
        args[1] = b"-Wnone"
        args[2] = b"--models=0"

        _check(clingo_control_new(args, 3, NULL, NULL, 20, &self._control))
        _check(clingo_control_add(self._control, b"base", NULL, 0, encoding_bytes))
        part.name = b"base"
        part.params = NULL
        part.size = 0
        _check(clingo_control_ground(self._control, &part, 1, NULL, NULL))

    def build_literal_caches(self, object literal_id_by_pred_args):
        cdef const clingo_symbolic_atoms_t *atoms = NULL
        cdef clingo_symbolic_atom_iterator_t it
        cdef clingo_symbolic_atom_iterator_t end
        cdef clingo_symbol_t symbol
        cdef clingo_literal_t literal
        cdef cbool equal = False
        cdef object lit_id
        cdef object special_key
        cdef dict body_literal_id_to_clingo = {}
        cdef dict special_atoms = {}

        self._symbol_to_literal_id = {}
        _check(clingo_control_symbolic_atoms(self._control, &atoms))
        _check(clingo_symbolic_atoms_begin(atoms, NULL, &it))
        _check(clingo_symbolic_atoms_end(atoms, &end))

        while True:
            _check(popper_clingo_symbolic_atoms_iterator_is_equal_to(atoms, it, end, &equal))
            if equal:
                break
            _check(clingo_symbolic_atoms_symbol(atoms, it, &symbol))
            _check(clingo_symbolic_atoms_literal(atoms, it, &literal))

            lit_id = _body_literal_id(symbol, literal_id_by_pred_args)
            if lit_id is not None:
                body_literal_id_to_clingo[lit_id] = literal
                self._symbol_to_literal_id[symbol] = lit_id
            else:
                special_key = _special_atom_key(symbol)
                if special_key is not None:
                    special_atoms[special_key] = literal

            _check(clingo_symbolic_atoms_next(atoms, it, &it))

        return body_literal_id_to_clingo, special_atoms

    def __dealloc__(self):
        if self._handle != NULL:
            clingo_solve_handle_close(self._handle)
            self._handle = NULL
            self._model = NULL
            self._needs_resume = False
            self._batch_control = NULL
        if self._control != NULL:
            clingo_control_free(self._control)
            self._control = NULL
        if self._clause_buffer != NULL:
            free(self._clause_buffer)
            self._clause_buffer = NULL
            self._clause_capacity = 0
        if self._model_symbols != NULL:
            free(self._model_symbols)
            self._model_symbols = NULL
            self._model_symbols_capacity = 0

    cdef void _close_handle(self):
        if self._handle != NULL:
            _check(clingo_solve_handle_close(self._handle))
            self._handle = NULL
        self._model = NULL
        self._needs_resume = False
        self._batch_control = NULL

    cdef void _ensure_handle(self):
        if self._handle == NULL:
            _check(clingo_control_solve(
                self._control,
                clingo_solve_mode_yield,
                NULL,
                0,
                NULL,
                NULL,
                &self._handle,
            ))

    cdef void _ensure_model_symbol_capacity(self, size_t size) except *:
        cdef clingo_symbol_t *new_buffer = NULL
        cdef size_t new_cap

        if size <= self._model_symbols_capacity:
            return

        new_cap = self._model_symbols_capacity if self._model_symbols_capacity > 0 else 8
        while new_cap < size:
            new_cap *= 2

        new_buffer = <clingo_symbol_t *>realloc(
            self._model_symbols,
            new_cap * sizeof(clingo_symbol_t),
        )
        if new_buffer == NULL:
            raise MemoryError()

        self._model_symbols = new_buffer
        self._model_symbols_capacity = new_cap

    cdef list _model_body_ids(self, const clingo_model_t *model):
        cdef size_t size = 0
        cdef size_t i
        cdef object lit_id
        cdef object symbol_to_literal_id_get = self._symbol_to_literal_id.get
        cdef list out = []

        _check(clingo_model_symbols_size(model, clingo_show_type_shown, &size))
        self._ensure_model_symbol_capacity(size)
        _check(clingo_model_symbols(model, clingo_show_type_shown, self._model_symbols, size))
        for i in range(size):
            lit_id = symbol_to_literal_id_get(self._model_symbols[i])
            if lit_id is not None:
                out.append(lit_id)

        return out

    def start_solve(self):
        self._close_handle()
        self._ensure_handle()

    def next_model_body_ids(self):
        cdef const clingo_model_t *model = NULL

        self._ensure_handle()
        if self._needs_resume:
            _check(clingo_solve_handle_resume(self._handle))
            self._needs_resume = False
            self._model = NULL

        _check(clingo_solve_handle_model(self._handle, &model))
        if model == NULL:
            self._close_handle()
            return None

        self._model = model
        self._needs_resume = True
        return self._model_body_ids(model)

    cdef void _ensure_clause_capacity(self, size_t size) except *:
        cdef clingo_literal_t *new_buffer = NULL
        cdef size_t new_cap

        if size <= self._clause_capacity:
            return

        new_cap = self._clause_capacity if self._clause_capacity > 0 else 8
        while new_cap < size:
            new_cap *= 2

        new_buffer = <clingo_literal_t *>realloc(
            self._clause_buffer,
            new_cap * sizeof(clingo_literal_t),
        )
        if new_buffer == NULL:
            raise MemoryError()

        self._clause_buffer = new_buffer
        self._clause_capacity = new_cap

    cdef void begin_clause_batch(self) except *:
        if self._model == NULL:
            raise RuntimeError("cannot add nogood without a current model")
        _check(clingo_model_context(self._model, &self._batch_control))

    cdef void add_nogood_c(self, object literals) except *:
        cdef Py_ssize_t size = len(literals)
        cdef Py_ssize_t i

        if self._batch_control == NULL:
            raise RuntimeError("cannot add nogood outside a clause batch")

        self._ensure_clause_capacity(<size_t>size)
        for i in range(size):
            self._clause_buffer[i] = -<clingo_literal_t>literals[i]
        _check(clingo_solve_control_add_clause(
            self._batch_control,
            self._clause_buffer,
            <size_t>size,
        ))

    cdef void end_clause_batch(self) noexcept:
        self._batch_control = NULL

    def add_nogood(self, object literals):
        self.begin_clause_batch()
        try:
            self.add_nogood_c(literals)
        finally:
            self.end_clause_batch()

    def first_model_body_ids(self):
        if self._handle != NULL:
            self._close_handle()
        return self.next_model_body_ids()
