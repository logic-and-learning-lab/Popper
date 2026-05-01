import numpy as np

_EMPTY = np.uint64(2**64 - 1)
_EMPTY_INT = int(_EMPTY)


class CompactHashTable:
    """Open-addressing hash table: uint64 key -> fixed-width numpy value.

    Linear probing, 0.75 load factor, capacity doubles on resize.
    The internal empty-slot sentinel key = 2^64-1 is stored out-of-band if it
    appears as a real key.
    """

    def __init__(self, val_dtype, initial_capacity=1024):
        cap = 1 << max(1, (max(1, initial_capacity) - 1).bit_length())
        self._cap = cap
        self._imask = cap - 1
        self._val_dtype = val_dtype
        self._keys = np.full(cap, _EMPTY, dtype=np.uint64)
        self._vals = np.zeros(cap, dtype=val_dtype)
        self._has_empty_key = False
        self._empty_key_value = np.zeros((), dtype=val_dtype)[()]
        self._size = 0
        self._array_size = 0

    def __len__(self):
        return self._size

    def __contains__(self, key):
        ikey = int(np.uint64(key))
        if ikey == _EMPTY_INT:
            return self._has_empty_key
        idx = ikey & self._imask
        while True:
            k = int(self._keys[idx])
            if k == ikey:
                return True
            if k == _EMPTY_INT:
                return False
            idx = (idx + 1) & self._imask

    def __getitem__(self, key):
        ikey = int(np.uint64(key))
        if ikey == _EMPTY_INT:
            if self._has_empty_key:
                return self._empty_key_value
            raise KeyError(key)
        idx = ikey & self._imask
        while True:
            k = int(self._keys[idx])
            if k == ikey:
                return self._vals[idx]
            if k == _EMPTY_INT:
                raise KeyError(key)
            idx = (idx + 1) & self._imask

    def get(self, key, default=None):
        ikey = int(np.uint64(key))
        if ikey == _EMPTY_INT:
            if self._has_empty_key:
                return self._empty_key_value
            return default
        idx = ikey & self._imask
        while True:
            k = int(self._keys[idx])
            if k == ikey:
                return self._vals[idx]
            if k == _EMPTY_INT:
                return default
            idx = (idx + 1) & self._imask

    def __setitem__(self, key, value):
        ikey = int(np.uint64(key))
        if ikey == _EMPTY_INT:
            if not self._has_empty_key:
                self._has_empty_key = True
                self._size += 1
            self._empty_key_value = np.asarray(value, dtype=self._val_dtype)[()]
            return
        if (self._array_size + 1) * 4 >= self._cap * 3:
            self._resize()
        self._insert(ikey, value)

    def _insert(self, ikey, value):
        idx = ikey & self._imask
        while True:
            k = int(self._keys[idx])
            if k == _EMPTY_INT:
                self._keys[idx] = ikey
                self._vals[idx] = value
                self._size += 1
                self._array_size += 1
                return
            if k == ikey:
                self._vals[idx] = value
                return
            idx = (idx + 1) & self._imask

    def _resize(self):
        old_keys, old_vals = self._keys, self._vals
        self._cap <<= 1
        self._imask = self._cap - 1
        self._keys = np.full(self._cap, _EMPTY, dtype=np.uint64)
        self._vals = np.zeros(self._cap, dtype=self._val_dtype)
        self._size = 1 if self._has_empty_key else 0
        self._array_size = 0
        live = old_keys != _EMPTY
        for k, v in zip(old_keys[live], old_vals[live]):
            self._insert(int(k), v)


class IndexedInternPool:
    """Interns frozenbitarray objects, assigning each a unique integer index."""

    def __init__(self, max_index=np.iinfo(np.int32).max):
        self._map = {}
        self._list = []
        self._max_index = max_index

    def intern(self, ba):
        idx = self._map.get(ba)
        if idx is None:
            idx = len(self._list)
            if idx > self._max_index:
                raise OverflowError("IndexedInternPool index exceeds configured maximum")
            self._map[ba] = idx
            self._list.append(ba)
        return idx

    def lookup(self, idx):
        return self._list[int(idx)]

    def __len__(self):
        return len(self._list)
