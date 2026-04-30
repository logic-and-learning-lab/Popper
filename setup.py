from pathlib import Path
import sys

from setuptools import Extension, setup

try:
    from Cython.Build import cythonize
except ImportError as exc:
    raise RuntimeError("Cython is required to build Popper's C extensions") from exc

try:
    import clingo

    CLINGO_INCLUDE_DIRS = [str(Path(clingo.__file__).parent)]
except ImportError:
    CLINGO_INCLUDE_DIRS = []

if sys.platform == "darwin":
    CLINGO_CAPI_EXTRA_LINK_ARGS = ["-undefined", "dynamic_lookup"]
else:
    CLINGO_CAPI_EXTRA_LINK_ARGS = []


setup(
    ext_modules=cythonize(
        [
            Extension(
                "popper._canonicalise_hash",
                ["popper/_canonicalise_hash.pyx"],
            ),
            Extension(
                "popper._gen_norec_id_native",
                ["popper/_gen_norec_id_native.pyx"],
                include_dirs=CLINGO_INCLUDE_DIRS,
            ),
            Extension(
                "popper._gen_norec_clingo_capi",
                ["popper/_gen_norec_clingo_capi.pyx"],
                include_dirs=CLINGO_INCLUDE_DIRS,
                extra_link_args=CLINGO_CAPI_EXTRA_LINK_ARGS,
            ),
        ],
        compiler_directives={"language_level": "3"},
    ),
)
