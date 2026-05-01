from setuptools import Extension, setup

try:
    from Cython.Build import cythonize
except ImportError as exc:
    raise RuntimeError("Cython is required to build Popper's C extensions") from exc


setup(
    ext_modules=cythonize(
        [
            Extension(
                "popper._canonicalise_hash",
                ["popper/_canonicalise_hash.pyx"],
            )
        ],
        compiler_directives={"language_level": "3"},
    ),
)
