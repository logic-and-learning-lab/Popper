# -*- coding: utf-8 -*-

import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="popper-ilp",
    version="1.1.0",                        # Update this for every new version
    author="Andrew Cropper",
    author_email="andrew.cropper@cs.ox.ac.uk",
    description="Popper",
    include_package_data=True,
    long_description=long_description,
    long_description_content_type="text/markdown",
    py_modules=['popper'],
    install_requires=[
        'clingo',
        'pyswip'
    ],                                             
    url="https://github.com/logic-and-learning-lab/Popper",
    packages=setuptools.find_packages()
    # classifiers=(                                 # Classifiers help people find your
        # "Programming Language :: Python :: 3"    # projects. See all possible classifiers
    # ),
)