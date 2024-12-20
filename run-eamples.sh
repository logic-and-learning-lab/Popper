#!/bin/bash

JOBS=10
mypath="examples/"

export mypath
# find "$mypath" -mindepth 1 -maxdepth 1 -type d | parallel --jobs=$JOBS python popper.py {} -q
# find "$mypath" -mindepth 1 -maxdepth 1 -type d | parallel --jobs=$JOBS python popper.py {} -q --noisy

find "$mypath" -mindepth 1 -maxdepth 1 -type d | parallel --jobs=$JOBS 'echo Processing {}; python popper.py {} -q'
find "$mypath" -mindepth 1 -maxdepth 1 -type d | parallel --jobs=$JOBS 'echo Processing {}; python popper.py {} -q --noisy'