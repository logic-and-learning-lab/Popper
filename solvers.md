# Install MaxSAT Solvers for Popper

### UWrMaxSAT

See the "quick install" section: <https://github.com/marekpiotrow/UWrMaxSat>

Ensure that `uwrmaxsat` is correctly installed and add it to your PATH.

### WMaxCDCL

```bash
wget https://maxsat-evaluations.github.io/2023/mse23-solver-src/exact/WMaxCDCL.zip
unzip WMaxCDCL.zip
cd WMaxCDCL/code/simp
make
```

Ensure that `wmaxcdcl` is correctly installed and add it to your PATH.

### NuWLS-c

```bash
wget https://maxsat-evaluations.github.io/2023/mse23-solver-src/anytime/NuWLS-c-2023.zip
unzip NuWLS-c-2023.zip
cd NuWLS-c-2023/code
make
cd ../bin
```

Ensure that `NuWLS-c` is correctly installed and add it to your PATH.

### FAQ

*I use MacOS and Homebrew, and I cannot build the solvers:*
Try adding these to the Makefile:

```makefile
LFLAGS     += -L/opt/homebrew/lib/
CFLAGS     += -I/opt/homebrew/include/
```

*I get the error: FileNotFoundError: No such file or directory: 'timeout':*
When using an anytime solver, the `timeout` command must be installed. On MacOS, try this command:

```bash
brew install coreutils
```
