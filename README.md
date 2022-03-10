# MPI stub

[![ci](https://github.com/scivision/mpi_stubs/actions/workflows/ci.yml/badge.svg)](https://github.com/scivision/mpi_stubs/actions/workflows/ci.yml)

The MPI Stubs library is one of many fine libraries and programs by Professor
[John Burkhardt](http://people.math.sc.edu/Burkardt/f_src/mpi_stubs/mpi_stubs.html)
of Florida State University.

MPI Stubs is a dummy MPI-2 library for basic operations when actual MPI library not available.
If your problem is capable of running on a single processor, MPI Stubs may work for small problems, with the obvious side effect of usually taking longer to compute in serial than in parallel.
This library can be useful to get users quick-started on platforms like Windows where setting up MPI takes an extra step or two.

MPI Stubs works for C, C++ and Fortran much like regular MPI, but on a single CPU core.

We have provided a CMake install package, or you can use this via CMake FetchContent or ExternalProject.
The Examples directory shows how you would use an installed copy of MPI stubs.

```sh
cmake -B build -DCMAKE_INSTALL_PREFIX=~/mpis
cmake --build build
cmake ---install build
```

Then the example using that package:

```sh
cd examples
cmake -B build -DCMAKE_PREFIX_PATH=~/mpis
cmake --build build
ctest --test-dir build -V
```
