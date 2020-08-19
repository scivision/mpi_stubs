# mpi_stubs

The MPI Stubs library is one of many fine libraries and programs by Professor
[John Burkhardt](http://people.math.sc.edu/Burkardt/f_src/mpi_stubs/mpi_stubs.html)
of Florida State University.

MPI Stubs is a dummy MPI-2 library for basic operations when actual MPI library not available.
If your problem is capable of running on a single processor, MPI Stubs may work for small problems, with the obvious side effect of usually taking longer to compute in serial than in parallel.
This library can be useful to get users quick-started on platforms like Windows where setting up MPI takes an extra step or two.

MPI Stubs comes for C, C++ and Fortran.
Each MPI Stubs code language operates indepedently--they do not depend on each other.
