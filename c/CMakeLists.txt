add_library(mpi_c mpi_stubs.c)
target_include_directories(mpi_c INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
add_library(MPI::MPI_C ALIAS mpi_c)
