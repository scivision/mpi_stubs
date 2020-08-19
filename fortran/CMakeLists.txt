add_library(mpi_f mpi_stubs.f90)
target_include_directories(mpi_f INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
