add_library(mpi_f mpi_stubs.f90 mpi_types.f90
reduce.f90)
set_target_properties(mpi_f PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
target_include_directories(mpi_f INTERFACE ${PROJECT_BINARY_DIR}/include)
add_library(MPI::MPI_Fortran ALIAS mpi_f)

if(PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  add_subdirectory(examples)
endif()
