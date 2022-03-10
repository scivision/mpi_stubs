cmake_minimum_required(VERSION 3.14...3.23)

project(MPI_stubs
LANGUAGES C CXX Fortran
VERSION 1.0.0
HOMEPAGE_URL http://people.math.sc.edu/Burkardt)

include(CTest)

# --- C, C++
add_library(mpi_c src/c/mpi.c)
target_include_directories(mpi_c PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/c>
$<INSTALL_INTERFACE:include>
)
# for FetchContent of this library
add_library(MPI::MPI_C INTERFACE IMPORTED GLOBAL)
target_link_libraries(MPI::MPI_C INTERFACE mpi_c)

add_library(MPI::MPI_CXX INTERFACE IMPORTED GLOBAL)
target_link_libraries(MPI::MPI_CXX INTERFACE mpi_c)

# --- Fortran
add_library(mpi_f src/fortran/mpi_stubs.f90 src/fortran/mpi_types.f90
src/fortran/reduce.f90)
set_target_properties(mpi_f PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
target_include_directories(mpi_f INTERFACE ${PROJECT_BINARY_DIR}/include)
# for FetchContent of this library
add_library(MPI::MPI_Fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(MPI::MPI_Fortran INTERFACE mpi_f)

if(BUILD_TESTING)
  add_subdirectory(examples)
endif()
