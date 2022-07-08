cmake_minimum_required(VERSION 3.14...3.24)

project(mpi_stub
LANGUAGES C CXX
VERSION 1.0.1
HOMEPAGE_URL http://people.math.sc.edu/Burkardt)

option(fortran "Build MPI::MPI_Fortran" ON)

if(fortran)
  enable_language(Fortran)
endif()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

# --- C, C++
add_library(mpi_c src/mpi.c)
target_include_directories(mpi_c PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
$<INSTALL_INTERFACE:include>
)
set_target_properties(mpi_c PROPERTIES
EXPORT_NAME MPI_C
)

add_library(mpi_cxx src/mpi.c)
target_include_directories(mpi_cxx INTERFACE
$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
$<INSTALL_INTERFACE:include>
)
set_target_properties(mpi_cxx PROPERTIES
EXPORT_NAME MPI_CXX
)

# for FetchContent of this library
add_library(MPI::MPI_C INTERFACE IMPORTED GLOBAL)
target_link_libraries(MPI::MPI_C INTERFACE mpi_c)

add_library(MPI::MPI_CXX INTERFACE IMPORTED GLOBAL)
target_link_libraries(MPI::MPI_CXX INTERFACE mpi_cxx)

# --- Fortran
if(fortran)
  add_library(mpi_fortran src/mpi_stubs.f90 src/mpi_types.f90 src/reduce.f90)
  set_target_properties(mpi_fortran PROPERTIES
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include
  EXPORT_NAME MPI_Fortran
  )
  target_include_directories(mpi_fortran PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:include>
  )
  # for FetchContent of this library
  add_library(MPI::MPI_Fortran INTERFACE IMPORTED GLOBAL)
  target_link_libraries(MPI::MPI_Fortran INTERFACE mpi_fortran)

  install(TARGETS mpi_fortran
  EXPORT ${PROJECT_NAME}-targets
  )
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/mpi.mod
  TYPE INCLUDE
  )
endif()

install(TARGETS mpi_c mpi_cxx
EXPORT ${PROJECT_NAME}-targets
)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/mpi.h
TYPE INCLUDE
)

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/install.cmake)
