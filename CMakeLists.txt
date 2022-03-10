cmake_minimum_required(VERSION 3.14...${CMAKE_VERSION})

project(MPI_stubs
LANGUAGES C CXX Fortran
VERSION 1.0.0
HOMEPAGE_URL http://people.math.sc.edu/Burkardt)

include(CTest)

# MPI::MPI_C
add_library(mpi_c c/mpi_stubs.c)
target_include_directories(mpi_c INTERFACE c)
add_library(MPI::MPI_C ALIAS mpi_c)
install(TARGETS mpi_c 
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin)
install(FILES c/mpi_stubs_c.h DESTINATION include)

# MPI::MPI_CXX
add_library(mpi_cpp cpp/mpi_stubs.cpp)
target_include_directories(mpi_cpp INTERFACE cpp)
add_library(MPI::MPI_CXX ALIAS mpi_cpp)
install(TARGETS mpi_cpp 
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin)
install(FILES cpp/mpi_stubs.hpp DESTINATION include)

# MPI::MPI_Fortran
add_library(mpi_f fortran/mpi_stubs.f90 fortran/mpi_types.f90
fortran/reduce.f90)
set_target_properties(mpi_f PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
target_include_directories(mpi_f INTERFACE ${PROJECT_BINARY_DIR}/include)
add_library(MPI::MPI_Fortran ALIAS mpi_f)
install(TARGETS mpi_f 
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin)

if(BUILD_TESTING)
  add_subdirectory(fortran/examples)
endif()
