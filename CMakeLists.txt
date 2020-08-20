cmake_minimum_required(VERSION 3.14)
project(MPI_stubs
LANGUAGES C CXX Fortran
VERSION 1.0.0
HOMEPAGE_URL http://people.math.sc.edu/Burkardt)
enable_testing()

add_subdirectory(c)
add_subdirectory(cpp)
add_subdirectory(fortran)
