add_library(mpi_cpp mpi_stubs.cpp)
target_include_directories(mpi_cpp INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
