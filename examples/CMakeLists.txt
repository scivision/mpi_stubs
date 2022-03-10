cmake_minimum_required(VERSION 3.14...3.23)

project(MPI_stub_examples
LANGUAGES C CXX)

enable_testing()

find_package(mpi_stub CONFIG REQUIRED)
if(TARGET MPI::MPI_Fortran)
  enable_language(Fortran)
endif()

add_executable(main_c main.c)
target_link_libraries(main_c PRIVATE MPI::MPI_C)
add_test(NAME c:basic COMMAND main_c)

add_executable(main_cpp main.cpp)
target_link_libraries(main_cpp PRIVATE MPI::MPI_CXX)
add_test(NAME cpp:basic COMMAND main_cpp)

if(TARGET MPI::MPI_Fortran)
  add_executable(helloworld_f helloworld.f90)
  target_link_libraries(helloworld_f PRIVATE MPI::MPI_Fortran)
  add_test(NAME fortran:helloworld COMMAND helloworld_f)

  add_executable(mpivers_f mpivers.f90)
  target_link_libraries(mpivers_f PRIVATE MPI::MPI_Fortran)
  add_test(NAME fortran:version COMMAND mpivers_f)

  add_executable(pass_f thread_pass.f90)
  target_link_libraries(pass_f PRIVATE MPI::MPI_Fortran)
  add_test(NAME fortran:pass COMMAND pass_f)
  set_tests_properties(fortran:pass PROPERTIES WILL_FAIL true)
endif()

get_property(_test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${_test_names} PROPERTIES
TIMEOUT 10
)

if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
get_target_property(libdir MPI::MPI_C IMPORTED_LOCATION_RELEASE)
  if(libdir)
    cmake_path(GET libdir PARENT_PATH dlldir)
    message(STATUS "DLL ${dlldir}")
    if(IS_DIRECTORY ${dlldir})
      set_tests_properties(${_test_names} PROPERTIES
      ENVIRONMENT_MODIFICATION PATH=path_list_append:${dlldir}
      )
    endif()
  endif()
endif()
