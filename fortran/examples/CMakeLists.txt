add_executable(helloworld_f helloworld.f90)
target_link_libraries(helloworld_f MPI::MPI_Fortran)
add_test(NAME fortran:helloworld COMMAND helloworld_f)

add_executable(mpivers_f mpivers.f90)
target_link_libraries(mpivers_f MPI::MPI_Fortran)
add_test(NAME fortran:version COMMAND mpivers_f)

add_executable(pass_f thread_pass.f90)
target_link_libraries(pass_f MPI::MPI_Fortran)
add_test(NAME fortran:pass COMMAND pass_f)
set_tests_properties(fortran:pass PROPERTIES WILL_FAIL true)
