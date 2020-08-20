module mpi_types

implicit none

integer, parameter ::  mpi_comm_world = 0
!
!  Return values
!
integer, parameter :: mpi_failure = 1
integer, parameter :: mpi_success = 0
!
!  recv message status
!
integer, parameter :: mpi_status_size = 3
integer, parameter ::  mpi_source = 1
integer, parameter ::  mpi_tag = 2
integer, parameter ::  mpi_count = 3
!
!  recv flags
!
integer, parameter ::  mpi_any_source = -1
integer, parameter ::  mpi_any_tag = -1
!
!  data types and sizes
!
integer, parameter :: mpi_integer = 1
integer, parameter :: mpi_real = 2
integer, parameter :: mpi_double_precision = 3
integer, parameter :: mpi_logical = 4
integer, parameter :: mpi_character = 5
!
!  allreduce operations
!
integer, parameter :: mpi_sum = 1
integer, parameter ::  mpi_max = 2
integer, parameter ::  mpi_min = 3
integer, parameter ::  mpi_product = 4

!> others

integer, parameter :: MPI_MAX_LIBRARY_VERSION_STRING = 3

end module mpi_types
