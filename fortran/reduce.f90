module reduce

use, intrinsic :: iso_fortran_env, only : real32, real64, int32

use mpi_types


implicit none
private
public :: mpi_reduce_scatter, mpi_reduce, mpi_allreduce


interface mpi_reduce_scatter
!! MPI_REDUCE_SCATTER collects a message of the same length from each process.
!
!  Discussion:
!
!    Copy values from DATA1 to DATA2.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, DATATYPE DATA1(N), the data to be processed.
!
!    Output, DATATYPE DATA2, the value of the reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer DATATYPE, indicates the datatype of DATA1 and DATA2.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
procedure rs_int32, rs_real32, rs_real64
end interface mpi_reduce_scatter


interface mpi_reduce
!*****************************************************************************80
!
!! MPI_REDUCE carries out a reduction operation.
!
!  Discussion:
!
!    The reduction operations are sum, maximum, minimum, product.
!
!    The first two arguments must not overlap or share memory in any way.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!    Thanks to Simppa Akaslompolo for correcting this routine!
!    11 January 2012.
!
!
!  Parameters:
!
!    Input, DATATYPE DATA1(N), the data to be processed.
!
!    Output (to RECEIVER only), DATATYPE DATA2(N), the value of the
!    reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer DATATYPE, indicates the datatype of DATA1 and DATA2.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Input, integer RECEIVER, the process that is to receive the
!    result.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

procedure reduce_real64, reduce_real32, reduce_int32
end interface mpi_reduce


interface mpi_allreduce
!*****************************************************************************80
!
!! MPI_ALLREDUCE carries out a reduction operation.
!
!  Discussion:
!
!    The reduction operations are MAXIMUM, MINIMUM, PRODUCT and SUM.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!    Thanks to Simppa Akaslompolo for correcting this routine!
!    12 January 2012.
!
!
!  Parameters:
!
!    Input, DATATYPE DATA1(N), the data to be processed.
!
!    Output, DATATYPE DATA2(N), the value of the reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer DATATYPE, indicates the datatype of DATA1 and DATA2.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
procedure reduce_real32, reduce_real64, reduce_int32
end interface mpi_allreduce

contains


subroutine rs_int32 ( data1, data2, n, datatype, operation, comm, ierror )

integer(int32), intent(in) :: data1(n)
integer(int32), intent(out) :: data2(n)
integer, intent(in) ::  n, datatype, comm, operation
integer, intent(out) :: ierror

data2(1:n) = data1(1:n)

end subroutine


subroutine rs_real32 ( data1, data2, n, datatype, operation, comm, ierror )

real(real32), intent(in) :: data1(n)
real(real32), intent(out) :: data2(n)
integer, intent(in) ::  n, datatype, comm, operation
integer, intent(out) :: ierror

data2(1:n) = data1(1:n)

end subroutine


subroutine rs_real64 ( data1, data2, n, datatype, operation, comm, ierror )

real(real64), intent(in) :: data1(n)
real(real64), intent(out) :: data2(n)
integer, intent(in) ::  n, datatype, comm, operation
integer, intent(out) :: ierror

data2(1:n) = data1(1:n)
ierror = mpi_success
end subroutine


subroutine reduce_real64( data1, data2, n, datatype, operation, receiver, comm, ierror )

real(real64), intent(in) :: data1(n)
real(real64), intent(out) :: data2(n)
integer, intent(in) :: n, datatype, operation, receiver, comm
integer, intent(out) :: ierror

ierror = MPI_SUCCESS

select case(operation)
case(mpi_max, mpi_min, mpi_product, mpi_sum)
 data2(1:n) = data1(1:n)
case default
  ierror = MPI_FAILURE
end select

end subroutine reduce_real64

subroutine reduce_real32( data1, data2, n, datatype, operation, receiver, comm, ierror )

real(real32), intent(in) :: data1(n)
real(real32), intent(out) :: data2(n)
integer, intent(in) :: n, datatype, operation, receiver, comm
integer, intent(out) :: ierror

ierror = MPI_SUCCESS

select case(operation)
case(mpi_max, mpi_min, mpi_product, mpi_sum)
  data2(1:n) = data1(1:n)
case default
  ierror = MPI_FAILURE
end select

end subroutine reduce_real32


subroutine reduce_int32( data1, data2, n, datatype, operation, receiver, comm, ierror )

integer(int32), intent(in) :: data1(n)
integer(int32), intent(out) :: data2(n)
integer, intent(in) :: n, datatype, operation, receiver, comm
integer, intent(out) :: ierror

ierror = MPI_SUCCESS

select case(operation)
case(mpi_max, mpi_min, mpi_product, mpi_sum)
  data2(1:n) = data1(1:n)
case default
  ierror = MPI_FAILURE
end select

end subroutine reduce_int32


end module reduce
