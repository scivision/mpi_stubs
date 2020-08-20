module gather

use mpi_types

implicit none
private
public :: mpi_allgather, mpi_allgatherv

contains

subroutine mpi_allgather ( sendbuf, sendcount, sendtype, recvbuf, recvcount, &
  recvtype, comm, ierror )

!! MPI_ALLGATHER gathers data from all the processes in a communicator.
!
!  Discussion:
!
!    The block of data sent from the J-th process is placed in the
!    J-th block of the receive buffer of every process.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!
!  Parameters:
!
!    Input, SENDTYPE SENDBUF(SENDCOUNT), the data to be sent.
!
!    Inout, integer SENDCOUNT, the number of data items being sent.
!
!    Input, integer SENDTYPE, the datatype of the data being sent.
!
!    Output, RECVTYPE RECVBUF(RECVCOUNT,GROUPSIZE), the data as received.
!
!    Input, integer RECVCOUNT, the number of data items to be received
!    from any one process.
!
!    Input, integer RECVTYPE, the datatype of the data being received.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.


integer recvcount
integer sendcount

integer comm
integer ierror
integer recvbuf(recvcount,*)
integer recvtype
integer sendbuf(sendcount)
integer sendtype

ierror = MPI_SUCCESS

if ( sendtype == mpi_double_precision ) then
  sendbuf(1:sendcount) = recvbuf(:sendcount,1)
else if ( sendtype == mpi_integer ) then
  sendbuf(1:sendcount) = recvbuf(:sendcount,1)
else if ( sendtype == mpi_real ) then
  sendbuf(1:sendcount) = recvbuf(:sendcount,1)
else
  ierror = MPI_FAILURE
end if

end


subroutine mpi_allgatherv ( sendbuf, sendcount, sendtype, recvbuf, &
  recvcounts, displs, recvtype, comm, ierror )

!! MPI_ALLGATHERV gathers data from all the processes in a communicator.
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
!    Input, SENDTYPE SENDBUF(SENDCOUNT), the data to be sent.
!
!    Inout, integer SENDCOUNT, the number of data items being sent.
!
!    Input, integer SENDTYPE, the datatype of the data being sent.
!
!    Output, RECVTYPE RECVBUF(*), the buffer to store the received data.
!
!    Input, integer RECVCOUNTS(0:GROUP_SIZE-1), the number of data items to be
!    received from each process.
!
!    Input, integer DISPLS(0:GROUP_SIZE-1), the I-th element is the displacement
!    in RECVBUF at which to place the input from process I.
!
!    Input, integer RECVTYPE, the datatype of the data being received.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


  integer sendcount

  integer comm
  integer displs(0:*)
  integer ierror
  integer nrecv
  integer recvbuf(*)
  integer recvcounts(0:*)
  integer recvtype
  integer sendbuf(sendcount)
  integer sendtype

  ierror = MPI_SUCCESS

  if ( sendtype == mpi_double_precision ) then
    sendbuf(1:recvcounts(0)) = recvbuf(displs(0))
  else if ( sendtype == mpi_integer ) then
    sendbuf(1:recvcounts(0)) = recvbuf(displs(0))
  else if ( sendtype == mpi_real ) then
    sendbuf(1:recvcounts(0)) = recvbuf(displs(0))
  else
    ierror = MPI_FAILURE
  end if

  return
end

end module gather
