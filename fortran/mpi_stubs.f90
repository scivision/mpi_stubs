module mpi

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit, real64

use mpi_types
use gather, only : mpi_allgather, mpi_allgatherv
use reduce, only : mpi_reduce_scatter, mpi_reduce, mpi_allreduce

implicit none
public

contains


subroutine mpi_get_library_version(version, resultlen, ierror)

character(*), intent(out) :: version
integer, intent(out) :: resultlen, ierror

ierror = mpi_success
resultlen = 1
version = '2'

end subroutine mpi_get_library_version


subroutine mpi_abort ( comm, errorcode, ierror )

!! MPI_ABORT shuts down the processes in a given communicator.
!
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Input, integer ERRORCODE, the error code to be returned.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer, intent(in) :: comm, errorcode
integer, intent(out) :: ierror

ierror = MPI_SUCCESS

write ( stderr, '(/, a, i12)' ) 'MPI_ABORT: Shut down with error code = ', errorcode

error stop errorcode
end subroutine mpi_abort


subroutine mpi_barrier ( comm, ierror )

!! MPI_BARRIER forces processes within a communicator to wait together.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer comm
integer ierror

ierror = MPI_SUCCESS

end


subroutine mpi_bcast ( data, n, datatype, node, comm, ierror )

!! MPI_BCAST broadcasts data from one process to all others.
!
!  Discussion:
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, datatype DATA(N), the data to be broadcast.
!
!    Input, integer N, the number of items of data.
!
!    Input, integer DATATYPE, the MPI code for the datatype of the data.
!
!    Input, integer NODE, the rank of the sending process within the
!    given communicator.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer n

integer comm
integer data(n)
integer datatype
integer ierror
integer node

ierror = MPI_SUCCESS

end


subroutine mpi_bsend ( data, n, datatype, iproc, itag, comm, ierror )

!! MPI_BSEND sends data from one process to another, using buffering.
!
!  Discussion:
!
!    Warn against sending message to self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, datatype DATA(N), the data to be sent.
!
!    Input, integer N, the number of data items to send.
!
!    Input, integer DATAYTPE, the MPI code for the datatype.
!
!    Input, integer IPROC, the rank of the process within the communicator
!    that is to receive the message.
!
!    Input, integer ITAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer n

integer comm
integer data(n)
integer datatype
integer ierror
integer iproc
integer itag

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_BSEND - Error: Should not send message to self.'

end


subroutine mpi_cart_create ( comm_old, ndims, dims, periods, reorder, comm_cart, &
  ierror )

!! MPI_CART_CREATE creates a communicator for a Cartesian topology.
!
!  Parameters:
!
!    Input, integer COMM_OLD, the MPI communicator.
!
!    Input, integer NDIMS, the number of dimensions in the Cartesian grid.
!
!    Input, integer DIMS(NDIMS), the number of processes in each dimension.
!
!    Input, logical PERIODS(NDIMS), is TRUE if the grid is periodic in
!    each dimension.
!
!    Input, logical REORDER, is TRUE if ranking may be reordered.
!
!    Output, integer COMM_CART, the new Cartesian communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer ndims

integer comm_cart
integer comm_old
integer dims(ndims)
integer ierror
logical periods(*)
logical reorder

ierror = MPI_SUCCESS
end


subroutine mpi_cart_get ( comm, ndims, dims, periods, coords, ierror )

!! MPI_CART_GET returns the "Cartesian coordinates" of the calling process.
!
!  Discussion:
!
!    Set all coordinates to 0.
!
!  Parameters:
!
!    Input, integer COMM, the MPI Cartesian communicator.
!
!    Output, integer NDIMS, the number of dimensions in the Cartesian grid.
!
!    Output, integer DIMS(NDIMS), the number of processes in each dimension.
!
!    Output, logical PERIODS(NDIMS), is TRUE if the grid is periodic in
!    each dimension.
!
!    Output, integer COORDS(NDIMS), coordinates of the calling process
!    as part of the Cartesian communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer ndims

integer comm
integer coords(*)
integer dims(*)
integer ierror
logical periods(*)

ierror = MPI_SUCCESS

coords(1:ndims) = 0
end


subroutine mpi_cart_shift ( comm, direction, disp, rank_source, rank_dest, &
  ierror )

!! MPI_CART_SHIFT finds the destination and source for Cartesian shifts.
!
!  Discussion:
!
!    Set ISOURCE = IDEST = SELF = 0.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Input, integer DIRECTION, the dimension to be shifted.
!
!    Input, integer DISP, the displacement.  Positive is upward, negative
!    is downward.
!
!    Output, integer RANK_SOURCE, the rank of the source process.
!
!    Output, integer RANK_DEST, the rank of the destination process.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer comm
integer direction
integer disp
integer ierror
integer rank_dest
integer rank_source

ierror = MPI_SUCCESS
rank_source = 0
rank_dest = 0

end


subroutine mpi_comm_dup ( comm, comm_out, ierror )

!! MPI_COMM_DUP duplicates a communicator.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer COMM_OUT, a new MPI communicator which duplicates COMM.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer comm
integer comm_out
integer ierror

ierror = MPI_SUCCESS

end


subroutine mpi_comm_free ( comm, ierror )

!! MPI_COMM_FREE "frees" a communicator.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer comm
integer ierror

ierror = MPI_SUCCESS

end


subroutine mpi_comm_rank ( comm, me, ierror )

!! MPI_COMM_RANK reports the rank of the calling process.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer ME, the rank of the calling process within the
!    given communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.


integer, intent(in) :: comm
integer, intent(out) :: me, ierror

ierror = MPI_SUCCESS
me = 0

end subroutine mpi_comm_rank


subroutine mpi_comm_size ( comm, nprocs, ierror )

!! MPI_COMM_SIZE reports the number of processes in a communicator.
!
!  Discussion:
!
!    The routine simply returns NPROCS = 1.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer NPROCS, the number of processes in the communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.


integer, intent(in) :: comm
integer, intent(out) :: nprocs, ierror

ierror = MPI_SUCCESS
nprocs = 1

end subroutine mpi_comm_size


subroutine mpi_comm_split ( comm, icolor, ikey, comm_new, ierror )

!! MPI_COMM_SPLIT splits up a communicator based on a key.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer comm
integer comm_new
integer icolor
integer ierror
integer ikey

ierror = MPI_SUCCESS

end


subroutine mpi_finalize( ierror )
!! MPI_FINALIZE shuts down the MPI library.
integer, intent(out) :: ierror
ierror = MPI_SUCCESS
end subroutine mpi_finalize


subroutine mpi_get_count ( status, datatype, icount, ierror )

!! MPI_GET_COUNT reports the number of items actually transmitted.
!
!  Discussion:
!
!    Warn against querying message from self, since no data copy is done.
!
!  Parameters:
!
!    Input, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Input, integer DATATYPE, the datatype of the data.
!
!    Output, integer ICOUNT, the number of data items transmitted.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer datatype
integer icount
integer ierror
integer status(mpi_status_size)

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_GET_COUNT - Error:  Should not query message from self.'

end


subroutine mpi_init ( ierror )

!! MPI_INIT initializes the MPI library.
!
!  Parameters:
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer ierror

ierror = MPI_SUCCESS

end


subroutine mpi_irecv ( data, n, datatype, iproc, itag, comm, irequest, ierror )

!! MPI_IRECV performs an immediate receive of data from another process.
!
!  Discussion:
!
!    For an immediate or nonblocking receive, the call to mpi_irecv begins
!    a receive operation, but program execution may proceed to the next
!    statement without waiting for confirmation that the receive has
!    been completed.  It is up to the user to issue an appropriate
!    statement later, such as a call to MPI_WAIT, with a copy of the
!    value of IREQUEST, to verify that the receive has completed.
!
!    Warn against receiving message from self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Output, DATATYPE DATA(N), a buffer which will contain the data.
!
!    Input, integer N, the number of items expected.
!
!    Input, integer DATATYPE, the MPI datatype of the data.
!
!    Input, integer IPROC, the MPI process from which the data is to
!    be received.
!
!    Input, integer ITAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IREQUEST, the request handle.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer n

integer comm
integer data(n)
integer datatype
integer ierror
integer iproc
integer irequest
integer itag

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_IRECV - Error: Should not "recv" message from self.'

end


subroutine mpi_isend ( data, n, datatype, iproc, itag, comm, request, ierror )

!! MPI_ISEND sends data to another process using nonblocking transmission.
!
!  Discussion:
!
!    Warn against sending message to self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, datatype DATA(N), the data to be sent.
!
!    Input, integer N, the number of data items to send.
!
!    Input, integer DATAYTPE, the MPI code for the datatype.
!
!    Input, integer IPROC, the rank of the process within the communicator
!    that is to receive the message.
!
!    Input, integer ITAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer REQUEST, a handle.  To determine if the data has been
!    received yet, call MPI_Test or MPI_Wait, including the value of REQUEST.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer n

integer comm
integer data(n)
integer datatype
integer ierror
integer iproc
integer itag
integer request

request = 0
ierror = MPI_FAILURE

write ( *, '(a)' ) ' '
write ( *, '(a)' ) 'MPI_ISEND - Error!'
write ( *, '(a)' )  '  Should not send message to self.'

end

subroutine mpi_recv ( data, n, datatype, iproc, itag, comm, status, ierror )

!! MPI_RECV receives data from another process within a communicator.
!
!  Discussion:
!
!    Warn against receiving message from self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Output, DATATYPE DATA(N), a buffer which will contain the data.
!
!    Input, integer N, the number of items expected.
!
!    Input, integer DATATYPE, the MPI datatype of the data.
!
!    Input, integer IPROC, the MPI process from which the data is to
!    be received.
!
!    Input, integer ITAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.


integer, intent(in) :: n, comm, datatype, iproc, itag

type(*), intent(inout) :: data(..)
integer, intent(out) :: ierror, status(mpi_status_size)

ierror = MPI_FAILURE

write ( *, '(/,a)' ) 'MPI_RECV - Error: Should not "recv" message from self.'

end


subroutine mpi_rsend ( buf, count, datatype, dest, tag, comm, ierror )

!! MPI_RSEND "ready sends" data from one process to another.
!
!  Discussion:
!
!    Warn against sending message to self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, integer BUF(*), the address of the send buffer.
!
!    Input, integer COUNT, the number of elements in the send buffer.
!
!    Input, integer DATATYPE, the type of the data to be send.
!
!    Input, integer DEST, the destination process.
!
!    Input, integer TAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer count

integer buf(count)
integer comm
integer datatype
integer dest
integer ierror
integer tag

ierror = MPI_FAILURE

write ( *, '(a)' ) ' '
write ( *, '(a)' ) 'MPI_RSEND - Error!'
write ( *, '(a)' ) '  Should not send message to self.'

end


subroutine mpi_send ( data, n, datatype, iproc, itag, comm, ierror )

!! MPI_SEND sends data from one process to another.
!
!  Discussion:
!
!    Warn against sending message to self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!
!  Parameters:
!
!    Input, datatype DATA(N), the data to be sent.
!
!    Input, integer N, the number of data items to send.
!
!    Input, integer DATAYTPE, the MPI code for the datatype.
!
!    Input, integer IPROC, the rank of the process within the communicator
!    that is to receive the message.
!
!    Input, integer ITAG, a tag for the message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.

class(*) :: data(n)
integer, intent(in) :: n, datatype, iproc, itag, comm
integer, intent(out) :: ierror

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_SEND - Error:  Should not send message to self.'

end subroutine mpi_send


subroutine mpi_sendrecv ( data_send, n_send, type_send, dest, tag_send, &
  data_recv, n_recv, type_recv, source, tag_recv, comm, status, ierror )

!! MPI_SENDRECV sends and receives data.
!
!  Discussion:
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Parameters:
!
!    Input, datatype DATA_SEND(N_SEND), the data to be sent.
!
!    Input, integer N_SEND, the number of data items to send.
!
!    Input, integer TYPE_SEND, the MPI code for the datatype sent.
!
!    Input, integer DEST, the rank of the process within the communicator
!    that is to receive the message.
!
!    Input, integer TAG_SEND, a tag for the sent message.
!
!    Input, datatype DATA_RECV(N_RECV), the data to be received.
!
!    Input, integer N_RECV, the number of data items to receive.
!
!    Input, integer TYPE_RECV, the MPI code for the datatype received.
!
!    Input, integer SOURCE, the rank of the process within the communicator
!    that is to send the message.
!
!    Input, integer TAG_RECV, a tag for the received message.
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!


integer, intent(in) :: n_recv, n_send, comm, dest, source, tag_recv, tag_send, type_recv, type_send
integer, intent(out) :: ierror, status(mpi_status_size)

type(*), intent(inout) :: data_recv(..)
type(*), intent(in) :: data_send(..)

ierror = MPI_FAILURE

write ( stderr, '(/,a)' )'MPI_SENDRECV - Error: Should not send message to self.'

end


subroutine mpi_wait ( request, status, ierror )

!! MPI_WAIT waits for an I/O request to complete.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Parameters:
!
!    Input, integer REQUEST, an MPI request.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.

integer ierror
integer request
integer status(mpi_status_size)

ierror = MPI_FAILURE

write ( stderr, '(/,a)') 'MPI_WAIT - Error: Should not wait on message from self.'

end


subroutine mpi_waitall ( icount, irequest, status, ierror )

!! MPI_WAITALL waits until all I/O requests have completed.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Parameters:
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!

integer icount
integer ierror
integer irequest
integer status(mpi_status_size)

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_WAITALL - Error: Should not wait on message from self.'

end


subroutine mpi_waitany ( icount, requests, index, status, ierror )


!! MPI_WAITANY waits until one I/O requests has completed.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Parameters:
!
!    Input, integer REQUESTS(ICOUNT), an array of requests.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.


integer requests(*)
integer icount
integer ierror
integer index
integer status(mpi_status_size)

ierror = MPI_FAILURE

write ( stderr, '(/,a)' ) 'MPI_WAITANY - Error:  Should not wait on message from self.'

end


real(real64) function mpi_wtick ( )

!! MPI_WTICK returns the number of seconds per clock tick.
!
!  Discussion:
!
!    The value returned here is simply a dummy value.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MPI_WTICK, the number of seconds per clock tick.
!

mpi_wtick = 1

end function mpi_wtick


real(real64) function mpi_wtime ( )

!! MPI_WTIME returns the elapsed wall clock time.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MPI_WTIME, the elapsed wall clock time.
!

integer count
integer count_max
integer count_rate

call system_clock ( count, count_rate, count_max )

mpi_wtime = real ( count, kind = 8 ) / real ( count_rate, kind = 8 )

end function mpi_wtime


subroutine timestring ( string )

!! TIMESTRING writes the current YMDHMS date into a string.
!
!  Example:
!
!    STRING = '31 May 2001   9:45:54.872 AM'
!
!  Parameters:
!
!    Output, character ( len = * ) STRING, contains the date information.
!    A character length of 40 should always be sufficient.
!

character ( len = 8 ) ampm
integer d
integer h
integer m
integer mm
character ( len = 9 ), parameter :: month(12) = [ &
  'January  ', 'February ', 'March    ', 'April    ', &
  'May      ', 'June     ', 'July     ', 'August   ', &
  'September', 'October  ', 'November ', 'December ' ]
integer n
integer s
character ( len = * ) string
integer values(8)
integer y

call date_and_time ( values = values )

y = values(1)
m = values(2)
d = values(3)
h = values(5)
n = values(6)
s = values(7)
mm = values(8)

if ( h < 12 ) then
  ampm = 'AM'
else if ( h == 12 ) then
  if ( n == 0 .and. s == 0 ) then
    ampm = 'Noon'
  else
    ampm = 'PM'
  end if
else
  h = h - 12
  if ( h < 12 ) then
    ampm = 'PM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Midnight'
    else
      ampm = 'AM'
    end if
  end if
end if

write ( string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

end

end module mpi
