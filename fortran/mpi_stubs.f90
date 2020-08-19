subroutine mpi_abort ( comm, errorcode, ierror )

!*****************************************************************************80
!
!! MPI_ABORT shuts down the processes in a given communicator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Input, integer ERRORCODE, the error code to be returned.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer errorcode
  integer ierror

  ierror = MPI_SUCCESS

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_ABORT:'
  write ( *, '(a,i12)' ) '  Shut down with error code = ', errorcode

  stop
end
subroutine mpi_allgather ( sendbuf, sendcount, sendtype, recvbuf, recvcount, &
  recvtype, comm, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
!
  implicit none

  include "mpi_stubs_f90.h"

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
    call mpi_copy_double_precision ( sendbuf, recvbuf(:,1), sendcount, ierror )
  else if ( sendtype == mpi_integer ) then
    call mpi_copy_integer ( sendbuf, recvbuf(:,1), sendcount, ierror )
  else if ( sendtype == mpi_real ) then
    call mpi_copy_real ( sendbuf, recvbuf(:,1), sendcount, ierror )
  else
    ierror = MPI_FAILURE
  end if

  return
end
subroutine mpi_allgatherv ( sendbuf, sendcount, sendtype, recvbuf, &
  recvcounts, displs, recvtype, comm, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

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
    call mpi_copy_double_precision ( &
      sendbuf, recvbuf(displs(0)), recvcounts(0), ierror )
  else if ( sendtype == mpi_integer ) then
    call mpi_copy_integer ( &
      sendbuf, recvbuf(displs(0)), recvcounts(0), ierror )
  else if ( sendtype == mpi_real ) then
    call mpi_copy_real ( &
      sendbuf, recvbuf(displs(0)), recvcounts(0), ierror )
  else
    ierror = MPI_FAILURE
  end if

  return
end
subroutine mpi_allreduce ( data1, data2, n, datatype, operation, comm, ierror )

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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data1(n)
  integer data2(n)
  integer datatype
  integer ierror
  integer operation

  ierror = MPI_SUCCESS

  if ( datatype == mpi_double_precision ) then

    call mpi_reduce_double_precision ( data1, data2, n, operation, ierror )

  else if ( datatype == mpi_integer ) then

    call mpi_reduce_integer ( data1, data2, n, operation, ierror )

  else if ( datatype == mpi_real ) then

    call mpi_reduce_real ( data1, data2, n, operation, ierror )

  else

    ierror = MPI_FAILURE

  end if

  return
end
subroutine mpi_barrier ( comm, ierror )

!*****************************************************************************80
!
!! MPI_BARRIER forces processes within a communicator to wait together.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer ierror

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_bcast ( data, n, datatype, node, comm, ierror )

!*****************************************************************************80
!
!! MPI_BCAST broadcasts data from one process to all others.
!
!  Discussion:
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data(n)
  integer datatype
  integer ierror
  integer node

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_bsend ( data, n, datatype, iproc, itag, comm, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data(n)
  integer datatype
  integer ierror
  integer iproc
  integer itag

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_BSEND - Error!'
  write ( *, '(a)' )  '  Should not send message to self.'

  return
 end
subroutine mpi_cart_create ( comm_old, ndims, dims, periods, reorder, comm_cart, &
  ierror )

!*****************************************************************************80
!
!! MPI_CART_CREATE creates a communicator for a Cartesian topology.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer ndims

  integer comm_cart
  integer comm_old
  integer dims(ndims)
  integer ierror
  logical periods(*)
  logical reorder

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_cart_get ( comm, ndims, dims, periods, coords, ierror )

!*****************************************************************************80
!
!! MPI_CART_GET returns the "Cartesian coordinates" of the calling process.
!
!  Discussion:
!
!    Set all coordinates to 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer ndims

  integer comm
  integer coords(*)
  integer dims(*)
  integer ierror
  logical periods(*)

  ierror = MPI_SUCCESS

  coords(1:ndims) = 0

  return
end
subroutine mpi_cart_shift ( comm, direction, disp, rank_source, rank_dest, &
  ierror )

!*****************************************************************************80
!
!! MPI_CART_SHIFT finds the destination and source for Cartesian shifts.
!
!  Discussion:
!
!    Set ISOURCE = IDEST = SELF = 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer direction
  integer disp
  integer ierror
  integer rank_dest
  integer rank_source

  ierror = MPI_SUCCESS
  rank_source = 0
  rank_dest = 0

  return
end
subroutine mpi_comm_dup ( comm, comm_out, ierror )

!*****************************************************************************80
!
!! MPI_COMM_DUP duplicates a communicator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer COMM_OUT, a new MPI communicator which duplicates COMM.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer comm_out
  integer ierror

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_comm_free ( comm, ierror )

!*****************************************************************************80
!
!! MPI_COMM_FREE "frees" a communicator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer ierror

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_comm_rank ( comm, me, ierror )

!*****************************************************************************80
!
!! MPI_COMM_RANK reports the rank of the calling process.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer ME, the rank of the calling process within the
!    given communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer ierror
  integer me

  ierror = MPI_SUCCESS
  me = 0

  return
end
subroutine mpi_comm_size ( comm, nprocs, ierror )

!*****************************************************************************80
!
!! MPI_COMM_SIZE reports the number of processes in a communicator.
!
!  Discussion:
!
!    The routine simply returns NPROCS = 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer NPROCS, the number of processes in the communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer ierror
  integer nprocs

  ierror = MPI_SUCCESS
  nprocs = 1

  return
end
subroutine mpi_comm_split ( comm, icolor, ikey, comm_new, ierror )

!*****************************************************************************80
!
!! MPI_COMM_SPLIT splits up a communicator based on a key.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer COMM, the MPI communicator.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer comm
  integer comm_new
  integer icolor
  integer ierror
  integer ikey

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_copy_double_precision ( data1, data2, n, ierror )

!*****************************************************************************80
!
!! MPI_COPY_DOUBLE copies a double precision vector.
!
!  Discussion:
!
!    This routine is not part of the MPI standard.  However, it is
!    needed by other routines which do emulate standard MPI routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, double precision DATA1(N), the data to be copied.
!
!    Output, double precision DATA2(N), the copied data.
!
!    Input, integer N, the number of items of data.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  double precision data1(n)
  double precision data2(n)
  integer ierror

  ierror = MPI_SUCCESS

  data2(1:n) = data1(1:n)

  return
end
subroutine mpi_copy_integer ( data1, data2, n, ierror )

!*****************************************************************************80
!
!! MPI_COPY_INTEGER copies an integer vector.
!
!  Discussion:
!
!    This routine is not part of the MPI standard.  However, it is
!    needed by other routines which do emulate standard MPI routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer DATA1(N), the data to be copied.
!
!    Output, integer DATA2(N), the copied data.
!
!    Input, integer N, the number of items of data.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer data1(n)
  integer data2(n)
  integer ierror

  ierror = MPI_SUCCESS

  data2(1:n) = data1(1:n)

  return
end
subroutine mpi_copy_real ( data1, data2, n, ierror )

!*****************************************************************************80
!
!! MPI_COPY_REAL copies a real vector.
!
!  Discussion:
!
!    This routine is not part of the MPI standard.  However, it is
!    needed by other routines which do emulate standard MPI routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, real DATA1(N), the data to be copied.
!
!    Output, real DATA2(N), the copied data.
!
!    Input, integer N, the number of items of data.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  real data1(n)
  real data2(n)
  integer ierror

  ierror = MPI_SUCCESS

  data2(1:n) = data1(1:n)

  return
end
subroutine mpi_finalize ( ierror )

!*****************************************************************************80
!
!! MPI_FINALIZE shuts down the MPI library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer ierror

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_get_count ( status, datatype, icount, ierror )

!*****************************************************************************80
!
!! MPI_GET_COUNT reports the number of items actually transmitted.
!
!  Discussion:
!
!    Warn against querying message from self, since no data copy is done.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer datatype
  integer icount
  integer ierror
  integer status(mpi_status_size)

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_GET_COUNT - Error!'
  write ( *, '(a)' ) '  Should not query message from self.'

  return
end
subroutine mpi_init ( ierror )

!*****************************************************************************80
!
!! MPI_INIT initializes the MPI library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer ierror

  ierror = MPI_SUCCESS

  return
end
subroutine mpi_irecv ( data, n, datatype, iproc, itag, comm, irequest, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data(n)
  integer datatype
  integer ierror
  integer iproc
  integer irequest
  integer itag

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_IRECV - Error!'
  write ( *, '(a)' ) '  Should not "recv" message from self.'

  return
end
subroutine mpi_isend ( data, n, datatype, iproc, itag, comm, request, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

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

  return
end
subroutine mpi_recv ( data, n, datatype, iproc, itag, comm, status, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data(n)
  integer datatype
  integer ierror
  integer iproc
  integer itag
  integer status(mpi_status_size)

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_RECV - Error!'
  write ( *, '(a)' ) '  Should not "recv" message from self.'

  return
end
subroutine mpi_reduce ( data1, data2, n, datatype, operation, receiver, &
  comm, ierror )

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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data1(n)
  integer data2(n)
  integer datatype
  integer ierror
  integer operation
  integer receiver

  if ( datatype == mpi_double_precision ) then

    call mpi_reduce_double_precision ( data1, data2, n, operation, ierror )

  else if ( datatype == mpi_integer ) then

    call mpi_reduce_integer ( data1, data2, n, operation, ierror )

  else if ( datatype == mpi_real ) then

    call mpi_reduce_real ( data1, data2, n, operation, ierror )

  else

    ierror = MPI_FAILURE

  end if

  return
end
subroutine mpi_reduce_double_precision ( data1, data2, n, operation, ierror )

!*****************************************************************************80
!
!! MPI_REDUCE_DOUBLE_PRECISION: reduction operation on double precision values.
!
!  Discussion:
!
!    The reduction operations are sum, maximum, minimum, product.
!
!    Thanks to Simppa Akaslompolo for correcting this routine!
!    11 January 2012.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, double precision DATA1(N), the data to be processed.
!
!    Output, double precision DATA2(N), the value of the reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  double precision data1(n)
  double precision data2(n)
  integer ierror
  integer operation

  ierror = MPI_SUCCESS

  if ( operation == mpi_max ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_min ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_product ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_sum ) then

    data2(1:n) = data1(1:n)

  else

    ierror = MPI_FAILURE

  end if

  return
end
subroutine mpi_reduce_integer ( data1, data2, n, operation, ierror )

!*****************************************************************************80
!
!! MPI_REDUCE_INTEGER carries out a reduction operation on integers.
!
!  Discussion:
!
!    The reduction operations are sum, maximum, minimum, product.
!
!    Thanks to Simppa Akaslompolo for correcting this routine!
!    11 January 2012.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer DATA1(N), the data to be processed.
!
!    Output, integer DATA2(N), the value of the reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer data1(n)
  integer data2(n)
  integer ierror
  integer operation

  ierror = MPI_SUCCESS

  if ( operation == mpi_max ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_min ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_product ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_sum ) then

    data2(1:n) = data1(1:n)

  else

    ierror = MPI_FAILURE

  end if

  return
end
subroutine mpi_reduce_real ( data1, data2, n, operation, ierror )

!*****************************************************************************80
!
!! MPI_REDUCE_REAL carries out a reduction operation on reals.
!
!  Discussion:
!
!    The reduction operations are sum, maximum, minimum, product.
!
!    Thanks to Simppa Akaslompolo for correcting this routine!
!    11 January 2012.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, real DATA1(N), the data to be processed.
!
!    Output, real DATA2(N), the value of the reduction operation.
!
!    Input, integer N, the number of items in DATA1.
!
!    Input, integer OPERATION, should have the value of one of the symbolic
!    constants MPI_MAX, MPI_MIN, MPI_PRODUCT or MPI_SUM.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  real data1(n)
  real data2(n)
  integer ierror
  integer operation

  ierror = MPI_SUCCESS

  if ( operation == mpi_max ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_min ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_product ) then

    data2(1:n) = data1(1:n)

  else if ( operation == mpi_sum ) then

    data2(1:n) = data1(1:n)

  else

    ierror = MPI_FAILURE

  end if

  return
end
subroutine mpi_reduce_scatter ( data1, data2, n, datatype, operation, comm, &
  ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
!
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data1(n)
  integer data2(n)
  integer datatype
  integer ierror
  integer operation

  ierror = MPI_SUCCESS

  if ( datatype == mpi_double_precision ) then
    call mpi_copy_double_precision ( data1, data2, n, ierror )
  else if ( datatype == mpi_integer ) then
    call mpi_copy_integer ( data1, data2, n, ierror )
  else if ( datatype == mpi_real ) then
    call mpi_copy_real ( data1, data2, n, ierror )
  else
    ierror = MPI_FAILURE
  end if

  return
end
subroutine mpi_rsend ( buf, count, datatype, dest, tag, comm, ierror )

!*****************************************************************************80
!
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

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

  return
end
subroutine mpi_send ( data, n, datatype, iproc, itag, comm, ierror )

!*****************************************************************************80
!
!! MPI_SEND sends data from one process to another.
!
!  Discussion:
!
!    Warn against sending message to self, since no data copy is done.
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n

  integer comm
  integer data(n)
  integer datatype
  integer ierror
  integer iproc
  integer itag

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_SEND - Error!'
  write ( *, '(a)' )  '  Should not send message to self.'

  return
end
subroutine mpi_sendrecv ( data_send, n_send, type_send, dest, tag_send, &
  data_recv, n_recv, type_recv, source, tag_recv, comm, status, ierror )

!*****************************************************************************80
!
!! MPI_SENDRECV sends and receives data.
!
!  Discussion:
!
!    The data to be transferred can be integer, real, or double precision.
!    In this routine, it is declared and documented as INTEGER type,
!    but using the other types should generally not cause a problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2017
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
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
  implicit none

  include "mpi_stubs_f90.h"

  integer n_recv
  integer n_send

  integer comm
  integer data_recv(n_recv)
  integer data_send(n_send)
  integer dest
  integer ierror
  integer source
  integer status(mpi_status_size)
  integer tag_recv
  integer tag_send
  integer type_recv
  integer type_send

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_SENDRECV - Error!'
  write ( *, '(a)' )  '  Should not send message to self.'

  return
end
subroutine mpi_wait ( request, status, ierror )

!*****************************************************************************80
!
!! MPI_WAIT waits for an I/O request to complete.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer REQUEST, an MPI request.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer ierror
  integer request
  integer status(mpi_status_size)

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_WAIT - Error!'
  write ( *, '(a)' ) '  Should not wait on message from self.'

  return
 end
subroutine mpi_waitall ( icount, irequest, status, ierror )

!*****************************************************************************80
!
!! MPI_WAITALL waits until all I/O requests have completed.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer icount
  integer ierror
  integer irequest
  integer status(mpi_status_size)

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_WAITALL - Error!'
  write ( *, '(a)' ) '  Should not wait on message from self.'

  return
end
subroutine mpi_waitany ( icount, requests, index, status, ierror )

!*****************************************************************************80
!
!! MPI_WAITANY waits until one I/O requests has completed.
!
!  Discussion:
!
!    Warn against waiting on message from self, since no data copy is done.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Input, integer REQUESTS(ICOUNT), an array of requests.
!
!    Output, integer STATUS(MPI_STATUS_SIZE), the MPI message status.
!
!    Output, integer IERROR, is nonzero if an error occurred.
!
  implicit none

  include "mpi_stubs_f90.h"

  integer requests(*)
  integer icount
  integer ierror
  integer index
  integer status(mpi_status_size)

  ierror = MPI_FAILURE

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MPI_WAITANY - Error!'
  write ( *, '(a)' ) '  Should not wait on message from self.'

  return
end
function mpi_wtick ( )

!*****************************************************************************80
!
!! MPI_WTICK returns the number of seconds per clock tick.
!
!  Discussion:
!
!    The value returned here is simply a dummy value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MPI_WTICK, the number of seconds per clock tick.
!
  implicit none

  real ( kind = 8 ) mpi_wtick

  mpi_wtick = 1.0D+00

  return
end
function mpi_wtime ( )

!*****************************************************************************80
!
!! MPI_WTIME returns the elapsed wall clock time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Gropp, Ewing Lusk, Anthony Skjellum,
!    Using MPI: Portable Parallel Programming with the
!    Message-Passing Interface,
!    Second Edition,
!    MIT Press, 1999,
!    ISBN: 0262571323.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MPI_WTIME, the elapsed wall clock time.
!
  implicit none

  integer count
  integer count_max
  integer count_rate
  real ( kind = 8 ) mpi_wtime

  call system_clock ( count, count_rate, count_max )

  mpi_wtime = real ( count, kind = 8 ) / real ( count_rate, kind = 8 )

  return
end
subroutine timestring ( string )

!*****************************************************************************80
!
!! TIMESTRING writes the current YMDHMS date into a string.
!
!  Example:
!
!    STRING = '31 May 2001   9:45:54.872 AM'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) STRING, contains the date information.
!    A character length of 40 should always be sufficient.
!
  implicit none

  character ( len = 8 ) ampm
  integer d
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
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

  return
end
