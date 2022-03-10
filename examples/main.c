#include <stdio.h>
#include "mpi.h"

int main(void){
int id, Nmpi;
int ierr = MPI_Init (NULL, NULL );

if ( ierr != 0 )
{
  fprintf(stderr, "MPI_Init fail.\n");
  return 1;
}

ierr = MPI_Comm_size(MPI_COMM_WORLD, &Nmpi);

ierr = MPI_Comm_rank(MPI_COMM_WORLD, &id);

if(Nmpi != 1) {
  fprintf(stderr, "MPI_Comm_size != 1.\n");
  return 1;
}

if(id != 0) {
  fprintf(stderr, "MPI_Comm_rank != 0.\n");
  return 1;
}

ierr = MPI_Finalize();
if (ierr != 0 )
{
  fprintf(stderr, "MPI_Finalize fail.\n");
  return 1;
}

return 0;
}
