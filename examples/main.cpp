#include <iostream>

#include "mpi.h"

int main(){
int id, Nmpi;
int ierr = MPI_Init (NULL, NULL );

if ( ierr != 0 )
{
  std::cerr << "MPI_Init fail." << std::endl;
  return EXIT_FAILURE;
}

ierr = MPI_Comm_size(MPI_COMM_WORLD, &Nmpi);

ierr = MPI_Comm_rank(MPI_COMM_WORLD, &id);

if(Nmpi != 1) {
  std::cerr << "MPI_Comm_size != 1." << std::endl;
  return EXIT_FAILURE;
}

if(id != 0) {
  std::cerr << "MPI_Comm_rank != 0." << std::endl;
  return EXIT_FAILURE;
}

ierr = MPI_Finalize();
if (ierr != 0 )
{
  std::cerr << "MPI_Finalize fail." << std::endl;
  return EXIT_FAILURE;
}

return EXIT_SUCCESS;
}
