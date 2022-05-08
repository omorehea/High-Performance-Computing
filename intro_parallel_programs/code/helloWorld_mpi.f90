program helloWorld_mpi
!-------------------------------------
!Program: helloWorld_mpi
!Author: Owen Morehead
!Description: 
!   Writes out "Hello" from each processor,
!   stating the processor rank and size of the comm world.
!-------------------------------------   

implicit none
include "mpif.h"


!MPI related variables
integer :: myid, numprocs, ierr

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

print*, "Hello from processor", myid, "out of", numprocs


call MPI_FINALIZE(ierr)





end program helloWorld_mpi
