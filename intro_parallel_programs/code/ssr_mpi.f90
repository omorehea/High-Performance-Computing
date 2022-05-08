program ssr_mpi
!-------------------------------------
!Program: ssr_mpi
!Author: Owen Morehead
!Description: 
!   Sends and print out data from one processor to another using MPI send and recieve.
!-------------------------------------   

implicit none
include "mpif.h"

!MPI related variables
integer :: myid, numprocs, ierr
integer :: tag, source, destination, buffsize
real :: buffer(3)
integer :: stat(MPI_STATUS_SIZE)


call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

buffer = (/1.0,2.0,3.0/)
tag = 1234
source = 0
destination = 1
buffsize = 3

if (myid .eq. source) then
   call MPI_SEND(buffer, buffsize, MPI_REAL, destination, tag, &
        &        MPI_COMM_WORLD, ierr)
   print*, 'Processor: ',myid, 'sent ', buffer
end if

if (myid .eq. destination) then
    call MPI_RECV(buffer, buffsize, MPI_REAL, source, tag, &
         &        MPI_COMM_WORLD, stat, ierr)
    print*, 'Processor: ',myid, 'received ', buffer
 end if


call MPI_FINALIZE(ierr)


end program ssr_mpi
