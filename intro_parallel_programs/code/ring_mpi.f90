program ring_mpi
!-------------------------------------
!Program: ring_mpi
!Author: Owen Morehead
!Description: 
!   Sends data around a ring of N processors.
!   Shifts all data to the right or left, specified by user.
!-------------------------------------   

implicit none
include "mpif.h"

!MPI related variables
integer :: myid, numprocs, count, ierr, request, tag, status(MPI_STATUS_SIZE)

!other variables
integer :: buffer, bufferlength, targetid, RL_shift, broad_buff, i 


call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

buffer = myid
count = 1

tag = 1234

!--- If processor id == 0 (root processor with 'data' to shift) ---
if (myid .eq. 0) then

   !ask user which direction to shift
   print*, "Shift data to Right or Left? Enter ['0' for Right] or ['1' for Left]"
   read(*,*) RL_shift

   broad_buff = RL_shift

end if

!--- Make all processors synchronize so they all can obtain RL_shift variable ---
call MPI_BARRIER(MPI_COMM_WORLD, ierr)

!--- Broadcast RL_shift direction variable (integer) to all processors from root procesor ---
call MPI_BCAST(broad_buff, count, MPI_INT, 0, MPI_COMM_WORLD, ierr)

RL_shift = broad_buff

do i = 1,numprocs

   !--- Now shift data to left or right N times ---

   if (RL_shift .eq. 0) then
      targetid = MOD(myid + 1, numprocs)

   else if (RL_shift .eq. 1) then
      targetid = MOD(myid - 1 + numprocs, numprocs)
   else
      print*, "Neither 0 or 1 entered for R-L shift value. Program terminating."
      stop
   end if

   !--- Send data (processor id) to targetid, non-blocking -> MPI_ISEND() ---
   !--- wait for message from neighboring processor on other side, blocking -> MPI_RECV() ---

   call MPI_ISEND(buffer, count, MPI_INT, targetid, tag, MPI_COMM_WORLD, request, ierr)
   call MPI_RECV(buffer, count, MPI_INT, MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, status, ierr)

   print*, "Processor number ", myid, " sent data ", buffer, " to processor number ", targetid
end do

call MPI_FINALIZE(ierr)

stop

end program ring_mpi

