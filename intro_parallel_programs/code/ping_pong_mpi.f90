program ping_pong_mpi
!-------------------------------------
!Program: ping_pong_mpi
!Author: Owen Morehead
!Description: 
!   Sends data backwards and forwards between two processors.
!-------------------------------------   

implicit none
include "mpif.h"

!MPI related variables
integer :: myid, numprocs, ierr

!request handles
integer :: s_request_ping, r_request_ping, s_request_pong, r_request_pong
integer :: request_ping, request_pong

!status handles
integer :: status_ping(MPI_STATUS_SIZE), status_pong(MPI_STATUS_SIZE)

!processor ID's
integer :: ping, pong

!other variables
integer :: buffer, bufferlength, tag, hits, win_stat


call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

win_stat = 12
hits = 0

buffer = 0
bufferlength = 1
tag = 1234

ping = 0             !ping processor
pong = numprocs - 1  !pong processor

!-- either processor can start the round --
if ((myid .eq. ping) .or. (myid .eq. pong)) then

   do while (hits .lt. win_stat)

      !--- if ping ---
      if (myid .eq. ping) then

         buffer = buffer + 1
         !send non-blocking message (integer) to pong:
         call mpi_isend(buffer, bufferlength, MPI_INTEGER, pong, tag, MPI_COMM_WORLD, &
              &         request_ping, ierr)

         !ping recieve message (integer) back from pong:
         call mpi_irecv(buffer, bufferlength, MPI_INTEGER, pong, tag, MPI_COMM_WORLD, &
              &         request_ping, ierr)

         !wait for message recieve to complete
         call mpi_wait(request_ping, status_ping, ierr)
         print*, "Message from Ping to Pong: ", buffer

        !update hit tally
         hits = hits + 1

      !--- if pong ---
      else

         buffer = buffer + 1
         call mpi_isend(buffer, bufferlength, MPI_INTEGER, ping, tag, MPI_COMM_WORLD, &
              &         request_pong, ierr)

         !recieve non-blocking message (integer) from ping: 
         call mpi_irecv(buffer, bufferlength, MPI_INTEGER, ping, tag, MPI_COMM_WORLD, &
              &         request_pong, ierr)

         !wait for send to complete
         call mpi_wait(request_pong, status_pong, ierr)
         print*, "Message from Pong to Ping: ", buffer

         !update hit tally
         hits = hits + 1

      end if

   end do

   if (myid .eq. ping) then
      print*, "Ping-Pong Game Over. ", hits, " total rallies achieved."
   end if

end if

call MPI_FINALIZE(ierr)

stop

end program ping_pong_mpi
