program ping_pong_simp_mpi
!-------------------------------------
!Program: ping_pong_mpi
!Author: Owen Morehead
!Description: 
!   Sends data backwards and forwards between two processors
!   using MPI send() and recv()
!-------------------------------------   

implicit none
include "mpif.h"

!MPI related variables
integer :: myid, numprocs, ierr

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

      !---- if ping ----
      if (myid .eq. ping) then

         !--- send blocking message (integer) to pong ---
         buffer = buffer + 1
         call MPI_SEND(buffer, bufferlength, MPI_INTEGER, pong, tag, MPI_COMM_WORLD, &
              &        ierr)
         
         print*, 'Ping sent message to Pong: ', buffer


        !update hit tally
         hits = hits + 1

         !--- ping recieve message (integer) back from pong ---
         call MPI_RECV(buffer, bufferlength, MPI_INTEGER, pong, tag, MPI_COMM_WORLD, &
              &        status_ping , ierr)
         print*, 'Ping recieved message from Pong: ',buffer

      !---- if not ping -> if pong ----
      else

         !--- pong recieve message (integer) from ping ---                                                                                          
         call MPI_RECV(buffer, bufferlength, MPI_INTEGER, ping, tag, MPI_COMM_WORLD, &
              &        status_ping , ierr)
         print*, 'Pong recieved message from Ping: ',buffer

         !--- send blocking message (integer) back to ping ---
!         buffer = buffer + 1
         call MPI_SEND(buffer, bufferlength, MPI_INTEGER, ping, tag, MPI_COMM_WORLD, &
              &        ierr)
         
         print*, 'Pong sent message to Ping: ',buffer
         

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

end program ping_pong_simp_mpi
