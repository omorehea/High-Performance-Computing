program latency_pong_mpi
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
integer :: buffer, bufferlength, tag, hits, win_stat, i, j, k, mlen_max, step_len, num_L, iter_count

real(kind=8), allocatable :: message(:)
integer, allocatable :: ivals(:)

!timer variables
real(kind=8) :: wtime_overhead, start, finish, time_diff
real(kind=8), allocatable :: times_array(:), avg_times(:)


!-- to use following variable to evaluate at specific message length values
!-- must change code below in some places.
!-- i = 1,size(Lvals), allocate(ivals(size(Lvals))), MPI_SEND,RECV both need Lvals(i) as message length input
!-- ivals(iter_count) = Lvals(i), lastsly, will want to change name of output file.

!integer :: Lvals(10)
!Lvals = (/10,50,100,500,1000,5000,10000,30000,60000,100000/)

wtime_overhead = 0; start = 0; finish = 0; time_diff = 0


call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

win_stat = 5000 !run the ping-pong loop this many times

!buffer = 0
!bufferlength = 1

tag = 1234

ping = 0             !ping processor
pong = numprocs - 1  !pong processor

iter_count = 0

allocate(times_array(win_stat))
times_array = 0


mlen_max = 100000
step_len = 50
num_L = mlen_max/step_len

mlen_max = mlen_max 

hits = 1



!-- either processor can start the round --
if ((myid .eq. ping) .or. (myid .eq. pong)) then
 
   allocate(ivals(num_L))
   allocate(avg_times(num_L))

   do i = 50,mlen_max,step_len

      allocate(message(i))
      message = 1.d0

   !   print*, "proc id", myid
   !   print*, "total hits: ", hits
   !   print*, "size of message: ", i
   !   print*, "message: ", message


      call MPI_BARRIER(MPI_COMM_WORLD, ierr)

      do while (hits .le. win_stat)

!         print*, "...playing..."

         !---- if ping ----
         if (myid .eq. ping) then

            !--- send blocking message (real) to pong ---
            
            !--- start timer ---

            start = MPI_WTIME()
            call MPI_SEND(message, i, MPI_REAL, pong, tag, MPI_COMM_WORLD, &
                 &        ierr)
            
            ! print*, 'Ping sent message to Pong: ', buffer
        

            !--- ping recieve message (real) back from pong ---
            call MPI_RECV(message, i, MPI_REAL, pong, tag, MPI_COMM_WORLD, &
                 &        status_ping , ierr)
      
            !--- end timer ---
            finish = MPI_WTIME()
            
            time_diff = finish - start
            times_array(hits) = time_diff


            !         print*, "Time for message send/recv: ", time_diff

            !--- update hits tally ---
            hits = hits + 1


            !---- if not ping -> if pong ----
         else

            !--- pong recieve message (real) from ping ---                                                                                          
            call MPI_RECV(message, i, MPI_REAL, ping, tag, MPI_COMM_WORLD, &
                 &        status_ping , ierr)

            !--- send blocking message (real) back to ping ---
      
            call MPI_SEND(message, i, MPI_REAL, ping, tag, MPI_COMM_WORLD, &
                 &        ierr)
         
            !--- update hit tally ---
            hits = hits + 1
         
         end if
         
      end do
 


  
      if (myid .eq. ping) then

         iter_count = iter_count + 1
         
         !--- average all the message times to get accurate result ---
         avg_times(iter_count) = sum(times_array(30:size(times_array)-30))/size(times_array(30:size(times_array)-30))

         ivals(iter_count) = i

      end if

!      call MPI_BARRIER(MPI_COMM_WORLD, ierr)

      hits = 1

      deallocate(message)

   end do


   !--- ping-pong loops are over, now write data to file ---

   if (myid .eq. ping) then

      open(1, file = 'latency_times_and_L_moreL.dat')

      do k = 1, size(avg_times)
         write(1,*) ivals(k), avg_times(k)

      end do

      close(1)

   end if


end if
   
deallocate(times_array, avg_times, ivals)

call MPI_FINALIZE(ierr)

stop

end program latency_pong_mpi
