program pi_mpi
!-------------------------------------
!Program: pi_mpi
!Author: Owen Morehead
!Description: 
!   Solves for pi using the 'dartboard method' in parallel.
!   A circular dartboard on square background has a ratio of the areas: pi(r^2)/(2r)^2 = pi/4
!   If we throw darts randomly at dartboard, examine ratio and therefore estimate pi.
!-------------------------------------   

implicit none
include "mpif.h"

!MPI related variables
integer :: myid, numprocs, ierr, status(MPI_STATUS_SIZE)

integer :: i, iter_max, seed, hits_tally, count, sgn

!array to store all hit tallies, which will be gathered in the end
integer, allocatable :: totals(:)
real :: dist_mag, pi_appx, x, y, radius

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

!--- some variable initialization ---

count = 1

allocate(totals(numprocs))
totals = 0

iter_max = 50000
hits_tally = 0

radius = 1.d0

!--- generate a seed for rand() which is different for each processor ---
seed = myid
call srand(seed)  !reinitializes the pseudo-random number generator called by rand()

do i = 1, iter_max

   if (rand() .gt. 0.5) then
      sgn = 1
   else
      sgn = -1
   end if

   !--- random x-coordinate of dart location ---
   x = radius * sgn * rand()

   if (rand() .gt. 0.5) then
      sgn = 1
   else
      sgn = -1
   end if

   !--- random y-coordinate of dart location ---         
   y = radius * sgn * rand()

   !--- dart locaiton distance magnitude from origin ---
   dist_mag = sqrt(x**2 + y**2)

   !--- record this iteration if dist_mag is inside circle ---
   if (dist_mag .le. radius) then
      hits_tally = hits_tally + 1
   end if

end do

print*, "Processor ", myid, " recorded ", hits_tally, " hits inside the circle, out of ", iter_max, " total hits"

!--- Gather all tallies to root processor ---
call MPI_GATHER(hits_tally, count, MPI_INT, totals, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)

!--- Compute pi_appx if processor 0 ---
if (myid .eq. 0) then
   pi_appx = 4.d0 * ( real(sum(totals)) / real(numprocs * iter_max) )

   print*, "Approximation of pi: ", pi_appx

end if

deallocate(totals)

call MPI_FINALIZE(ierr)

end program pi_mpi
