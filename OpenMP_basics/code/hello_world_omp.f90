!-----------------------------------------------------------------
!
!Program: hello_world_omp.f90
!Description: Prints hello world from each thread using OpenMP
!Author: Owen Morehead
!Date: May 14, 2022
!
!-----------------------------------------------------------------

program hello_world_omp

implicit none

integer :: myid, nthreads, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM

!$OMP PARALLEL PRIVATE(myid, nthreads)

! obtain thread number
myid = OMP_GET_THREAD_NUM()

print*, "Hello World from thread = ", myid

! only master thread prints total threads
if (myid .eq. 0) then
   nthreads = OMP_GET_NUM_THREADS()
   print*, "Number of threads = ", nthreads
end if

! all threads join master thread and disband
!$OMP END PARALLEL


end program hello_world_omp





















