!----------------------------------------------------------------
!
!Program: gol_mpi_columns.f90
!Description: Performs the 'Game of Life' algorithm in parallel
!             using 1D COLUMN-WISE Domain Decomposition
!Author: Owen  Morehead
!Date: June 2, 2022
!
!----------------------------------------------------------------

program gol_mpi_columns

use mpi

use gol_subs

implicit none
  
!include "mpif.h"


!-- MPI related variables --
integer :: myid, numprocs, ierr

!-- Array containing the partitioned grid to each processor --
integer, allocatable :: grid_local(:,:)
integer, allocatable :: grid_total(:,:)
integer :: num_cols, num_rows

integer :: config

!-- looping variables and others --
integer :: i, j, step, N

!-- local array that contains myid, left and right processors id, and total # of processors --
integer :: myid_buffer(4)

!-- variable to determine how many columns each processor will handle --
integer, allocatable :: workload_find(:)

integer, allocatable :: counts(:)
integer, allocatable :: displs(:)

character(len=4) :: Nstr
character(len=20) :: formatname

integer :: errorcode

!-- Initialize MPI -- 
call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)


!--- ask user to initialize problem ---

if (myid .eq. 0) then
   print*, "------ Running parallel Game of Life program ------"
   print*, ""

   print*, "------ Enter desired initial pattern configuration :"
   write(*,*) " Enter: | 0 for random | 1 for glider | any other number will result in empty grid |"
   read(*,*) config

   write(*,*) '---- Enter the dimension of the square 2D grid: ---- '
   write(*,*) '-- Please choose dim to be atleast equal to the total amount of processors --'
   write(*,*) 'i.e., enter 20 for 20 x 20 grid: ' 
   read(*,*) N

   if (N < numprocs) then
      print*, "N is less than total number of processors! Program Aborting..."
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
   end if

end if

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!-----------------------------------------------------------------------------------------------

!-- allocate array to size of total number of proccesors --
allocate(workload_find(numprocs))


!-- have proc 0 determine workload for each processor
if (myid .eq. 0) then
   call workload_determine(N, workload_find, numprocs)
   
   allocate(grid_total(N,N))
   grid_total = 0
   call grid_init(grid_total, myid_buffer, config)

end if

!if (myid .eq. 0) then
!   print*,'total grid: '
!   do i = 1, size(grid_total,1)
!      print*, grid_total(i,:)
!   end do
!end if


!-- Broadcast workload_find to each processor from root processor --
call MPI_BCAST(workload_find, numprocs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

!-- All processors need value of N, dimension of total grid --
N = sum(workload_find)


num_cols = workload_find(myid + 1) !number of columns for local processor to work on
                                   !(remember, processor #s start at 0, Fortran starts indexing at 1)

!-- create formatname for nicer printing in print function --
write(Nstr,10) N  
10 format(I4) 
formatname = '(' // Nstr // 'i2)' 


!-- Allocate the local, ghost cell padded grid --
allocate(grid_local(N + 2, num_cols + 2))
grid_local = 0


!-- scatter the portions of the total grid in proc 0 to all other processors --
!-- handling uneven load balancing -> use scatterv() with counts and displs --

allocate(counts(numprocs))
allocate(displs(numprocs))
counts = 0
displs = 0

do i = 1, numprocs
   counts(i) = N*workload_find(i)
end do
do i = 2, numprocs
   displs(i) = counts(i-1) + displs(i-1)
end do

deallocate(workload_find)


call MPI_SCATTERV(grid_total,counts, displs, MPI_INTEGER, grid_local(2:size(grid_local,1)-1,2:size(grid_local,2)-1), &  
     &            N*num_cols, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr) 


!-- establish communicaiton between processors in ring formation --
call ring_comm(myid, myid_buffer, numprocs)

!-- apply periodic boundary conditions to all local processors grids --
call periodic_boundaries_init(grid_local, myid_buffer) 

!-- wait for all processors to catch up to this point --
call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!-- print out initial grid state at step = 0 --
if (myid .eq. 0) then

   print*, "------ Initial State of Grid, Step 0 ------"
   print*, ""
   
!   print*,'proc 0 grid: '
!   do i = 1, size(grid_local,1)
!      print*, grid_local(i,:)
!   end do

end if

!-- this function will print the total grid, no ghost padding --
call print_grid_total(myid, numprocs, grid_local, N, counts, displs, formatname)


do  step = 1, 80

   !-- update grid -- 
   call grid_update(grid_local, myid_buffer)
   call periodic_boundaries_init(grid_local,myid_buffer)

   if (mod(step, 20) .eq. 0) then
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      
      if (myid .eq. 0) then

         print*, "Step number: ", step
         print*, ""
         print*, "------ Grid ------"
         print*, ""
      end if
      
      call print_grid_total(myid, numprocs, grid_local, N, counts, displs, formatname)

   end if

end do

if (myid .eq. 0) then
   deallocate(grid_total)
end if

deallocate(grid_local,counts,displs)

call MPI_FINALIZE(ierr)
stop

end program gol_mpi_columns





