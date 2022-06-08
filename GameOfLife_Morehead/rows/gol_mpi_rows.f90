!----------------------------------------------------------------
!
!Program: gol_mpi_rows.f90
!Description: Performs the 'Game of Life' algorithm in parallel
!             using 1D ROW-WISE Domain Decomposition
!Author: Owen  Morehead
!Date: June 2, 2022
!
!----------------------------------------------------------------

program gol_mpi_rows

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

integer :: config, config_rest, alive_tot, index_ceil

!-- looping variables and others --
integer :: i, j, step, N, sum, N_run

!-- local array that contains myid, left and right processors id, and total # of processors --
integer :: myid_buffer(4)

!-- variable to determine how many columns each processor will handle --
integer, allocatable :: workload_find(:)

integer, allocatable :: counts(:)
integer, allocatable :: displs(:)

character(len=4) :: Nstr
character(len=20) :: formatname

!-- derived data type for row-wise decomp --
integer :: rowtype, resizedrowtype, intsize
integer(kind=MPI_ADDRESS_KIND) :: extent, begin

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

   write(*,*) '---- Enter the dimension of the square 2D grid: N ---- '
   write(*,*) '-- Please choose dim s.t. processors have equal load balance --> mod(N,numprocs) = 0'
   write(*,*) 'i.e., enter 20 for 20 x 20 grid: ' 
   read(*,*) N

   !-- make sure processors have equal load balance --

   if (mod(N,numprocs) .ne. 0) then
      print*, "Uneven load balance between processors! Program aborting..."
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
   end if

end if

call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!-- allocate array to size of total number of proccesors --
allocate(workload_find(numprocs))


!-- have proc 0 determine workload for each processor --
if (myid .eq. 0) then
   call workload_determine(N, workload_find, numprocs)
   
   allocate(grid_total(N,N))
   grid_total = 0
   call grid_init(grid_total(:,:), myid_buffer, config)

end if


!-- Broadcast workload_find to each processor from root processor --
call MPI_BCAST(workload_find, numprocs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

!-- All processors need variable N -> dimension of original square grid --
N = sum(workload_find)


num_rows = workload_find(myid + 1) !number of ROWS for local processor to work on
                                   !(remember, processor #s start at 0, Fortran starts indexing at 1)

!-- formatname for printing matrices funciton --
write(Nstr,10) N  
10 format(I4) 
formatname = '(' // Nstr // 'i2)' 


!-- Allocate the local, ghost cell padded grid --
allocate(grid_local(num_rows + 2, N + 2))
grid_local = 0


!-- VECTOR DERIVED DATA TYPE SETUP --

call MPI_TYPE_VECTOR(N, size(grid_local,1)-2, N, MPI_INTEGER, rowtype, ierr)

call MPI_TYPE_SIZE(MPI_INTEGER, intsize, ierr)

extent = (size(grid_local,1)-2)*intsize
begin = 1

call MPI_TYPE_CREATE_RESIZED(rowtype, begin, extent, resizedrowtype, ierr)
call MPI_TYPE_COMMIT(resizedrowtype, ierr)


!-- scatter the portions of the total grid in proc 0 to all other processors using derived data type --

allocate(counts(numprocs))
allocate(displs(numprocs))

counts = 1
displs = 0

!do i = 1, numprocs
   !counts(i) = (size(grid_local,1)-2)*(size(grid_local,2)-2)
   !counts(i) = (size(grid_local,1)-2)
!   counts(i) = 1

!end do


do i = 2, numprocs
!   displs(i) = ((size(grid_local,1)-2) + displs(i-1))
   displs(i) = displs(i-1) + 1
end do

!if (myid .eq. 1) then
!   print*, "myid: ", myid
!   print*, "counts: ", counts
!   print*, "displs: ", displs
!   print*, "intsize: ", intsize
!   print*, "extent: ", extent
!   print*, "dim local grids: ", size(grid_local,1)-2, size(grid_local,2)-2

!end if

deallocate(workload_find)


!-- SCATTERV using resizedrowtype derived data type  --

call MPI_SCATTERV(grid_total(:,:), counts, displs, resizedrowtype, grid_local(2:(size(grid_local,1)-1),2:(size(grid_local,2)-1)), &
     &            (size(grid_local,1)-2)*(size(grid_local,2)-2), MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)



!-- establish communicaiton between processors in ring formation --
call ring_comm(myid, myid_buffer, numprocs)

!-- apply periodic boundary conditions to all local processors grids --
call periodic_boundaries_init_rows(grid_local, myid_buffer) 

!-- wait for all processors to catch up to this point --
call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!-- print out initial grid state at step = 0 --
if (myid .eq. 0) then

   print*, "------ Initial State of Grid, Step 0 ------"
   print*, ""
   

end if

!-- this function will print the total grid, no ghost padding --
call print_grid_total_rowwise(myid, numprocs, grid_local, N, counts, displs, resizedrowtype, formatname)


do  step = 1, 80

   !-- update grid -- 
   call grid_update(grid_local, myid_buffer)
   call periodic_boundaries_init_rows(grid_local, myid_buffer)

   if (mod(step, 20) .eq. 0) then
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)

      if (myid .eq. 0) then

!         print*,'proc 0 grid: '
!         do i = 1, size(grid_local,1)
!            print*, grid_local(i,:)
!         end do


         print*, "Step number: ", step
         print*, ""
         print*, "------ Grid ------"
         print*, ""
      end if
      
      call print_grid_total_rowwise(myid, numprocs, grid_local, N, counts, displs, resizedrowtype, formatname)

   end if

end do

if (myid .eq. 0) then
   deallocate(grid_total)
end if

deallocate(grid_local,counts,displs)

call MPI_TYPE_FREE(rowtype,ierr)
call MPI_FINALIZE(ierr)
stop

end program gol_mpi_rows




