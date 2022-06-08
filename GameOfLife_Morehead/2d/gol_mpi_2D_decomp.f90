!----------------------------------------------------------------
!
!Program: gol_mpi_2D_decomp.f90
!Description: Performs the 'Game of Life' algorithm in parallel
!             using 2D subgrid Domain Decomposition
!Author: Owen  Morehead
!Date: May 14, 2022
!
!
!-- as of now, program only works for 4 processors
!-- all of which need an evenly balanced load!
!----------------------------------------------------------------

program gol_mpi_2D_decomp

use mpi

use gol_subs

implicit none


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
integer :: myid_buffer(6)

!-- variable to determine how many columns each processor will handle --
integer, allocatable :: workload_find(:)

integer, allocatable :: counts(:)
integer, allocatable :: displs(:)

character(len=4) :: Nstr
character(len=20) :: formatname

!-- variable for derived data type for 2D DECOMP --
integer :: subarray, resizedsubarray, intsize
integer :: row, col, dimprocgrid
real(kind=4) :: dimprocgrid_real, N_real
integer :: procgridsize !this is the size of the processor grid. 
!                        i.e., if 4 procs, grid is 2 x 2 -> procgridsize = 2
real(kind=4) :: procgridsize_real
real(kind=4) :: numprocs_real

integer(kind=MPI_ADDRESS_KIND) :: extent, begin

integer :: errorcode

!-- Initialize MPI -- 
call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

!-- ONLY A GRID OF 4 PROCCESORS IS CURRENTLY WORKING --
if (myid .eq. 0) then   
   if (numprocs .ne. 4) then
      print*, "Number of processors chosen is not 4"
      print*, "Only a grid of 4 processors is currently working. Program aborting..."
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
   end if
end if

!--- ask user to initialize problem ---

if (myid .eq. 0) then
   print*, "------ Running parallel Game of Life program ------"
   print*, ""

   print*, "------ Enter desired initial pattern configuration :"
   write(*,*) " Enter: | 0 for random | 1 for glider | any other number will result in empty grid |"
   read(*,*) config

   write(*,*) '---- Enter the dimension of the square 2D grid, N: ---- '
   write(*,*) '-- For this 2D decomp, only a grid of 4 procs is currenlty working. Must choose N s.t.  mod(N,numprocs/2) = 0. '
   write(*,*) 'i.e., enter 20 for 20 x 20 grid: ' 
   read(*,*) N

   if (mod(N,(numprocs/2)) .ne. 0) then
      print*, "mod(N/2) must equal zero! Program aborting..."
      call MPI_ABORT(MPI_COMM_WORLD, errorcode, ierr)
   end if

   allocate(grid_total(N,N))
   grid_total = 0
   call grid_init(grid_total(:,:), myid_buffer, config)

end if

call MPI_BARRIER(MPI_COMM_WORLD,ierr)


!-- Broadcast N to each processor from root processor --
call MPI_BCAST(N, numprocs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)


N_real = N
dimprocgrid_real = N_real/(numprocs/2)
dimprocgrid = int(dimprocgrid_real)

numprocs_real = numprocs
procgridsize_real = sqrt(numprocs_real)
procgridsize = int(procgridsize_real)

if (myid .eq. 0) then
   print*, "dimprocgrid: ", dimprocgrid
end if

!-- formatname to reformat the printed grids in print function --
write(Nstr,10) N  
10 format(I4) 
formatname = '(' // Nstr // 'i2)' 


!-- Allocate the local, ghost cell padded grid --
allocate(grid_local(dimprocgrid+2, dimprocgrid+2))
grid_local = 0


!-- scatter the portions of the total grid in proc 0 to all other processors --
!-- first make derived data type --

call MPI_TYPE_CREATE_SUBARRAY(2, (/N,N/), (/(size(grid_local,1)-2),(size(grid_local,2)-2)/), (/0,0/), &
     &                        MPI_ORDER_FORTRAN, MPI_INTEGER, subarray, ierr) 


call MPI_TYPE_SIZE(MPI_INTEGER, intsize, ierr)

begin = 1
extent = (size(grid_local,1)-2)*intsize

call MPI_TYPE_CREATE_RESIZED(subarray, begin, extent, resizedsubarray, ierr)
call MPI_TYPE_COMMIT(resizedsubarray, ierr)


allocate(counts(numprocs))
allocate(displs(numprocs))
counts = 1
displs = 0

forall( col = 1:procgridsize, row=1:procgridsize )
   displs(1 + (row-1) + procgridsize*(col-1)) = (row-1) + (size(grid_local,1)-2)*procgridsize*(col-1)
end forall


!-- scatterv to scatter squre subgrids to each processor --

call MPI_SCATTERV(grid_total(:,:), counts, displs, resizedsubarray, grid_local(2:(size(grid_local,1)-1),2:(size(grid_local,2)-1)), &
     &            (size(grid_local,1)-2)*(size(grid_local,2)-2), MPI_INTEGER, 0, MPI_COMM_WORLD, ierr) 


!if (myid .eq. 2) then
!   print*,'proc 2 grid: '
!   do i = 1, size(grid_local,1)
!      print*, grid_local(i,:)
!   end do

!end if


!-- establish communicaiton between processors in ring formation --
call ring_comm_2d(myid, myid_buffer, numprocs)

!-- apply periodic boundary conditions to all local processors grids --
call periodic_boundaries_init_2d(grid_local, myid_buffer) 

!-- wait for all processors to catch up to this point --
call MPI_BARRIER(MPI_COMM_WORLD,ierr)

!-- print out initial grid state at step = 0 --
if (myid .eq. 0) then

   print*, "------ Initial State of Grid, Step 0 ------"
   print*, ""
   
end if


!-- this function will print the total grid, no ghost padding --
call print_grid_total_2d(myid, numprocs, grid_local, N, counts, displs, resizedsubarray, formatname)


do  step = 1, 80

   !-- update grid -- 
   call grid_update(grid_local, myid_buffer)
   call periodic_boundaries_init_2d(grid_local, myid_buffer)

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
      
      call print_grid_total_2d(myid, numprocs, grid_local, N, counts, displs, resizedsubarray,formatname)

   end if

end do

if (myid .eq. 0) then
   deallocate(grid_total)
end if

deallocate(grid_local,counts,displs)

call MPI_FINALIZE(ierr)
stop

end program gol_mpi_2D_decomp





