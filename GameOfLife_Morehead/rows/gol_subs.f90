!--------------------------------------------------------------------------------------
!
!Module: gol_subs.f90
!Description: Module containing all subroutines used in Driver Programs
!Author: Owen Morehead
!Date: June 2, 2022
!
!--------------------------------------------------------------------------------------

module gol_subs

use mpi

implicit none


contains

subroutine workload_determine(N, workload, numprocs)
!---------------------------------------------------------------------------------------
!
!Description: Determines how many columns of the grid space to distribute to each processor
!
!Parameters:
!            N -- integer : size of square array
!            workload -- integer array : empty vector to contain workload for each processor upon output
!            numprocs -- integer : total number of processors
!
!---------------------------------------------------------------------------------------

  implicit none
  integer, intent(inout) :: N, numprocs
  integer, intent(inout) :: workload(:)

  !-- if there is more processors than columns --
  if (numprocs .gt. N) then
     
     !each processor only gets one column, until the columns run out
     workload(1:N) = 1
     workload(N+1:) = 0

  !-- if number of columns equals number of processors --
  else if (numprocs .eq. N) then
     
     !each processor gets one column
     workload = 1

  !-- if more columns than processors --
  else

     ! equal load balancing if mod(N, numprocs) = 0
     ! if not, processor 0 will take what columns are left
     ! while all other processors get equal amount
     
     workload(1) = N / numprocs + mod(N, numprocs)
     workload(2:) = N / numprocs

  end if

end subroutine workload_determine


subroutine ring_comm(myid, myid_buffer, numprocs)
!--------------------------------------------------------------------------------------
!
!Description: Establishes ring-like communication between processors
!
!Parameters:
!            myid -- integer : specific processors ID
!            numprocs -- integer : total number of processors
!            myid_buffer -- integer array : array containing communication data
!
!              myid_buffer(1) : current processor number
!              myid_buffer(2) : processor to the left 
!              myid_buffer(3) : processor to the right
!              myid_buffer(4) : total number of processors
!
!--------------------------------------------------------------------------------------

  implicit none

  integer :: myid, numprocs, myid_left, myid_right
  integer :: myid_buffer(:)

  !Determine communication between processors
  
  !Periodic boundaries -->

  !First processor communicates with last one

  if (myid .eq. 0) then
     myid_right = myid + 1
     myid_left = numprocs - 1

  !Last processor communicates with first one
  else if (myid .eq. numprocs - 1) then
     myid_right = 0
     myid_left = myid - 1

  !Any other processor
  else
     myid_right = myid + 1
     myid_left = myid - 1
  
  end if

  !Store results in myid_buffer array
  myid_buffer(1) = myid
  myid_buffer(2) = myid_left
  myid_buffer(3) = myid_right
  myid_buffer(4) = numprocs


end subroutine ring_comm

subroutine grid_init(grid_domain, myid_buffer, config)
!------------------------------------------------------------------------------
!
!Description: Initializes the grid of alive and dead cells.
!            !
!Parameters:
!            grid_domain -- 2D integer array : representing the desired domain (0's and 1's)
!            config -- integer : desired configuration of alive and dead cells
!            myid_buffer -- integer array : array containing communication data for each proc
!            
!              myid_buffer(1) : current processor number
!              myid_buffer(2) : processor to the left 
!              myid_buffer(3) : processor to the right
!              myid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------
  implicit none

  integer :: myid
  integer :: grid_domain(:,:)
  integer :: myid_buffer(:)
  integer :: config
  integer :: grid_domain_dup(size(grid_domain,1),size(grid_domain,2))
  real(kind=8) :: mat_nums(size(grid_domain,1),size(grid_domain,2))

  integer :: i,j
  
  !-- create grid configuration based on value of config variable --

  !-- create random initialization --
  if (config == 0) then
     
     call random_number(mat_nums)
     grid_domain(1:size(grid_domain,1),1:size(grid_domain,2)) = floor(2*mat_nums)

     !-- glider configuration --
     !-- presented in uppermost left corner of grid --
     
  else if (config == 1) then
     grid_domain = 0 !start by setting all cells to 0 (dead)

     grid_domain(2,1) = 1 !these certain cells are 1 (alive)
     grid_domain(3,2) = 1
     grid_domain(1:3,3) = 1
     
    ! grid_domain = grid_domain_dup + grid_domain

     !-- for now, any other config value will set an empty grid --
  else
     grid_domain = 0

  end if

end subroutine grid_init


subroutine periodic_boundaries_init(grid, myid_buffer)
!------------------------------------------------------------------------------ 
!
!Description: Sets the periodic boundary conditions for all the processors
!             for standard COLUMN-WISE domain decomposition
!Parameters:
!            grid -- 2D integer array : representing the current initialized grid of each processor 
!            myid_buffer -- integer array : array containing communication data for each processor
!
!              myid_buffer(1) : current processor number
!              myid_buffer(2) : processor to the left 
!              myid_buffer(3) : processor to the right
!              myid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------ 

  implicit none

  integer :: grid(:,:)
  integer :: myid_buffer(:)
  integer, allocatable :: col_temp(:)
  integer :: height, width
  
  integer :: request, stat(mpi_status_size), tag, ierr
  
  tag = 1234

  height = size(grid,1)
  width = size(grid,2)
  
  !-- allocate variable to use in mpi send/recv --
  !-- column minus the top and bottom corner cells --
  allocate(col_temp(height-2))

  !-- right-most column of current grid --
  col_temp = grid(2:(height-1),width-1)

  !-- sending right-most column from each processor to processor on right --
  call MPI_ISEND(col_temp, height-2, MPI_INTEGER, myid_buffer(3), &
       &         1234, MPI_COMM_WORLD, request, ierr)

  !-- recieve right-most column from each source processor to the left --
  call MPI_RECV(grid(2:(height-1),1), height-2, MPI_INTEGER, myid_buffer(2), &
       &        tag, MPI_COMM_WORLD, stat, ierr)


  !-- left-most column of current grid --
  col_temp = grid(2:height-1,2)
  
  !-- sending left-most column from each processor to processor on left --
  call MPI_ISEND(col_temp, height-2, MPI_INTEGER, myid_buffer(2), &
       &         1234, MPI_COMM_WORLD, request, ierr)
  
  !-- recieve left-most column from each source processor to the right --
  call MPI_RECV(grid(2:(height-1),width), height-2, MPI_INTEGER, myid_buffer(3), &
       &        tag, MPI_COMM_WORLD, stat, ierr)
  
  !-----------------------------------------------------------------------

  !-- set top and bottom row boundary conditions, this now includes corners --

  grid(height,:) = grid(2,:)
  grid(1,:) = grid(height-1,:)


  deallocate(col_temp)

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)


end subroutine periodic_boundaries_init



subroutine periodic_boundaries_init_rows(grid, myid_buffer)
!------------------------------------------------------------------------------ 
!
!Description: Sets the periodic boundary conditions for all the processors
!             for ROW-WISE domain decomposition
!Parameters:
!            grid -- 2D integer array : representing the current initialized grid of each processor 
!            myid_buffer -- integer array : array containing communication data for each processor
!
!              myid_buffer(1) : current processor number
!              myid_buffer(2) : processor to the left 
!              myid_buffer(3) : processor to the right
!              myid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------ 

  implicit none

  integer :: grid(:,:)
  integer :: myid_buffer(:)
  integer, allocatable :: row_temp(:)
  integer :: height, width
  
  integer :: request, stat(mpi_status_size), tag, ierr
  
  tag = 1234

  height = size(grid,1)
  width = size(grid,2)
  
  !-- allocate variable to use in mpi send/recv --
  !-- column minus the top and bottom corner cells --
  allocate(row_temp(width-2))

  !-- bottom-most column of current grid --
  row_temp = grid(height-1,2:width-1)

  !-- sending bottom-most column from each processor to processor below --
  call MPI_ISEND(row_temp, width-2, MPI_INTEGER, myid_buffer(3), &
       &         1234, MPI_COMM_WORLD, request, ierr)

  !-- recieve bottom-most column from each source processor above and put in first row --
  call MPI_RECV(grid(1,2:width-1), width-2, MPI_INTEGER, myid_buffer(2), &
       &        tag, MPI_COMM_WORLD, stat, ierr)


  !-- top-most column of current grid --
  row_temp = grid(2,2:width-1)
  
  !-- sending top-most column from each processor to processor above --
  call MPI_ISEND(row_temp, width-2, MPI_INTEGER, myid_buffer(2), &
       &         1234, MPI_COMM_WORLD, request, ierr)
  
  !-- recieve top-most column from each source processor below and put in bottom row --
  call MPI_RECV(grid(height,2:width-1), width-2, MPI_INTEGER, myid_buffer(3), &
       &        tag, MPI_COMM_WORLD, stat, ierr)
  
  !-----------------------------------------------------------------------

  !-- set left and right column boundary conditions, this now includes corners --
  grid(:,width) = grid(:,2)   
  grid(:,1) = grid(:,width-1)

  deallocate(row_temp)

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)


end subroutine periodic_boundaries_init_rows



subroutine grid_update(grid, myid_buffer)
!------------------------------------------------------------------------------  
!
!Description: Update the current grid configuration using Game Of Life algorithm
!
!Parameters: 
!           grid -- 2D integer array : representing the current initialized grid   
!           myid_buffer -- integer array : array containing communication data for each proc
!
!              myid_buffer(1) : current processor number
!              myid_buffer(2) : processor to the left 
!              myid_buffer(3) : processor to the right
!              myid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------  

  implicit none

  integer :: grid(:,:)
  integer :: myid_buffer(:)
 
  integer, allocatable :: grid_temp(:,:)
  integer :: i,j,height,width,count

  height = size(grid,1)
  width = size(grid,2)

  allocate(grid_temp(height,width))

  grid_temp = grid

  !-- Update all inner, non-ghost nodes of data --

  do j = 2, width - 1

     do i = 2, height - 1

        count = 0

        !-- check value of all 8 neighboring cells and add to count variable --
        count = count + grid_temp(i-1,j-1) + grid_temp(i-1,j) + grid_temp(i-1,j+1) &
             &        + grid_temp(i+1,j-1) + grid_temp(i+1,j) + grid_temp(i+1,j+1) & 
             &        + grid_temp(i,j+1) + grid_temp(i,j-1)


        !-- determine if cell is alive or dead based on count value --

        if (count .eq. 3) then
           grid(i,j) = 1
        
        else if (count .ne. 2) then !this option will run if count .ne. 3 and count .ne. 2
           grid(i,j) = 0

        !-- if count .eq. 2, no change in cell status. no need for this code --

        end if

     end do
  end do

deallocate(grid_temp)

end subroutine grid_update


subroutine print_grid_total(myid, numprocs, grid_local, dim_global, counts, displs, formatname)
!------------------------------------------------------------------------------  
!
!Description: Prints the current state of the grid to the screen
!             for standard COLUMN-WISE domain decomposition
!Parameters: 
!            myid -- integer : specific processors ID
!            numprocs -- integer : total number of processors
!            grid_local -- 2D integer array : representing the current state of the local processors grid
!            dim_global -- integer : dimension of square total grid domain
!            counts -- integer array : array of counts for gatherv
!            displs -- integer array : array of displacements for gatherv
!            formatname -- character : used to print grids with less spacing between columns
!
!------------------------------------------------------------------------------  

  integer none

  integer :: myid, numprocs, dim_global
  integer :: grid_local(:,:)
  integer :: grid_local_noghost(size(grid_local,1)-2,size(grid_local,2)-2)

  integer :: counts(:), displs(:)
  integer, allocatable :: grid_total_array(:)
  integer, allocatable :: grid_total(:,:)

  integer :: status(MPI_STATUS_SIZE), ierr

  integer :: i,j

  character(len=15) :: formatname

  !-- store non-ghosted local matrix on each processor --
  do j = 2, (size(grid_local,2)-1)
     do i = 2, (size(grid_local,1)-1)
        grid_local_noghost(i-1,j-1) = grid_local(i,j)
     end do
  end do

  if (myid .eq. 0) then
     !-- only master node allocates space for the total grid to print --
     allocate(grid_total_array(dim_global*dim_global))
     allocate(grid_total(dim_global,dim_global))
  end if

  !-- gather all data to a 2D array in root processor --
  call MPI_GATHERV(grid_local_noghost, size(grid_local_noghost,1) * size(grid_local_noghost,2), &
       &           MPI_INTEGER, grid_total, counts, displs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  
  if (myid .eq. 0) then

     !-- print grid --
     write(*,formatname) ((grid_total(i,j), j = 1,size(grid_total,2)), i = 1,size(grid_total,1))

     print*, ""

     deallocate(grid_total_array)

  end if

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)


end subroutine print_grid_total


subroutine print_grid_total_rowwise(myid, numprocs, grid_local, dim_global, counts, displs, resizedrowtype, formatname)
!------------------------------------------------------------------------------  
!
!Description: Prints the current state of the grid to the screen
!             using ROW-WISE domain decomposition
!Parameters: 
!            myid -- integer : specific processors ID
!            numprocs -- integer : total number of processors
!            grid_local -- 2D integer array : representing the current state of the local processors grid
!            dim_global -- integer : dimension of square total grid domain
!            counts -- integer array : array of counts for gatherv
!            displs -- integer array : array of displacements for gatherv
!            resizedrowtype -- integer : derived data type used for gatherv row-wise
!            formatname -- character : used to print grids with less spacing between columns
!
!------------------------------------------------------------------------------  

  integer none

  integer :: myid, numprocs, dim_global
  integer :: grid_local(:,:)
  integer :: grid_local_noghost(size(grid_local,1)-2,size(grid_local,2)-2)
  
  integer :: resizedrowtype
  integer :: counts(:), displs(:)
  integer, allocatable :: grid_total_array(:)
  integer, allocatable :: grid_total_nog(:,:)

  integer :: status(MPI_STATUS_SIZE), ierr

  integer :: i,j

  character(len=20) :: formatname
  
 
  !-- store non-ghosted local matrix on each processor --
  do j = 2, (size(grid_local,2)-1)
     do i = 2, (size(grid_local,1)-1)
        grid_local_noghost(i-1,j-1) = grid_local(i,j)
     end do
  end do


  if (myid .eq. 0) then
     !-- only master node allocates space for the total grid to print --
     allocate(grid_total_array(dim_global*dim_global))
     allocate(grid_total_nog(dim_global,dim_global))
  end if


  call MPI_GATHERV(grid_local_noghost, size(grid_local_noghost,1)*size(grid_local_noghost,2), MPI_INTEGER, &
       &           grid_total_nog, counts, displs, resizedrowtype, 0, MPI_COMM_WORLD, ierr)


  
  if (myid .eq. 0) then

     !-- print grid --
     write(*,formatname) ((grid_total_nog(i,j), j = 1,size(grid_total_nog,2)), i = 1,size(grid_total_nog,1))

     print*, ""

     deallocate(grid_total_nog)

  end if

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)


end subroutine print_grid_total_rowwise



end module gol_subs
