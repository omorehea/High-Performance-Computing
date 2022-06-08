!-----------------------------------------------------------------------

!Program: gol_serial.f90
!Description: Performs the 'Game of Life' algorithm sequentially
!Author: Owen Morehead
!Date: May 8, 2022

!-----------------------------------------------------------------------

program gol_serial

implicit none

integer, allocatable :: A(:,:), A_padded(:,:), A_new(:,:)
integer, allocatable :: nums(:)
real, allocatable :: mat_nums(:,:)
integer :: config, random = 0, glider = 1
integer :: alive_tot, index_ceil
integer :: i, j, step, N, sum, N_run
integer :: seed

real :: set_index

print*, "------ Running sequential Game of Life program ------"
print*, ""

print*, "------ Enter desired initial pattern configuration :"
write(*,*) " Enter: | 0 for random | 1 for glider | 2 for center pattern |"
read(*,*) config

!--------                                                                                                                                        
!ask user to delcare size of matrix, and total number of desired algorithm iterations                                                                                                              
!-------                                                                                                                                               

write(*,*) "Enter the dimension of the square array (i.e. enter 3 for 3 x 3): "
read(*,*) N

write(*,*) "Enter the total number of desired steps to run algorithm for: "
read(*,*) N_run


allocate(A(N,N))
allocate(A_padded(N+2,N+2))
allocate(mat_nums(N,N))

A = 0; A_padded = 0

allocate(nums(2))
nums = (/0,1/)


!-- create random initialization --
if (config == 0) then

   call random_number(mat_nums)
   A(:,:) = floor(2*mat_nums)

end if

!-- create glider specific initialization --
if (config == 1) then

   !-- glider pattern --
   !-- presented in uppermost left corner of grid --
   A(2,1) = 1; A(3,2) = 1; A(1:3,3) = 1

end if

if (config == 2) then
   
   set_index = size(A,1)/2
   index_ceil = ceiling(set_index)

   !-- pattern in center of grid --
   A(index_ceil,index_ceil) = 1
   A((index_ceil - 2):index_ceil:2,index_ceil+1) = 1
   A((index_ceil - 1):index_ceil,index_ceil + 2) = 1

end if



print*,""
print*,"Starting grid matrix A"
print*, ""

do i = 1, size(A,1)
   print*, A(i,:)
end do

allocate(A_new(N,N))
A_new = A

!------
!run the algorithm N_run amount of times
!------
 
do step = 1, N_run

   !------
   !fill in original (N x N) matrix into padded matrix
   !------

   do j = 2,N+1
      do i = 2,N+1
         A_padded(i,j) = A_new(i-1,j-1)
      end do
   end do

   !--------                                                                                                                                   
   !pad the original matrix with the reflection of the edges                                                                                
   !--------                                                                                                                                              
   A_padded(2:(size(A_padded,1)-1),1) = A_padded(2:(size(A_padded,1)-1),size(A_padded,2)-1)     !leftmost edge                                                                           
   A_padded(2:(size(A_padded,1)-1),size(A_padded,2)) = A_padded(2:(size(A_padded,1)-1),2)       !rightmost edge                                                                   

   A_padded(1,:) = A_padded(size(A_padded,1)-1,:)       !entire top edge (includes corners)                                                                                 
   A_padded(size(A_padded,1),:) = A_padded(2,:)         !entire bottom edge (includes corner)                                                                          
 

   !------
   !calculate new matrix grid that contains a 1 (alive) at any location 
   !if exactly 3 of its 8 neighbors are 1 (alive)
   !------
   
   !-----
   !run the algorithm N_run amount of times
   !-----


   !sum together all neighboring 8 elements and check if sum = 3
   alive_tot = 0
   do i = 2,N+1
      do j = 2,N+1
         sum = 0
         sum = sum + A_padded(i-1,j-1) + A_padded(i,j-1) + A_padded(i-1,j) + A_padded(i,j+1) &
              + A_padded(i+1,j) + A_padded(i+1,j+1) + A_padded(i-1,j+1) + A_padded(i+1,j-1)
         if (sum == 3) then
            A_new(i-1,j-1) = 1.0
 
         else if (sum == 2) then
            A_new(i-1,j-1) = A_new(i-1,j-1)
 
         else 
            A_new(i-1,j-1) = 0   
         end if
         alive_tot = count(A_new == 1)

      end do
   end do

 !  print*, ""
 !  print*, "Iteration: ", step
 !  print*, "Total alive cells: ", alive_tot
   

end do






print*,""
print*,"Grid matrix A at end of algorithm"
print*, ""

do i = 1, size(A_new,1)
   print*, A_new(i,:)
end do






deallocate(A,A_padded,mat_nums,nums)


end program gol_serial
