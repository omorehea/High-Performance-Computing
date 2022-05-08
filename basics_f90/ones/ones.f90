!Program: ones.f90
!Description: Dynamically creates an array of random 0,1 elements 
!Author: Owen Morehead
!Date: April 5, 2022

program ones

implicit none

integer, allocatable :: A(:,:),A2(:,:),A_new(:,:)
integer, allocatable :: nums(:)
real, allocatable :: mat_nums(:,:)
!real :: r,x
integer :: i,j,N,sum

! ----- variables for portable seed setting -----
!integer :: i_seed
!integer,  allocatable :: a_seed(:)
!integer :: dt_seed(1:8)
! ----- end of variables for seed setting -----

print*, "------ Running ones.f90 program ------"
print*, ""

!--------
!ask user to delcare size of matrix
!-------

write(*,*) "Enter the dimension of the square array: "
read(*,*) N

!if (N .le. 2) then
!   print*, "Matrix is 2 x 2 or smaller. Make matrix 3 x 3 or larger please..."
!   stop
!end if

allocate(A(N,N))
allocate(A2(N+2,N+2))
allocate(mat_nums(N,N))
A = 0; A2 = 0

allocate(nums(2))
nums = (/0,1/)

!--------
!fill in matrix A with pseudorandom 0's and 1's
!--------

call random_number(mat_nums)
A(:,:) = floor(2*mat_nums)

!--------
!longer way to fill in matrix A with random 0's and 1's
!--------

!do j = 1, N
!   do i = 1,N
!      call random_number(r)
!      if (r .le. 1.d0/2.d0) then
!         A(i,j) = 0
!      else
!         A(i,j) = 1
!      end if
     ! x = nums(int(r*size(nums)+1))
     ! A(i,j) = x
!   end do
!end do


!--------
!fill in original (N x N) matrix into padded matrix
!--------

do i = 2,N+1
   do j = 2,N+1
      A2(i,j) = A(i-1,j-1)
   end do
end do

!--------
!pad the original matrix with the reflection of the edges
!--------

A2(2:(size(A2,1)-1),1) = A(:,size(A,2))    !leftmost edge
A2(2:(size(A2,1)-1),size(A2,2)) = A(:,1)    !rightmost edge
A2(1,2:size(A2,2)-1) = A(size(A,1),:)       !top edge
A2(size(A2,1),2:size(A2,2)-1) = A(1,:)      !bottom edge

A2(1,1) = A(size(A,1),size(A,2))            !top left corner
A2(1,size(A2,2)) = A(size(A,1),1)           !top right corner
A2(size(A2,1),1) = A(1,size(A,2))           !bottom left corner
A2(size(A2,1),size(A2,2)) = A(1,1)          !bottom right corner


!--------
!print original (N x N) pseudo-random matrix of 0's and 1's
!print original matrix padded with zeros (N+2 x N+2)
!--------
 
print*,""
print*,"Matrix A: "
print*, ""
!print "(f16.3)", ((A(i,j), j = 1,size(A,2)),i = 1,size(A,1))

do i = 1, size(A,1)
   print*, A(i,:)
end do

print*,""
print*,"Matrix A ghost padded: "
print*, ""

do i = 1, size(A2,1)
   print*, A2(i,:)
end do

!--------
!calculates new array that contains a 1 at any location if exactly 3 out of the 8 
!surrounding surrounding neighboring locations in the original array contain 1's
!--------

allocate(A_new(N,N))
A_new = 0

!sum together all neighboring 8 elements and check if sum equals 3
do i = 2,N+1
   do j = 2,N+1
      sum = 0
      sum = sum + A2(i-1,j-1) + A2(i,j-1) + A2(i-1,j) + A2(i,j+1) &
                + A2(i+1,j) + A2(i+1,j+1) + A2(i-1,j+1) + A2(i+1,j-1)
      if (sum == 3) then
         A_new(i-1,j-1) = 1.0
      end if
   end do
end do

print*,""
print*,"Updated Matrix Based on Neighboring Values: "
print*, ""
                                                                                                                                              
do i = 1, size(A_new,1)
   print*, A_new(i,:)
end do


deallocate(A); deallocate(A2); deallocate(A_new)

end program ones
