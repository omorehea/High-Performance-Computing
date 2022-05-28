!-------------------------------------------------------------
!
!Program: do_matprod_omp.f90
!Description: Calculates the product of two matrices using do loops in OpenMP
!Author: Owen Morehead
!Date: May 14, 2022
!
!-------------------------------------------------------------

program do_matprod_omp

implicit none

real, allocatable :: A(:,:), B(:,:), C(:,:), mat_rand(:,:)
real :: minvalue, minvalue_temp
real :: minlocat(2), minlocat_temp(2)
real(kind=8) :: start_time, end_time, OMP_GET_WTIME

integer :: i, j, k, N

minvalue = 10000000
minvalue_temp = minvalue
minlocat = 0 

write(*,*) "Enter the dimension of the square arrays: "
read(*,*) N

allocate(A(N,N))
allocate(B(size(A,2),size(A,1)))
allocate(C(size(A,1),size(A,1)))
allocate(mat_rand(size(A,1),size(A,2)))


!A = reshape( (/1,3,2,4/), (/size(A,1),size(A,2)/) )
!B = reshape( (/3,5,4,6/), (/size(B,1),size(B,2)/) )

A = 0
B = 0
C = 0

call random_number(mat_rand)
A(:,:) = mat_rand*5

call random_number(mat_rand)
B(:,:) = mat_rand*5

!print*, "matrix A : "                                                                                                               
!do i = 1, size(A,2)                                                                                                             
!   print*, A(i,:)                                                                                                               
!end do  

!print*, "matrix B : "                                                                                                             
!do i = 1, size(B,2)                                                                                                               
!   print*, B(i,:)                                                                                                                 
!end do  

!-------------------------- do loop version --------------------------
print*, "----- PARALLEL DO LOOP VERSION -----"
print*, ""

start_time = OMP_GET_WTIME()

!$OMP PARALLEL DO
!$OMP& SHARED(A,B,C,minvalue,minlocat) PRIVATE(i,j,k)

do j = 1, size(C,1)
   do i = 1, size(C,1)
      do k = 1, size(C,1)
         C(i,j) = C(i,j) + A(i,k)*B(k,j)
      end do
      
      !check if min val in c and record elements location
      if (C(i,j) < minvalue) then
         minvalue = C(i,j)
         minlocat(1) = i
         minlocat(2) = j
      end if
      
   end do
end do

!$OMP END PARALLEL DO

end_time = OMP_GET_WTIME()

!-- print matrix C --

!print*, "matrix C : "
!do i = 1, size(C,2)
!   print*, C(i,:)
!end do


print*, "min value in C: ", minvalue, "and its location in C: (",minlocat,")"

print*, "Work took :", end_time - start_time, "seconds"
print*, ""

!-------------------------- workspace version --------------------------  
print*,"----- PARALLEL WORKSHARE VERSION -----"
print*,""

C = 0
minvalue = 1000000
minlocat = 0

start_time = 0
end_time = 0

start_time = OMP_GET_WTIME()

!$OMP PARALLEL SHARED(A,B,C,minvalue,minlocat)

!$OMP WORKSHARE

C = matmul(A,B)
minvalue = minval(C)
minlocat = minloc(C)

!$OMP END WORKSHARE NOWAIT

!$OMP END PARALLEL

end_time = OMP_GET_WTIME()

print*, "min value in C: ", minvalue, "and its location in C: (",minlocat,")"

print*, "Work took :", end_time - start_time, "seconds"
print*, ""






end program do_matprod_omp
