!Program: trap_int.f90
!Author: Owen Morehead
!Date: April 5, 2022
!Description: Integrates a function f(x) between two limits using the Trapezoidal rule.

program trap_int

implicit none

real(kind=8) :: a, b, pi
real(kind=8), external :: f
real(kind=8) :: sum, dx = 0.d0
integer :: i, N

pi = acos(-1.d0)

print*, ""
print*, " ------ Performing Trapezoidal Integration ------"
print*, ""

write(*,*) "Enter the Lower bound of integration over f(x): "
read(*,*) a
write(*,*) "Enter the Upper bound of integration over f(x): "
read(*,*) b

if (a > b) then
   stop "Lower bound value greater than Upper bound value"
end if

write(*,*) "Enter the number of intervals, N, for integration over f(x): "
read(*,*) N

dx = (b-a)/N

do i = 0, N-1
   sum = sum + (dx/2)*( f(a + dx * i) + f(a + dx*(i+1)) )
end do

print*, ""
!write(*,20) 'Solution to integral over f(x) from', a, ' to ', b
write(*,20)  a, b
20 format('Solution to integral of f(x) from',f10.3,'     to',f10.3)
print*,'=', sum



end program trap_int
