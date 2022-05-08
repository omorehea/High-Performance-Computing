!Program: funct.f90
!Description: define a funciton f(x) to be integrated by the driver program, trap.f90

function f(x)

  implicit none
  real(kind=8) :: x, f

  f = x**2
!  f = sin(x)

end function f
