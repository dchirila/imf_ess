program scalars_numeric_types
  implicit none

  integer i
  integer :: j = 20
  integer, parameter :: K = 30

  real x
  real :: y = 1.23
  real, parameter :: Z = 1.e2

  complex c1
  complex :: c2 = (1.0, 7.19e23)
  complex, parameter :: C3 = (2., 3.37)


  real :: m = 3.14, n = 2.0

  i = m/n
  print*, "i=", i
  m = -m ! negate m
  i = m/n
  print*, "i=", i

end program scalars_numeric_types
