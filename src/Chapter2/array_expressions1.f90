program array_expressions1
  implicit none
  integer, parameter :: N=100
  real, parameter :: PI=3.1415
  integer :: i
  real, dimension(-N:N) :: &
       xAxis = [ (i*(pi/N), i=-N,N) ], &
       a = 0, b = 0

  ! Compact array-expressions, using elemental functions.
  ! a(i) == sin( xAxis(i) )
  a = sin(xAxis)
  ! b(i) == sin( xAxis(i) ) + cos( xAxis(i) )/2.
  b = sin(xAxis) + cos(xAxis)/2.

  write(*, '(f8.4, 2x, f8.4, 2x, f8.4)') &
       [ (xAxis(i), a(i), b(i), i=-N,N) ]
end program array_expressions1
