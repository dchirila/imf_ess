integer function add_two_ints_1( a, b )
  implicit none
  integer, intent(in) :: a, b

  add_two_ints_1 = a + b
end function add_two_ints_1

program functions1
  implicit none

  integer :: add_two_ints_1
  integer :: x=2, y=3

  write(*,'(a,i0)') "x+y = ", add_two_ints_1( x, y )
end program functions1

