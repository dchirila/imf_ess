program swap_numbers_internal_subroutine
  implicit none
  real :: a=2., b=3.

  print*, "Before subroutine call:"
  print*, "a=", a, ", b=", b

  call swap(a, b)

  print*, "After subroutine call:"
  print*, "a=", a, ", b=", b

contains

  subroutine swap(x, y)
    real, intent(inout) :: x, y
    real aux
    ! perform swap
    aux=x; x=y; y=aux
  end subroutine swap

end program swap_numbers_internal_subroutine

