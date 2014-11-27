! File: overload_normal_procedures.f90
! Purpose: Demonstrate how normal procedures can be overloaded in Fortran, using
!          generic interfaces.

! A procedure outside the module.
subroutine swapReal( a, b )
  real, intent(inout) :: a, b
  real :: tmp
  tmp = a; a = b; b = tmp
end subroutine swapReal

module Utilities
  implicit none
  private     ! Make things 'private' by default...
  public swap ! ...BUT, expose the generic-interface.
  ! Generic interface
  interface swap
     ! Need explicit interface for non-module procedures...
     subroutine swapReal( a, b )
       real, intent(inout) :: a, b
     end subroutine swapReal
     ! ...BUT, module-procedures are attached with a
     ! 'module procedure'-statement.
     module procedure swapInteger
  end interface swap
contains
  ! Module-procedure.
  subroutine swapInteger( a, b )
    integer, intent(inout) :: a, b
    integer :: tmp
    tmp = a; a = b; b = tmp
  end subroutine swapInteger
end module Utilities

program test_util_a
  use Utilities
  implicit none
  integer :: i1 = 1, i2 = 3
  real    :: r1 = 9.2, r2 = 5.6

  write(*,'("Initial state:",1x,2(a,i0,1x), 2(a,f0.2,1x))') &
       "i1 = ", i1, ", i2 = ", i2, ", r1 = ", r1, ", r2 = ", r2
  call swap( i1, i2 )
  call swap( r1, r2 )
  write(*,'("State after swaps:",1x,2(a,i0,1x), 2(a,f0.2,1x))') &
       "i1 = ", i1, ", i2 = ", i2, ", r1 = ", r1, ", r2 = ", r2
end program test_util_a
