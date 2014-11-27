! File: forbidden_side_effects_for_pure.f90
! Purpose: Demonstrate some side-effects which are forbidden for pure
! procedures.
! NOTE: The offending lines are commented-out; to test the reaction of your
! compiler to a particular side-effect, un-comment the corresponding lines.

module Test
  implicit none
  integer :: moduleVar

contains
  pure real function doubleNumber( x )
    real, intent(in) :: x
    ! OK: Local (and non-persistent) local variable.
    real :: y
    ! Error: for functions, intent of args can only be 'in'.
    !real, intent(inout) :: x

    ! Error: Not allowed to modify data external to the procedure
    !        (in this case, we have access to the data from the module
    !        through host-association).
    !moduleVar = 0.

    ! Error: Initialization at declaration confers saved-status,
    !        so it is not allowed!
    !real :: b = 0.

    ! Error: Granting saved-status explicitly is also not allowed, of
    !        course.
    !real, save :: c

    ! Error: Pure procedures cannot contain the 'stop'-keyword.
    !if( x < 0 ) stop

    ! Error: Not allowed to write to unit which is not internal-file
    !        (declared locally to the procedure).
    !write(*, '(f0.8)') "x = ", x

    ! Error: Subroutine 'halveNumber' is pure, and was correctly defined,
    !        but it tries to modify 'x', which is declared with intent(in)
    !        within the function 'doubleNumber'.
    !call halveNumber(x)

    y = 0.
    ! Error: Not allowed to call a non-pure procedure from a pure one.
    !call addUserSpecifiedNumberTo( y )

    ! OK: Function-result is simply computed from the function-arguments, as
    !     in a mathematical function.
    doubleNumber = 2*x
  end function doubleNumber

  pure subroutine halveNumber( x )
    real, intent(inout) :: x

    x = x / 2.0
  end subroutine halveNumber

  subroutine addUserSpecifiedNumberTo( x )
    real, intent(inout) :: x
    real :: y

    read(*,*) y ! Get a number from the user...
    x = x+y     ! ...and add it to x.
  end subroutine addUserSpecifiedNumberTo
end module Test

program pure_example
  use Test
  implicit none

  print*, doubleNumber( 1.0 )
end program pure_example

