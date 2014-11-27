! File: dt_basic_demo.f90
! Purpose: Demonstrate how to define a basic DT (derived data-type),
!          and how this can be used in a program.

! DT-definitions usually included in modules.
module Vec2D_class
  implicit none

  type Vec2D ! Below: declarations for data-members
     real :: mU = 0., mV = 0.
   contains   ! Below: declarations for type-bound procedures
     procedure :: getMagnitude => getMagnitudeVec2D
  end type Vec2D

contains
  real function getMagnitudeVec2D( this )
    class(Vec2D), intent(in) :: this
    getMagnitudeVec2D = sqrt( this%mU**2 + this%mV**2 )
  end function getMagnitudeVec2D
end module Vec2D_class

program test_driver_a
  use Vec2D_class
  implicit none

  type(Vec2D) :: A ! Implicit initialization
  type(Vec2D) :: B = Vec2D(mU=1.1, mV=9.4) ! can use mU&mV as keywords
  type(Vec2D), parameter :: C = Vec2D(1.0, 3.2)

  ! Accessing components of a data-type.
  write(*, '(3(a,1x,f0.3))') &
       "A%U =", A%mU, ", A%V =", A%mV, ", A%magnitude =", A%getMagnitude(), &
       "B%U =", B%mU, ", B%V =", B%mV, ", B%magnitude =", B%getMagnitude(), &
       "C%U =", C%mU, ", C%V =", C%mV, ", C%magnitude =", C%getMagnitude()
end program test_driver_a

