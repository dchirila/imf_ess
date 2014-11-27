! File: dt_constructor_and_initializer.f90
! Purpose: Demonstrate the two mechanisms (i.e. user-defined constructor or
!          init-subroutine) which can be used to initialize variables of a
!          derived data-type, WHILE still keeping the data-members of the type
!          PRIVATE (to facilitate implementation-hiding).

module Vec2D_class
  implicit none
  private ! Make module-entities "private" by default.

  type, public :: Vec2D ! DT explicitly declared "public"
     private  ! Make internal data "private" by default.
     real :: mU = 0., mV = 0.
   contains
     private  ! Make methods "private" by default.
     procedure, public :: init => initVec2D
     ! . . . more methods (ommitted in this example) . . .
  end type Vec2D

  ! Generic IFACE, for type-overloading
  ! (to implement user-defined CTOR)
  interface Vec2D
     module procedure createVec2D
  end interface Vec2D

contains
  type(Vec2D) function createVec2D( u, v ) ! CTOR
    real, intent(in) :: u, v
    createVec2D%mU = u
    createVec2D%mV = v
  end function createVec2D

  subroutine initVec2D( this, u, v ) ! init-subroutine
    class(Vec2D), intent(inout) :: this
    real, intent(in) :: u, v
    ! copy-over data inside the object
    this%mU = u
    this%mV = v
  end subroutine initVec2D
end module Vec2D_class

program test_driver_b
  use Vec2D_class
  implicit none

  type(Vec2D) :: A, D
  ! ERROR: cannot define constants of DT with private data!
  !type(Vec2D), parameter :: B = Vec2D(1.0, 3.2)
  ! ERROR: cannot use user-CTOR to initialize at declaration!
  !type(Vec2D) :: C = Vec2D(u=1.1, v=9.4)

  ! Separate call to CTOR.
  A = Vec2D(u=1.1, v=9.4)

  ! Separate call to init-subroutine
  call D%init(u=1.1, v=9.4)
end program
