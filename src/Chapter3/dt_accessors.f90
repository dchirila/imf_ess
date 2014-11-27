! File: dt_accessors.f90
! Purpose: Extend the code in file 'dt_constructor_and_initializer.f90', to
!          allow inquiry of vector-components, and to compute the magnitude of
!          the vector.

module Vec2d_class
  implicit none
  private ! Make module-entities "private" by default.

  type, public :: Vec2d ! DT explicitly declared "public"
     private  ! Make internal data "private" by default.
     real :: mU = 0., mV = 0.
   contains
     private  ! Make methods "private" by default.
     procedure, public :: init => initVec2d
     procedure, public :: getU => getUVec2d
     procedure, public :: getV => getVVec2d
     procedure, public :: getMagnitude => getMagnitudeVec2d
  end type Vec2d

  ! Generic IFACE, for type-overloading
  ! (to implement user-defined CTOR)
  interface Vec2d
     module procedure createVec2d
  end interface Vec2d

contains
  type(Vec2d) function createVec2d( u, v ) ! CTOR
    real, intent(in) :: u, v
    createVec2d%mU = u
    createVec2d%mV = v
  end function createVec2d

  subroutine initVec2d( this, u, v ) ! init-subroutine
    class(Vec2d), intent(inout) :: this
    real, intent(in) :: u, v
    ! copy-over data inside the object
    this%mU = u
    this%mV = v
  end subroutine initVec2d

  real function getUVec2d( this ) ! accessor-method (GETter)
    class(Vec2d), intent(in) :: this
    getUVec2d = this%mU ! direct-access IS allowed here
  end function getUVec2d

  real function getVVec2d( this ) ! accessor-method (GETter)
    class(Vec2d), intent(in) :: this
    getVVec2d = this%mV
  end function getVVec2d

  real function getMagnitudeVec2d( this ) result(mag)
    class(Vec2d), intent(in) :: this
    mag = sqrt( this%mU**2 + this%mV**2 )
  end function getMagnitudeVec2d
end module Vec2d_class

program test_driver_c
  use Vec2d_class
  implicit none

  type(Vec2d) :: A, B

  A = Vec2d(u=1.1, v=9.4) ! init via custom-CTOR
  call B%init(u=1.1, v=9.4) ! init via subroutine-call

  ! Accessing components of DT through methods (type-bound procedures).
  write(*, '(3(a,1x,f0.3))') "A%U =", A%getU(), &
       ", A%V =", A%getV(), ", A%magnitude =", A%getMagnitude()

  write(*, '(3(a,1x,f0.3))') "B%U =", B%getU(), &
       ", B%V =", B%getV(), ", B%magnitude =", B%getMagnitude()
end program test_driver_c
