! File: overload_intrinsic_operators.f90
! Purpose: Demonstrate how operator-overloading (with an intrinsic operator) can
!          be implemented, for a simplified version of our 'Vec3d' data-type.
! NOTE: In real-world applications, it is better to make the components of the
!       data-type 'private', using the techniques we discussed. Here, we omit
!       this, for brevity.

module Vec3d_class
  implicit none

  type, public :: Vec3d
     real :: mU = 0., mV = 0., mW = 0. ! Make 'private' in practice!
   contains
     procedure :: display ! Convenience output-method.
  end type Vec3d

  ! Generic interface, for operator-overloading.
  interface operator(-)
     module procedure negate   ! unary-minus
     module procedure subtract ! binary-subtraction
  end interface operator(-)

contains
  type(Vec3d) function negate( inVec )
    class(Vec3d), intent(in) :: inVec
    negate%mU = -inVec%mU
    negate%mV = -inVec%mV
    negate%mW = -inVec%mW
  end function negate

  ! NOTE: it is also possible to overload binary operators with heterogeneous
  ! data-types. In our case, we could devine two more overloads for
  ! binary-'-', to support subtraction when inVec1 or inVec2 is a scalar. In that
  ! case, only the type of inVec1 or inVec2 needs to change, and the code inside
  ! the function to be adapted.
  type(Vec3d) function subtract( inVec1, inVec2 )
    class(Vec3d), intent(in) :: inVec1, inVec2
    subtract%mU = inVec1%mU - inVec2%mU
    subtract%mV = inVec1%mV - inVec2%mV
    subtract%mW = inVec1%mW - inVec2%mW
  end function subtract

  ! Utility-method, for more convenient display of 'Vec3d'-elements.
  ! NOTE: A better solution is to use I/O for derived-types (see Metcalf2011).
  subroutine display( this, nameString )
    class(Vec3d), intent(in) :: this
    character(len=*), intent(in) :: nameString
    write(*,'(2a,3(f0.2,2x),a)') &
         trim(nameString), " = ( ", this%mU, this%mV, this%mW, ")"
  end subroutine display
end module Vec3d_class

program test_overload_intrinsic_operators
  use Vec3d_class
  implicit none
  type(Vec3d) :: A = Vec3d(2., 4., 6.), B = Vec3d(1., 2., 3.)

  write(*,'(/,a)') "initial-state:"
  call A%display("A"); call B%display("B")

  A = -A
  write(*,'(/,a)') 'after operation "A = -A":'
  call A%display("A"); call B%display("B")

  A = A - B
  write(*,'(/,a)') 'after operations "A = A - B":'
  call A%display("A"); call B%display("B")
end program test_overload_intrinsic_operators

