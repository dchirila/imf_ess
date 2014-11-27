! File: overload_custom_operator.f90
! Purpose: Demonstrate how operator-overloading (with a custom operator-name)
!          can be implemented, for a simplified version of our 'Vec3d'
!          data-type.
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
  interface operator(.cross.)
     module procedure cross_product ! binary
  end interface operator(.cross.)

contains
  type(Vec3d) function cross_product( inVec1, inVec2 )
    class(Vec3d), intent(in) :: inVec1, inVec2
    cross_product%mU = inVec1%mV*inVec2%mW - inVec1%mW*inVec2%mV
    cross_product%mV = inVec1%mW*inVec2%mU - inVec1%mU*inVec2%mW
    cross_product%mW = inVec1%mU*inVec2%mV - inVec1%mV*inVec2%mU
  end function cross_product

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
  type(Vec3d) :: A = Vec3d(2., 4., 6.), B = Vec3d(3., 2., 1.), C

  write(*,'(/,a)') "initial-state:"
  call A%display("A"); call B%display("B"); call C%display("C")

  C = A .cross. B
  write(*,'(/,a)') 'after assigning "C = A .cross. B":'
  call A%display("A"); call B%display("B"); call C%display("C")
end program test_overload_intrinsic_operators

