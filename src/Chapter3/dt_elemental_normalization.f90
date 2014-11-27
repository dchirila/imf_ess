! File: dt_elemental_normalization.f90
! Purpose: Demonstrate elemental procedures, for normalizing vectors of type
!          'Vec3d'.
! NOTE: The elemental function can be applied to a single vector, as well as to
!       arrays of such vector-elements (in which case, another array of same
!       shape will be produced, where elements in 'out' are element-wise
!       application of the normalization-function to corresponding elements in
!       'in').

module Vec3d_class
  implicit none
  private
  public :: normalize ! Expose the elemental function.

  type, public :: Vec3d
     real :: mU = 0., mV = 0., mW = 0.
  end type Vec3d

contains
  type(Vec3d) elemental function normalize( this )
    type(Vec3d), intent(in) :: this
    ! Local variable (note that the 'getMagnitude'-method could also be called,
    ! but we do not have it implemented here, for brevity).
    real :: magnitude
    magnitude = sqrt( this%mU**2 + this%mV**2 + this%mW**2 )
    normalize%mU = this%mU / magnitude
    normalize%mV = this%mV / magnitude
    normalize%mW = this%mW / magnitude
  end function normalize
end module Vec3d_class

program test_elemental
  use Vec3d_class
  implicit none

  type(Vec3d) :: scalarIn, array1In(10), array2In(15, 20)
  type(Vec3d) :: scalarOut, array1Out(10), array2Out(15, 20)

  ! Place some values in the 'in'-variables...
  scalarOut = normalize( scalarIn ) ! Apply normalize to scalar
  array1Out = normalize( array1In ) ! Apply normalize to rank-1 array
  array2Out = normalize( array2In ) ! Apply normalize to rank-2 array
end program test_elemental
