module GeomUtils
  use NumericKinds
  use PhysicsConstants
  implicit none
  public

contains
  ! Convert radians to degrees
  real(RK) function rad2Deg( radians )
    real(RK), intent(in) :: radians
    rad2Deg = radians / ONE_DEG_IN_RADS
  end function rad2Deg

  ! Convert degrees to radians
  real(RK) function deg2Rad( degrees )
    real(RK), intent(in) :: degrees
    deg2Rad = degrees * ONE_DEG_IN_RADS
  end function deg2Rad

  ! Sine of an angle given in degrees
  real(RK) function sinD( degrees )
    real(RK), intent(in) :: degrees
    sinD = sin( deg2Rad( degrees ) )
  end function sinD

  ! Cosine of an angle given in degrees
  real(RK) function cosD( degrees )
    real(RK), intent(in) :: degrees
    cosD = cos( deg2Rad( degrees ) )
  end function cosD

  ! Estimate y(x), given 2 points (x0,y0) & (x1,y1).
  real(RK) function linInterp( x0, y0, x1, y1, x )
    real(RK), intent(in) :: x0, y0, x1, y1, x
    ! Check for malformed-denominator in division.
    ! NOTE: More robust option is to use IEEE features of
    ! Fortran 2003 (see e.g. Clerman 2011).
    if( abs(x1-x0) < (epsilon(x)*abs((x - x0))) ) then
       stop("FP error in linInterp-function! Aborting.")
    end if
    ! Actual computation
    linInterp = y0 + (x - x0)/(x1 - x0)*(y1 - y0)
  end function linInterp
end module GeomUtils

