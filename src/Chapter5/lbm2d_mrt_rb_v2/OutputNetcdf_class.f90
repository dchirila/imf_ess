! File: OutputNetcdf_class.f90
! Author: Dragos B. Chirila
! Purpose: Module for the 'OutputNetcdf' derived-type (C++ "class"), for Section
!          5.2 in the book; this demonstrates how to create a new netCDF
!          dataset from scratch.
! NOTE(S): - can ignore the lines starting with '! .......'

module OutputNetcdf_class
  use NumericKinds, only : IK, I3B, RK
  use OutputBase_class
  use netcdf
  implicit none

  type, extends(OutputBase) :: OutputNetcdf
     private
     ! internal handlers for netCDF objects
     integer(I3B) :: mNcID, mPressVarID, mUxVarID, mUyVarID, mTempVarID, &
          mUyMaxVarID, mTimeVarID
   contains
     private
     ! public methods which differ from base-class analogues
     procedure, public :: init => initOutputNetcdf
     procedure, public :: writeOutput => writeOutputNetcdf
     procedure, public :: cleanup => cleanupOutputNetcdf
     ! internal method
     procedure prepareFileOutputNetcdf
  end type OutputNetcdf
! ............... (continues below) ......

! ............... (continued from above) ......
contains
  subroutine initOutputNetcdf( this, nX, nY, numOutSlices, dxD, dtD, &
       nItersMax, outFilePrefix, Ra, Pr, maxMach )
    class(OutputNetcdf), intent(inout) :: this
    integer(IK), intent(in) :: nX, nY, numOutSlices, nItersMax
    real(RK), intent(in) :: dxD, dtD, Ra, Pr, maxMach
    character(len=*), intent(in) :: outFilePrefix

    ! initialize parent-type
    call this%OutputBase%init( nX, nY, numOutSlices, dxD, dtD, &
         nItersMax, outFilePrefix, Ra, Pr, maxMach )

    if( this%isActive() ) then
       call this%prepareFileOutputNetcdf()
    end if
  end subroutine initOutputNetcdf
! ............... (continues below) ......

! ............... (continued from above) ......
subroutine prepareFileOutputNetcdf( this )
  class(OutputNetcdf), intent(inout) :: this

  ! Variables to store temporary IDs returned by the netCDF library; no need
  ! to save these, since they are only needed when writing the file-header.
  ! NOTES: - we have 3 dimension IDs (2D=space + 1D=time)
  !        - HOWEVER, there is no 'tVarID', since this ID is needed later (to
  !          append values to this UNLIMITED-axis), so it is stored in the
  !          internal state of the type (in 'mTimeVarID')
  integer(I3B) :: dimIDs(3), xDimID, yDimID, tDimID, &
       xVarID, yVarID

  ! create the netCDF-file (NF90_CLOBBER overwrites file if it already exists,
  ! while NF90_64BIT_OFFSET enables 64bit-offset mode)
  call ncCheck( nf90_create( &
       path=trim(adjustl(this%mOutFilePrefix)) // ".nc", &
       cmode=ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncid=this%mNcID) )

  ! global attributes
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, "Conventions", "CF-1.6") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, SPACE_UNITS_STR, &
       "channel height $L$") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, TIME_UNITS_STR, &
       "diffusive time-scale $\frac{L^2}{\kappa}$") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, PRESS_UNITS_STR, &
       "$\frac{\rho_0\kappa^2}{L^2}$") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, VEL_UNITS_STR, &
       "$\frac{\kappa}{L}$") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, TEMP_UNITS_STR, &
       "temperature-difference between horizontal walls $\theta_b-\theta_t$") )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, "Ra", this%mRa) )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, "Pr", this%mPr) )
  call ncCheck( nf90_put_att(this%mNcID, NF90_GLOBAL, "maxMach", this%mMaxMach) )

  ! define dimensions (netCDF will return ID for each)
  call ncCheck( nf90_def_dim(this%mNcID, "x", this%mNx, xDimID) )
  call ncCheck( nf90_def_dim(this%mNcID, "y", this%mNy, yDimID) )
  call ncCheck( nf90_def_dim(this%mNcID, "t", NF90_UNLIMITED, tDimID) )
  ! define coordinates
  call ncCheck( nf90_def_var(this%mNcID, "x", NF90_REAL, xDimID, xVarID) )
  call ncCheck( nf90_def_var(this%mNcID, "y", NF90_REAL, yDimID, yVarID) )
  call ncCheck( nf90_def_var(this%mNcID, "t", NF90_REAL, tDimID, &
       this%mTimeVarID) )
  ! assign units-attributes to coordinate vars
  call ncCheck( nf90_put_att(this%mNcID, xVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, xVarID, "long_name", SPACE_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, yVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, yVarID, "long_name", SPACE_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, this%mTimeVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mTimeVarID, "long_name", &
       TIME_UNITS_STR) )

  ! dimIDs-array is used for passing the IDs corresponding to the dimensions
  ! of the variables
  dimIDs = [ xDimID, yDimID, tDimID ]

  ! define the variables: to save space, we store most results as NF90_REAL;
  ! however, for the 'mUyMax'-field, we need NF90_DOUBLE, to distinguish the
  ! 1st bifurcation in the Rayleigh-Benard system
  call ncCheck( &
       nf90_def_var(this%mNcID, "press_diff", NF90_REAL, dimIDs, this%mPressVarID))
  call ncCheck( &
       nf90_def_var(this%mNcID, "temp_diff", NF90_REAL, dimIDs, this%mTempVarID))
  call ncCheck( &
       nf90_def_var(this%mNcID, "u_x", NF90_REAL, dimIDs, this%mUxVarID))
  call ncCheck( &
       nf90_def_var(this%mNcID, "u_y", NF90_REAL, dimIDs, this%mUyVarID))
  call ncCheck( &
       nf90_def_var(this%mNcID, "max_u_y", NF90_DOUBLE, tDimID, this%mUyMaxVarID))

  ! assign units-attributes to output-variables
  call ncCheck( nf90_put_att(this%mNcID, this%mPressVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mPressVarID, "long_name", &
       PRESS_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, this%mTempVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mTempVarID, "long_name", &
       TEMP_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, this%mUxVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mUxVarID, "long_name", &
       VEL_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, this%mUyVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mUyVarID, "long_name", &
       VEL_UNITS_STR) )
  call ncCheck( nf90_put_att(this%mNcID, this%mUyMaxVarID, "units", "1") )
  call ncCheck( nf90_put_att(this%mNcID, this%mUyMaxVarID, "long_name", &
       VEL_UNITS_STR) )

  ! end define-mode (informs netCDF we finished defining metadata)
  call ncCheck( nf90_enddef(this%mNcID) )

  ! write data (but only for coordinates which are NOT UNLIMITED)
  call ncCheck( nf90_put_var(this%mNcID, xVarID, this%mXVals) )
  call ncCheck( nf90_put_var(this%mNcID, yVarID, this%mYVals) )
end subroutine prepareFileOutputNetcdf
! ............... (continues below) ......

! ............... (continued from above) ......
subroutine writeOutputNetcdf( this, rawMacros, iterNum )
  class(OutputNetcdf), intent(inout) :: this
  real(RK), dimension(:, :, 0:), intent(in) :: rawMacros
  integer(IK), intent(in) :: iterNum
  ! local variables
  real(RK) :: currTime

  if( this%isTimeToWrite( iterNum ) ) then
     ! increment output time-slice if it is time to generate output
     this%mCurrOutSlice = this%mCurrOutSlice + 1

     ! Evaluate current dimensionless-time (0.5 due to Strang-splitting)
     if( iterNum == 0 ) then
        currTime = 0._RK
     else
        currTime = (iterNum-0.5_RK)*this%mDtD
     end if
     ! append value to UNLIMITED time-dimension
     call ncCheck( nf90_put_var( this%mNcID, this%mTimeVarID, &
          values=currTime, start=[this%mCurrOutSlice]) )

     this%mUyMax = maxval( abs(rawMacros(:, :, 1)) )

     ! write data (scaled to dimensionless units) to file
     ! - dimensionless pressure-difference
     call ncCheck( nf90_put_var( this%mNcID, this%mPressVarID, &
        values=rawMacros(:,:,0)*this%mDRhoSolver2PressDimless, &
        start=[1, 1, this%mCurrOutSlice], count=[this%mNx, this%mNy, 1]) )
     ! - dimensionless temperature-difference
     call ncCheck( nf90_put_var( this%mNcID, this%mTempVarID, &
        values=rawMacros(:,:,3), &
        start=[1, 1, this%mCurrOutSlice], count=[this%mNx, this%mNy, 1]) )
     ! - dimensionless Ux
     call ncCheck( nf90_put_var( this%mNcID, this%mUxVarID, &
        values=rawMacros(:,:,1)*this%mVelSolver2VelDimless, &
        start=[1, 1, this%mCurrOutSlice], count=[this%mNx, this%mNy, 1]) )
     ! - dimensionless Uy
     call ncCheck( nf90_put_var( this%mNcID, this%mUyVarID, &
        values=rawMacros(:,:,2)*this%mVelSolver2VelDimless, &
        start=[1, 1, this%mCurrOutSlice], count=[this%mNx, this%mNy, 1]) )
     ! - max<Uy> (for bifurcation test criterion)
     call ncCheck( nf90_put_var( this%mNcID, this%mUyMaxVarID, &
        values=this%mUyMax*this%mVelSolver2VelDimless, start=[this%mCurrOutSlice]))
  end if
end subroutine writeOutputNetcdf
! ............... (continues below) ......

! ............... (continued from above) ......
subroutine cleanupOutputNetcdf( this )
  class(OutputNetcdf), intent(inout) :: this
  if( this%isActive() ) then
     call ncCheck( nf90_close(this%mNcID) )
     call this%OutputBase%cleanup()
  end if
end subroutine cleanupOutputNetcdf
! ...............

! error-checking wrapper for netCDF operations
subroutine ncCheck( status )
  ! ...............
  integer(I3B), intent(in) :: status

  if( status /= nf90_noerr ) then
     write(*,'(a)') trim( nf90_strerror(status) )
     stop "ERROR (netCDF-related) See message above for details! Aborting..."
  end if
end subroutine ncCheck
end module OutputNetcdf_class
