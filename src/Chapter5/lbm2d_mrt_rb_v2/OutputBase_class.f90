module OutputBase_class
  use NumericKinds, only : IK, RK
  implicit none

  ! string-constants for output metadata
  character(len=*), parameter :: UNITS_STR="units", & ! for global-attribute
       SPACE_UNITS_STR="char. length", TIME_UNITS_STR="char. time", &
       PRESS_UNITS_STR="char. pressure-difference", &
       VEL_UNITS_STR="char. velocity", TEMP_UNITS_STR="char. temperature-difference"

  type :: OutputBase
     real(RK) :: mUyMax
     character(len=256) :: mOutFilePrefix

     ! information about the simulation
     integer(IK) :: mNx, mNy, mNumOutSlices, mNumItersMax, mOutDelay, mOutInterv
     real(RK) :: mDxD, mDtD, mRa, mPr, mMaxMach
     integer(IK) :: mCurrOutSlice ! for tracking output time-slices

     ! arrays for coordinates along each dimension (space & time)
     real(RK), dimension(:), allocatable :: mXVals, mYVals, &
          mTVals ! 1st output-slice ~ t=0 (ICs)

     ! conversion-factors for translating output from numerical- to
     ! dimensionless-units
     real(RK) :: mDRhoSolver2PressDimless, mVelSolver2VelDimless
   contains
     private
     procedure, public :: init => initOutputBase
     procedure, public :: cleanup => cleanupOutputBase
     procedure, public :: isActive => isActiveOutputBase
     procedure, public :: isTimeToWrite => isTimeToWriteOutputBase
  end type OutputBase
contains

  subroutine initOutputBase( this, nX, nY, numOutSlices, dxD, dtD, &
       nItersMax, outFilePrefix, Ra, Pr, maxMach )
    class(OutputBase), intent(inout) :: this
    integer(IK), intent(in) :: nX, nY, nItersMax, numOutSlices
    real(RK), intent(in) :: dxD, dtD, Ra, Pr, maxMach
    character(len=*), intent(in) :: outFilePrefix
    ! local vars
    integer(IK) :: x, y, t, tOut

    if( numOutSlices < 0 ) then
       this%mNumOutSlices = nItersMax + 1 ! write everything
    else
       this%mNumOutSlices = numOutSlices
    end if

    if( this%isActive() ) then ! prepare output only if actually writing
       ! copy over remaining arguments into internal-state
       this%mNx = nX; this%mNy = nY
       this%mNumItersMax = nItersMax
       this%mDxD = dxD; this%mDtD = dtD
       this%mRa = Ra; this%mPr = Pr; this%mMaxMach = maxMach
       this%mOutFilePrefix = outFilePrefix

       ! conversion-factors for output
       this%mVelSolver2VelDimless = dxD / dtD
       this%mDRhoSolver2PressDimless = this%mVelSolver2VelDimless**2 / 3._RK

       ! get memory for (dimensionless) coordinate-arrays
       allocate( this%mXVals(nX), this%mYVals(nY), this%mTVals(this%mNumOutSlices) )

       ! Enforce safety-check: cannot request more output-slices than nItersMax!
       if( this%mNumOutSlices > (this%mNumItersMax+1) ) then
          write(*,'(3(a,/),a)') "ERROR: invalid combination of output-parameters", &
               "Cause: numOutSlices too high for computed number of iterations!", &
               "Fixes: decrease numOutSlices OR increase simTime", &
               "Aborting..."
          stop
       elseif( this%mNumOutSlices <= 0 ) then
       end if

       if( this%mNumOutSlices > 1 ) then ! avoid divide-by-zero if writing only ICs
          ! write output (mostly) every 'mOutInterv' iters...
          this%mOutInterv = this%mNumItersMax / (this%mNumOutSlices-1)
          ! ...except in the beginning
          this%mOutDelay = mod( this%mNumItersMax, this%mNumOutSlices-1 ) + 1
       else ! mNumOutSlices can only be 1
          this%mOutInterv = 1
       end if

       ! fill-in coordinate-arrays...
       ! ...X-dimension
       do x=1, this%mNx
          this%mXVals(x) = real( dxD*(x-0.5) )
       end do
       ! ...Y-dimension
       do y=1, this%mNy
          this%mYVals(y) = real( dxD*(y-0.5) )
       end do
       ! ...time-dimension
       ! NOTE: the time-delay between the first two output-slices is different from
       ! the subsequent ones, because:
       ! a) the ICs are written as 1st output-slice and
       ! b) the total number of model-iterations is not necessarily a multiple of
       ! the number of output-slices requested by the user
       this%mTVals(1) = real(0) ! ICs
       if( this%mNumOutSlices > 1 ) then
          tOut = this%mNumOutSlices
          do t=this%mNumItersMax, this%mOutDelay, -this%mOutInterv
             this%mTVals(tOut) = real( (t-0.5)*dtD )
             tOut = tOut-1 ! decrement time-slice index
          end do
       end if

       this%mCurrOutSlice = 0
    else
       write(*,'(a)') "INFO: no file-output, due to chosen 'numOutSlices'"
    end if
  end subroutine initOutputBase

  subroutine cleanupOutputBase( this )
    class(OutputBase), intent(inout) :: this
    if( this%isActive() ) then
       deallocate( this%mXVals, this%mYVals, this%mTVals )
    end if
  end subroutine cleanupOutputBase

  logical function isActiveOutputBase( this )
    class(OutputBase), intent(in) :: this
    isActiveOutputBase = ( this%mNumOutSlices > 0 )
  end function isActiveOutputBase

  ! Implement criterion for determining at which iterations to write output,
  ! based on the number of time-slices requested by user, subject to the
  ! constraints of:
  ! a) writing the ICs
  ! b) writing the last iteration (since we made the effort to compute so far in
  ! the first place)
  ! c) having equidistant (in time) output-slices (except for the initial delay)
  logical function isTimeToWriteOutputBase( this, iterNum )
    class(OutputBase), intent(in) :: this
    integer(IK), intent(in) :: iterNum

    if( this%isActive() ) then
       isTimeToWriteOutputBase = (iterNum==0) .or. ( &
            (this%mNumOutSlices /= 1) .and. &
            (iterNum >= this%mOutDelay) .and. &
            (mod(this%mNumItersMax-iterNum, this%mOutInterv) == 0) )
    else
       isTimeToWriteOutputBase = .false.
    end if
  end function isTimeToWriteOutputBase
end module OutputBase_class
