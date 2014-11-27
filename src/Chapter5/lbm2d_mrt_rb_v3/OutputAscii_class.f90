! NOTE: for brevity, this output-writer only creates files for UyMax
! (single-file per simulation) and temperature-anomaly (one file for each output
! time-slice).
module OutputAscii_class
  use NumericKinds, only : IK, RK, RK_FMT
  use OutputBase_class
  implicit none

  type, extends(OutputBase) :: OutputAscii
     private
     integer(IK) :: mSummaryFileUnit, mTempFileUnit
     character(len=256) :: mSummaryFileName, mTempFileName, &
          mFmtStrngFieldFileNames
   contains
     private
     ! public methods which differ from base-class analogues
     procedure, public :: init => initOutputAscii
     procedure, public :: writeOutput => writeOutputAscii
     procedure, public :: cleanup => cleanupOutputAscii
     ! internal method(s)
     procedure writeSummaryFileHeaderOutputAscii
  end type OutputAscii
contains
  subroutine initOutputAscii( this, nX, nY, numOutSlices, dxD, dtD, &
       nItersMax, outFilePrefix, Ra, Pr, maxMach )
    class(OutputAscii), intent(inout) :: this
    integer(IK), intent(in) :: nX, nY, nItersMax, numOutSlices
    real(RK), intent(in) :: dxD, dtD, Ra, Pr, maxMach
    character(len=*), intent(in) :: outFilePrefix
    ! .............................
    ! local
    integer(IK) :: numDigitsSlices

    ! initialize parent-type
    call this%OutputBase%init( nX, nY, numOutSlices, dxD, dtD, &
         nItersMax, outFilePrefix, Ra, Pr, maxMach )

    if( this%isActive() ) then
       this%mSummaryFileName = trim(adjustl(this%mOutFilePrefix)) // "_summary.dat"

       ! open simulation's summary-file (also contains UyMax-series)
       open(newunit=this%mSummaryFileUnit, file=this%mSummaryFileName, &
            status='replace', action='write' )
       ! add metadata to summary-file
       call this%writeSummaryFileHeaderOutputAscii( this%mSummaryFileUnit )

       ! determine how wide the part of the field file-names encoding the
       ! time-slice number needs to be...
       numDigitsSlices = aint(log10(real(this%mNumOutSlices))) + 1
       ! ...and create appropriate format-string
       write(this%mFmtStrngFieldFileNames, '(a, i0, a)') "(a, i0.", numDigitsSlices, ", a)"
    end if
  end subroutine initOutputAscii

  subroutine writeSummaryFileHeaderOutputAscii( this, fileUnit )
    class(OutputAscii), intent(inout) :: this
    integer(IK), intent(in) :: fileUnit
    ! .............................
    ! local
    integer(IK) :: x, y, t

    write(fileUnit, '(i0,2x,a)') this%mNx, "# Nx"
    write(fileUnit, '(i0,2x,a)') this%mNy, "# Ny"
    write(fileUnit, '(i0,2x,a)') this%mNumOutSlices, "# Nt"
    write(fileUnit, '(' // RK_FMT // ',2x,a)') this%mRa, "# Ra"
    write(fileUnit, '(' // RK_FMT // ',2x,a)') this%mPr, "# Pr"
    write(fileUnit, '(' // RK_FMT // ',2x,a)') this%mMaxMach, "# maxMach"
    ! write units
    write(fileUnit, '(a,2x,a)') '"' // SPACE_UNITS_STR // '"', &
         "# X-units"
    write(fileUnit, '(a,2x,a)') '"' // SPACE_UNITS_STR // '"', &
         "# Y-units"
    write(fileUnit, '(a,2x,a)') '"' // TIME_UNITS_STR // '"', &
         "# time-units"
    write(fileUnit, '(a,2x,a)') '"' // VEL_UNITS_STR // '"', &
         "# velocity-units"
    ! write dimension-vectors...
    ! ...X
    do x=1, this%mNx
       write(fileUnit, '(' // RK_FMT // ',2x)', advance='no') this%mXVals(x)
    end do
    write(fileUnit, '(a)') "# XVals"
    ! ...Y
    do y=1, this%mNy
       write(fileUnit, '(' // RK_FMT // ',2x)', advance='no') this%mYVals(y)
    end do
    write(fileUnit, '(a)') "# YVals"
    ! ...time
    do t=1, this%mNumOutSlices
       write(fileUnit, '(' // RK_FMT // ',2x)', advance='no') this%mTVals(t)
    end do
    write(fileUnit, '(a)') "# TimeVals"
  end subroutine writeSummaryFileHeaderOutputAscii

  subroutine writeOutputAscii( this, rawMacros, iterNum )
    class(OutputAscii), intent(inout) :: this
    real(RK), dimension(:, :, 0:), intent(in) :: rawMacros
    integer(IK), intent(in) :: iterNum
    ! .............................
    ! local vars
    integer(IK) :: x, y

    if( this%isTimeToWrite( iterNum ) ) then
       ! increment output time-slice if it is time to generate output
       this%mCurrOutSlice = this%mCurrOutSlice + 1

       this%mUyMax = maxval( abs(rawMacros(:, :, 1)) )

       ! write UyMax to its file
       write(this%mSummaryFileUnit, '(' // RK_FMT // ',2x)', advance='no') this%mUyMax

       ! generate file-name for temperature-anomaly at this time-slice
       write(this%mTempFileName, fmt=this%mFmtStrngFieldFileNames) &
            trim(adjustl(this%mOutFilePrefix)) // "_temp_diff_", &
            this%mCurrOutSlice, ".dat"
       ! open file for writing
       open(newunit=this%mTempFileUnit, file=this%mTempFileName, status='replace', action='write')
       ! write field-file metadata
       write(this%mTempFileUnit, '(a)') '# NOTE: see file "' // &
            trim(adjustl(this%mSummaryFileName)) // &
            '" for information about simulation-parameters'
       write(this%mTempFileUnit, '(' // RK_FMT // ',2x,a)') &
            this%mTVals(this%mCurrOutSlice), &
            "# dimensionless-time for this file"
       write(this%mTempFileUnit, '(a,2x,a)') &
            '"' // TEMP_UNITS_STR // '"', "# temperature-units"
       ! write field-data to file
       do y=1, this%mNy
          do x=1, this%mNx
             write(this%mTempFileUnit, '(2x,' // RK_FMT // ')', advance='no') rawMacros(x, y, 3)
          end do
          write(this%mTempFileUnit, *)
       end do
       ! close file for temperature-anomaly @ this time-slice
       close(this%mTempFileUnit)
    end if
  end subroutine writeOutputAscii

  subroutine cleanupOutputAscii( this )
    class(OutputAscii), intent(inout) :: this
    if( this%isActive() ) then
       write(this%mSummaryFileUnit, '(a)') " # UyMax"
       close(this%mSummaryFileUnit)
       call this%OutputBase%cleanup()
    end if
  end subroutine cleanupOutputAscii
end module OutputAscii_class

