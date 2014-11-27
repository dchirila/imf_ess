! File: lbm2d_mrt_rb_v1.f90
!
! Purpose: Example program for solving the 2D incompressible Navier-Stokes
! equation (INSE), with a thermal component coupled using the Boussinesq
! approximation. For simulating the evolution of the (athermal) fluid, and of
! the temperature-field, we use the lattice Boltzmann method (LBM), with
! multi-relaxation-time (MRT) collision operators.
!
! Geometry: rectangular channel, with aspect-ratio
! $\gamma \equiv \frac{L_x}{L_y} = 2.0158$
!
! Boundary Conditions (BCs):
! - periodic domain-wrapping along X-direction (for both fluid- and
!   temperature-solvers)
! - fluid: no-slip (achieved through "bounce-back") at top and bottom walls
! - temperature: constant temperature (achieved through "anti-bounce-back") at
!   top and bottom walls; the lower wall is maintained at a higher temperature,
!   to allow convection to occur
!
! Initial Conditions (ICs):
! - fluid: at rest and without any pressure-gradients
! - temperature: linear temperature-profile (to speed-up the simulations), plus
!   a small perturbation near the center of the bottom wall, to break the
!   symmetry.
!
! Author: Dragos B. Chirila
!
! Version Information: This is a basic version, with ASCII-output and no
! parallelization, for Section 4.3 in the book.
!
! References:
! - [Rayleigh1916] "On convection currents in a horizontal layer of fluid, when
! the higher temperature is on the under side", Lord Rayleigh, 1916, Philosophical
! Magazine Series 6, 32(192)
! - [Wang2013] "Lattice Boltzmann simulations of thermal convective flows in two
! dimensions", J. Wang et al., 2013, Computers & Mathematics with Applications,
! 65(2)
! - [Shan19917] "Simulation of Rayleigh-Benard convection using a lattice
! Boltzmann method", 1997, Physical Review E, 55(3)

module NumericKinds
  implicit none

  ! KINDs for different types of REALs
  ! .............................
  integer, parameter :: &
       R_SP = selected_real_kind(  6,   37 ), &
       R_DP = selected_real_kind( 15,  307 ), &
       R_QP = selected_real_kind( 33, 4931 )
  ! Alias for precision that we use in the program (change this to any of the
  ! values 'R_SP', 'R_DP', or 'R_QP', to switch to another precision globally).
  integer, parameter :: RK = R_DP ! if changing this, also change RK_FMT

  ! KINDs for different types of INTEGERs
  ! .............................
  integer, parameter :: &
       I1B = selected_int_kind(2), & ! max = 127
       I2B = selected_int_kind(4), & ! max ~ 3.28x10^4
       I3B = selected_int_kind(9), & ! max ~ 2.15x10^9
       I4B = selected_int_kind(18)   ! max ~ 9.22x10^18
  ! Alias for integer-precision (analogue role to RK above).
  integer, parameter :: IK = I3B

  ! Edit-descriptors for real-values
  character(len=*), parameter :: R_SP_FMT = "f0.6", &
       R_DP_FMT = "f0.15", R_QP_FMT = "f0.33"
  ! Alias for output-precision to use in the program (keep this in sync with RK)
  character(len=*), parameter :: RK_FMT = R_DP_FMT

  interface swap ! generic IFACE
     module procedure swapRealRK, swapIntIK
  end interface swap
contains

  elemental subroutine swapRealRK( a, b )
    real(RK), intent(inout) :: a, b
    real(RK) :: tmp
    tmp = a; a = b; b = tmp
  end subroutine swapRealRK

  elemental subroutine swapIntIK( a, b )
    integer(IK), intent(inout) :: a, b
    integer(IK) :: tmp
    tmp = a; a = b; b = tmp
  end subroutine swapIntIK
end module NumericKinds

module LbmConstantsMrtD2Q5
  use NumericKinds, only : IK, RK
  implicit none

  integer(IK), dimension(2, 0:4), parameter :: EV_TEMP = reshape( &
       source = [ &
       0,   0, &
       1,   0, &
       0,   1, &
      -1,   0, &
       0,  -1],&
       shape = [2, 5])

  integer(IK), dimension(0:4), parameter :: OPPOSITE_TEMP = &
       [0, 3, 4, 1, 2]

  real(RK), dimension(0:4, 0:4), parameter :: N_TEMP = reshape( &
       source = [ &
       1,  1,  1,  1,  1, &
       0,  1,  0, -1,  0, &
       0,  0,  1,  0, -1, &
      -4,  1,  1,  1,  1, &
       0,  1, -1,  1, -1],&
       shape = [5, 5])

  ! NOTE: the inverse can also be computed programatically as:
  !       $N \times (N \times N^T)^{-1}$.
  !       However, we hard-code it here, to make this matrix a constant too
  !       (which enables some code-optimization by the compilers).
  real(RK), dimension(0:4, 0:4), parameter :: N_INV_TEMP = reshape( &
       source = [ &
       4,   0,   0,  -4,   0, &
       4,  10,   0,   1,   5, &
       4,   0,  10,   1,  -5, &
       4, -10,   0,   1,   5, &
       4,   0, -10,   1,  -5],&
       shape = [5, 5]) / 20.0
end module LbmConstantsMrtD2Q5

module LbmConstantsMrtD2Q9
  use NumericKinds, only : IK, RK
  implicit none

  integer(IK), dimension(2, 0:8), parameter :: EV_FLUID = reshape( &
       source = [ &
       0,   0, &
       1,   0, &
       0,   1, &
      -1,   0, &
       0,  -1, &
       1,   1, &
      -1,   1, &
      -1,  -1, &
       1,  -1],&
       shape = [2, 9])

  integer(IK), dimension(0:8), parameter :: OPPOSITE_FLUID = &
       [0, 3, 4, 1, 2, 7, 8, 5, 6]

  real(RK), dimension(0:8, 0:8), parameter :: M_FLUID = reshape( &
       source = [ &
       1,  1,  1,  1,  1,  1,  1,  1,  1, &
       0,  1,  0, -1,  0,  1, -1, -1,  1, &
       0,  0,  1,  0, -1,  1,  1, -1, -1, &
      -4, -1, -1, -1, -1,  2,  2,  2,  2, &
       0,  1, -1,  1, -1,  0,  0,  0,  0, &
       0,  0,  0,  0,  0,  1, -1,  1, -1, &
       0, -2,  0,  2,  0,  1, -1, -1,  1, &
       0,  0, -2,  0,  2,  1,  1, -1, -1, &
       4, -2, -2, -2, -2,  1,  1,  1,  1],&
       shape = [9, 9])

  ! NOTE: same comment as for N_INV_TEMP applies
  real(RK), dimension(0:8, 0:8), parameter :: M_INV_FLUID = reshape( &
       source = [ &
       4,  0,  0, -4,  0,  0,  0,  0,  4, &
       4,  6,  0, -1,  9,  0, -6,  0, -2, &
       4,  0,  6, -1, -9,  0,  0, -6, -2, &
       4, -6,  0, -1,  9,  0,  6,  0, -2, &
       4,  0, -6, -1, -9,  0,  0,  6, -2, &
       4,  6,  6,  2,  0,  9,  3,  3,  1, &
       4, -6,  6,  2,  0, -9, -3,  3,  1, &
       4, -6, -6,  2,  0,  9, -3, -3,  1, &
       4,  6, -6,  2,  0, -9,  3, -3,  1],&
       shape = [9, 9]) / 36.0
end module LbmConstantsMrtD2Q9

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
       allocate(this%mXVals(nX), this%mYVals(nY), this%mTVals(this%mNumOutSlices))

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

module MrtSolverBoussinesq2D_class
  use NumericKinds, only : IK, RK, swap
  use LbmConstantsMrtD2Q5
  use LbmConstantsMrtD2Q9
  implicit none

  type :: MrtSolverBoussinesq2D
     private
     ! parameters for the algorithm (not bound to the RB-setup)
     real(RK) :: mAlphaG, mAParam, mViscosity, mDiffusivity, &
          mTempColdWall, mTempHotWall, &
          ! for relaxation-matrices, we store only the non-zero part (= diagonals)
          mRelaxVecFluid(0:8), mRelaxVecTemp(0:4)

     ! internal model arrays
     ! NOTES: - last dimension is for 2-lattice alternation
     !        - 1st dimension: 0-8 = fluid, 9-13 = temp DFs
     real(RK), dimension(:,:,:,:), allocatable :: mDFs
     ! raw moments from which we can compute macroscopic fields; this is used
     ! mainly for simulation-output
     ! 0 ~ pressure| 1 ~ uX| 2 ~ uY| 3 ~ temp
     real(RK), dimension(:,:,:), allocatable :: mRawMacros

     integer(IK) :: mOld, mNew, & ! for tracking most recent lattice
          mNx, mNy ! mesh-size (received from 'sim'-class)

   contains
     private
     procedure, public :: init => initMrtSolverBoussinesq2D
     procedure, public :: advanceTime => advanceTimeMrtSolverBoussinesq2D
     procedure, public :: cleanup => cleanupMrtSolverBoussinesq2D
     procedure, public :: getRawMacros => getRawMacrosMrtSolverBoussinesq2D
     ! internal methods
     procedure :: calcLocalMomsMrtSolverBoussinesq2D
     procedure :: calcLocalEqMomsMrtSolverBoussinesq2D
  end type MrtSolverBoussinesq2D

contains
  function getRawMacrosMrtSolverBoussinesq2D( this ) result(macros)
    class(MrtSolverBoussinesq2D), intent(in) :: this
    real(RK), dimension(this%mNx, this%mNy, 0:3) :: macros
    macros = this%mRawMacros
  end function getRawMacrosMrtSolverBoussinesq2D

  subroutine initMrtSolverBoussinesq2D( this, nX, nY, tempColdWall, tempHotWall, &
       viscosity, diffusivity, alphaG, aParam, relaxVecFluid, relaxVecTemp )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    real(RK), intent(in) :: tempColdWall, tempHotWall, &
         viscosity, diffusivity, alphaG, aParam, &
         relaxVecFluid(0:8), relaxVecTemp(0:4)
    integer(IK), intent(in) :: nX, nY
    integer(IK) :: x, y, i ! dummy vars
    integer(IK), dimension(0:1) :: dest
    ! temporary moments-vars
    real(RK) :: fluidMoms(0:8), tempMoms(0:4), tempPerturbation

    ! copy argument-values internally
    this%mNx = nX; this%mNy = nY
    this%mTempColdWall = tempColdWall; this%mTempHotWall = tempHotWall
    this%mViscosity = viscosity; this%mDiffusivity = diffusivity
    this%mAlphaG = alphaG; this%mAParam = aParam
    this%mRelaxVecFluid = relaxVecFluid; this%mRelaxVecTemp = relaxVecTemp

    tempPerturbation=this%mTempHotWall/1.E5_RK

    ! get memory for model-state arrays (and Y-buffers)
    allocate( this%mDFs(0:13, 1:this%mNx, 0:(this%mNy+1), 0:1) )
    allocate( this%mRawMacros(this%mNx, this%mNy, 0:3) )

    ! initialize
    this%mDFs = 0._RK
    this%mRawMacros = 0._RK

    ! init tracking-vars for lattice-alternation
    this%mOld = 0; this%mNew = 1

    ! ICs for model's state-arrays
    do y=1, this%mNy
       do x=1, this%mNx
          ! reset moments-vectors
          fluidMoms = 0._RK; tempMoms = 0._RK
          ! Initialize pressure with steady-state (quadratic) profile, to avoid
          ! the initial oscillations.
          fluidMoms(0) = &
               (3._RK*this%mAlphaG)/(2._RK*this%mNy)*(y-0.5_RK)*(this%mNy+0.5_RK-y)

          ! Initialize temperature with steady-state (linear) profile, to save
          ! CPU-time. Also here, we insert small perturbation, to break the
          ! symmetry of the system (otherwise, the simulation is too stable).
          tempMoms(0) = 0.5_RK - (2._RK*y-1._RK)/(2._RK*this%mNy)
          if( (x == this%mNx/3+1) .and. (y == 2) ) then
             tempMoms(0) = tempMoms(0)+tempPerturbation
          end if

          ! map moments onto DFs...
          ! ...fluid
          do i=0, 8
            this%mDFs(i, x, y, this%mOld) = dot_product(M_INV_FLUID(:,i), fluidMoms)
          end do
          ! ...temp
          do i=0, 4
            this%mDFs(i+9, x, y, this%mOld) = dot_product(N_INV_TEMP(:,i), tempMoms)
          end do

          ! Fill buffers for bounce-back (for initial time-step)
          ! ...fluid
          do i=0, 8
             dest(0) = mod(x+EV_FLUID(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_FLUID(2, i)
             if( (dest(1) == 0) .or. (dest(1) == this%mNy+1) ) then
                this%mDFs(i, dest(0), dest(1), this%mOld) = &
                     this%mDFs(i, x, y, this%mOld)
             end if
          end do

          ! ...temp
          do i=0, 4
             dest(0) = mod(x+EV_TEMP(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_TEMP(2, i)
             if( (dest(1) == 0) .or. (dest(1) == this%mNy+1) ) then
                this%mDFs(i+9, dest(0), dest(1), this%mOld) = &
                     this%mDFs(i+9, x, y, this%mOld)
             end if
          end do

          ! save ICs
          this%mRawMacros(x, y, :) = [ fluidMoms(0:2), tempMoms(0) ]
       end do
    end do
  end subroutine initMrtSolverBoussinesq2D

  subroutine calcLocalMomsMrtSolverBoussinesq2D( this, x, y, fluidMoms, tempMoms )
    class(MrtSolverBoussinesq2D), intent(in) :: this
    integer(IK), intent(in) :: x, y
    real(RK), intent(out) :: fluidMoms(0:8), tempMoms(0:4)
    ! .............................
    integer(IK) :: i ! dummy index

    fluidMoms = 0._RK; tempMoms = 0._RK
    ! update fluid-moments
    do i=0, 8
       fluidMoms(i) = dot_product( M_FLUID(:,i), this%mDFs(0:8, x, y, this%mOld) )
    end do
    ! update temp-moments
    do i=0, 4
       tempMoms(i) = dot_product( N_TEMP(:,i), this%mDFs(9:13, x, y, this%mOld) )
    end do
  end subroutine calcLocalMomsMrtSolverBoussinesq2D

  subroutine calcLocalEqMomsMrtSolverBoussinesq2D( this, &
       dRho, uX, uY, temp, fluidEqMoms, tempEqMoms )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    real(RK), intent(in) :: dRho, uX, uY, temp
    real(RK), intent(out) :: fluidEqMoms(0:8), tempEqMoms(0:4)
    ! .............................
    ! fluid
    fluidEqMoms(0) = dRho
    fluidEqMoms(1) = uX
    fluidEqMoms(2) = uY
    fluidEqMoms(3) = -2._RK*dRho + 3._RK*(uX**2+uY**2)
    fluidEqMoms(4) = uX**2-uY**2
    fluidEqMoms(5) = uX*uY
    fluidEqMoms(6) = -uX
    fluidEqMoms(7) = -uY
    fluidEqMoms(8) = -fluidEqMoms(3) - dRho
    ! temp
    tempEqMoms(0) = temp
    tempEqMoms(1) = temp*uX
    tempEqMoms(2) = temp*uY
    tempEqMoms(3) = this%mAParam*temp
    tempEqMoms(4) = 0._RK
  end subroutine calcLocalEqMomsMrtSolverBoussinesq2D

  ! advance solver-state by one time-step (core LBM-algorithm)
  subroutine advanceTimeMrtSolverBoussinesq2D( this )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    ! local vars
    integer(IK) :: x, y, i, old, new ! dummy indices
    integer(IK), dimension(0:1) :: dest
    real(RK) :: fluidMoms(0:8), tempMoms(0:4), &
         fluidEqMoms(0:8), tempEqMoms(0:4)

    ! initializations
    dest = 0; fluidMoms = 0._RK; tempMoms = 0._RK
    fluidEqMoms = 0._RK; tempEqMoms = 0._RK
    old = this%mOld; new = this%mNew

    do y=1, this%mNy
       do x=1, this%mNx
          call this%calcLocalMomsMrtSolverBoussinesq2D(x, y, fluidMoms, tempMoms)

          ! add 1st-half of force term (Strang splitting)
          fluidMoms(2) = fluidMoms(2) + this%mAlphaG*0.5_RK*tempMoms(0)

          ! save moments related to output
          this%mRawMacros(x, y, :) = &
               [ fluidMoms(0), fluidMoms(1), fluidMoms(2), tempMoms(0) ]

          call this%calcLocalEqMomsMrtSolverBoussinesq2D( dRho=fluidMoms(0), &
               uX=fluidMoms(1), uY=fluidMoms(2), temp=tempMoms(0), &
               fluidEqMoms=fluidEqMoms, tempEqMoms=tempEqMoms )

          ! collision (in moment-space)
          fluidMoms = fluidMoms - this%mRelaxVecFluid * (fluidMoms - fluidEqMoms)
          tempMoms  = tempMoms  - this%mRelaxVecTemp * (tempMoms - tempEqMoms)

          ! add 2nd-half of force term (Strang splitting)
          fluidMoms(2) = fluidMoms(2) + this%mAlphaG*0.5_RK*tempMoms(0)

          ! map moments back onto DFs...
          ! ...fluid
          do i=0, 8
             this%mDFs(i, x, y, old) = dot_product( M_INV_FLUID(:, i), fluidMoms )
          end do
          ! ...temp
          do i=0, 4
             this%mDFs(i+9, x, y, old) = dot_product( N_INV_TEMP(:, i), tempMoms )
          end do

          ! stream to new array...
          ! ...fluid
          do i=0, 8
             dest(0) = mod(x+EV_FLUID(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_FLUID(2, i)
             ! STREAM (also storing runaway DFs in Y-buffer space)
             this%mDFs(i, dest(0), dest(1), new) = this%mDFs(i, x, y, old)
             if( dest(1) == 0 ) then
                if( EV_FLUID(2, i) /= 0 ) then
                   ! apply bounce-back @bottom
                   this%mDFs(OPPOSITE_FLUID(i), x, y, new) = &
                        this%mDFs(i, dest(0), dest(1), old)
                end if
             elseif( dest(1) == this%mNy+1 ) then
                if( EV_FLUID(2, i) /= 0 ) then
                   ! apply bounce-back @top
                   this%mDFs(OPPOSITE_FLUID(i), x, y, new) = &
                        this%mDFs(i, dest(0), dest(1), old)
                end if
             end if
          end do
          ! ...temp
          do i=0, 4
             dest(0) = mod(x+EV_TEMP(1, i)+this%mNx-1, this%mNx)+1
             dest(1) = y+EV_TEMP(2, i)
             ! STREAM (also storing runaway DFs in Y-buffer space)
             this%mDFs(i+9, dest(0), dest(1), new) = this%mDFs(i+9, x, y, old)
             if( dest(1) == 0 ) then
                ! apply anti-bounce-back @bottom
                this%mDFs(OPPOSITE_TEMP(i)+9, x, y, new) = &
                     -this%mDFs(i+9, dest(0), dest(1), old) + &
                     2._RK*sqrt(3._RK)*this%mDiffusivity*this%mTempHotWall
             elseif( dest(1) == this%mNy+1 ) then
                ! apply anti-bounce-back @top
                this%mDFs(OPPOSITE_TEMP(i)+9, x, y, new) = &
                     -this%mDFs(i+9, dest(0), dest(1), old) + &
                     2._RK*sqrt(3._RK)*this%mDiffusivity*this%mTempColdWall
             end if
          end do
       end do
    end do

    ! swap 'pointers' (for lattice-alternation)
    call swap( this%mOld, this%mNew )
  end subroutine advanceTimeMrtSolverBoussinesq2D

  subroutine cleanupMrtSolverBoussinesq2D( this )
    class(MrtSolverBoussinesq2D), intent(inout) :: this
    deallocate( this%mDFs, this%mRawMacros ) ! release memory
  end subroutine cleanupMrtSolverBoussinesq2D
end module MrtSolverBoussinesq2D_class

module RBenardSimulation_class
  use NumericKinds, only : IK, RK
  use MrtSolverBoussinesq2D_class
  use OutputAscii_class
  implicit none

  ! Fixed simulation-parameters
  real(RK), parameter :: &
       ! To allow the 1st instability to develop, the aspect-ratio needs to be a
       ! multiple of $\frac{2 \pi}{k_C}$, where $k_C = 3.117$ (see [Shan1997]).
       ASPECT_RATIO = 2*2.0158, &
       ! See [Wang2013] for justification of these parameters.
       SIGMA_K = 3._RK - sqrt(3._RK), &
       SIGMA_NU_E = 2._RK * (2._RK*sqrt(3._RK) - 3._RK), &
       TEMP_COLD_WALL = -0.5, TEMP_HOT_WALL = +0.5

  type :: RBenardSimulation
     private
     integer(IK) :: mNx, mNy, & ! lattice size
          mNumIters1CharTime, mNumItersMax, &
          mNumOutSlices ! user-setting

     type(MrtSolverBoussinesq2D) :: mSolver ! associated solver...
     type(OutputAscii) :: mOutSink ! ...and output-writer

   contains
     private
     procedure, public :: init => initRBenardSimulation
     procedure, public :: run => runRBenardSimulation
     procedure, public :: cleanup => cleanupRBenardSimulation
  end type RBenardSimulation

contains
  subroutine initRBenardSimulation( this, Ra, Pr, nY, simTime, maxMach, &
       numOutSlices, outFilePrefix )
    class(RBenardSimulation), intent(out) :: this
    real(RK), intent(in) :: Ra, Pr, simTime, maxMach
    integer(IK), intent(in) :: nY, numOutSlices
    character(len=*), intent(in) :: outFilePrefix
    ! .............................
    ! local vars
    integer(IK) :: i ! dummy index
    real(RK) :: sNuEpsE, sQ, dxD, dtD, viscosity, diffusivity, alphaG, aParam, &
         relaxVecFluid(0:8), relaxVecTemp(0:4)

    ! copy argument-values inside internal state
    this%mNy = nY
    this%mNumOutSlices = numOutSlices

    ! compute derived parameters
    this%mNx = nint( ASPECT_RATIO*nY, IK )
    dxD = 1._RK/nY
    alphaG = (maxMach**2)/(3._RK*nY)
    dtD = maxMach/(nY*sqrt(3._RK*Ra*Pr))
    viscosity = nY*maxMach*sqrt(Pr/(3._RK*Ra))
    diffusivity = (nY*maxMach)/sqrt(3._RK*Ra*Pr)

    this%mNumIters1CharTime = nint( 1._RK/dtD, IK )
    this%mNumItersMax = nint( this%mNumIters1CharTime*simTime, IK )

    write(*,*) "nItersMax =", this%mNumItersMax
    sNuEpsE = 2._RK / (6._RK*viscosity + 1._RK)
    sQ = 8._RK * ((2._RK-sNuEpsE)/(8._RK-sNuEpsE))
    aParam = ((60._RK*diffusivity) / sqrt(3._RK)) - 4._RK
    ! Enforce safety-check: for stability, we need $a < 1$
    if( aParam >= 1._RK ) then
       write(*,'(4(a,/),a)') "ERROR: invalid combination of model-parameters", &
            "Possible cause: Prandtl-number too low", &
            "Possible fix: increase Pr, or maxMach", &
            "(but later approach increases model-errors)", &
            "Aborting..."
       stop
    end if

    ! build representations of relaxation-matrices
    relaxVecFluid = [ 0._RK, (1._RK, i=1,2), (sNuEpsE, i=1,3), (sQ, i=1,2), sNuEpsE ]
    relaxVecTemp = [ 0._RK, (SIGMA_K, i=1,2), (SIGMA_NU_E, i=1,2) ]

    call this%mOutSink%init( this%mNx, this%mNy, numOutSlices, &
         dxD, dtD, this%mNumItersMax, outFilePrefix, Ra, Pr, maxMach )

    call this%mSolver%init( this%mNx, this%mNy, TEMP_COLD_WALL, TEMP_HOT_WALL, &
         viscosity, diffusivity, alphaG, aParam, relaxVecFluid, relaxVecTemp )

    call this%mOutSink%writeOutput( this%mSolver%getRawMacros(), 0 )
  end subroutine initRBenardSimulation

  subroutine runRBenardSimulation( this )
    class(RBenardSimulation), intent(inout) :: this
    integer(IK) :: currIterNum ! dummy index
    real(RK) :: tic, toc ! for performance-reporting

    call cpu_time(time=tic) ! serial

    ! MAIN loop (time-iteration)
    do currIterNum=1, this%mNumItersMax
       ! simple progress-monitor
       if( mod(currIterNum-1, (this%mnumitersmax-1)/10) == 0 ) then
          write(*, '(i5,a)') nint((currIterNum*100._RK)/this%mnumitersmax), "%"
       end if

       call this%mSolver%advanceTime()

       call this%mOutSink%writeOutput( this%mSolver%getRawMacros(), currIterNum )
    end do

    call cpu_time(time=toc) ! serial

    write(*,'(/,a,f0.2,a)') "Performance Information: achieved ", &
         this%mNumItersMax*real(this%mNx*this%mNy, RK) / (1.0e6*(toc-tic)), &
         " MLUPS (mega-lattice-updates-per-second)"
  end subroutine runRBenardSimulation

  subroutine cleanupRBenardSimulation( this )
    class(RBenardSimulation), intent(inout) :: this

    call this%mSolver%cleanup()
    call this%mOutSink%cleanup()
  end subroutine cleanupRBenardSimulation
end module RBenardSimulation_class

program lbm2d_mrt_rb
  use RBenardSimulation_class
  implicit none

  type(RBenardSimulation) :: testSim

  call testSim%init( Ra=1900._RK, Pr=7.1_RK, &
       nY=62, simTime=15._RK, maxMach=0.3_RK, &
       numOutSlices=80, outFilePrefix="rb_Ra_1900" )

  call testSim%run()

  call testSim%cleanup()
end program lbm2d_mrt_rb
