! File: solve_heat_diffusion_v3.f90
! Purpose: OOP implementation in Fortran of the ADE method of Barakat & Clark
!          (1966), for solving the time-dependent heat-diffusion equation in 2D,
!          with a linearly-varying temperature profile on the domain-boundaries.
!          This version builds upon 'solve_heat_diffusion_v2.f90' (which brought
!          support for NAMELIST-input) by adding some rudimentary
!          parallelization, using OpenMP.
! NOTES: - (for gfortran users): gfortran-4.8 still had no support for
!          'final'-methods, and an error will be raised by that version (or
!          earlier) of that compiler; HOWEVER, the final-procedure can be safely
!          removed in our case, since it is included for demonstration-purposes
!          only (the arrays which it de-allocates are freed anyway by the runtime
!          system, when the 'Solver'-instance goes out of scope). Comment
!          corresponding line below, to disable use of this feature.
!        - The file "heat_diffusion_config.nml", containing the parameters for the
!          solver, needs to be located in working directory.
!        - (NEW in this version) To take advantage of parallelization, this
!          program needs to be compiled with OpenMP support (typically, by
!          adding the '-fopenmp' flag to the compilation-command -- please check
!          the documentation of your compiler to make sure this option is
!          enabled).
!        - Lines containing only comments with dots can be ignored (they just
!          help with including code in TeX-files).

module NumericKinds
  implicit none

  ! KINDs for different types of REALs
  integer, parameter :: &
       R_SP = selected_real_kind(  6,   37 ), &
       R_DP = selected_real_kind( 15,  307 ), &
       R_QP = selected_real_kind( 33, 4931 )
  ! Alias for precision that we use in the program (change this to any of the
  ! values 'R_SP', 'R_DP', or 'R_QP', to switch to another precision globally).
  integer, parameter :: RK = R_DP ! if changing this, also change RK_FMT

  ! KINDs for different types of INTEGERs
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
end module NumericKinds

module Config_class
  use NumericKinds
  implicit none
  private

  type, public :: Config
     real(RK) :: mDiffusivity = 1.15E-6_RK, & ! sandstone
          ! NOTE: "physical" units here (Celsius)
          mTempA = 100._RK, &
          mTempB =  75._RK, &
          mTempC =  50._RK, &
          mTempD =  25._RK, &
          mSideLength  = 30._RK
     integer(IK) :: mNx = 200 ! # of points for square side-length
  end type Config

  ! Generic IFACE for user-defined CTOR
  interface Config
     module procedure createConfig
  end interface Config

contains
  type(Config) function createConfig( cfgFilePath )
    character(len=*), intent(in) :: cfgFilePath
    integer :: cfgFileID

    ! Constant to act as safeguard-marker, allowing us to check if values were
    ! actually obtained from the NAMELIST.
    ! NOTE: '-9999' is an integer which can be *exactly* represented in the
    !       mantissa of single-/double-precision IEEE reals. This means that the
    !       expression:
    !         int(aReal, IK) == MISS
    !       will be TRUE as long as
    !         (a) 'aReal' was initialized with MISS and
    !         (b) other instructions (e.g. NAMELIST-I/O here) did not modify the
    !             value of 'aReal'.
    integer(IK), parameter :: MISS = -9999

    ! We need local-variables, to mirror the ones in the NAMELIST
    real :: sideLength=MISS, diffusivity=MISS, &
         tempA=MISS, tempB=MISS, tempC=MISS, tempD=MISS
    integer :: nX = MISS
    ! NAMELIST definition
    namelist/heat_diffusion_ade_params/ sideLength, diffusivity, nX, &
         tempA, tempB, tempC, tempD

    open( newunit=cfgFileID, file=trim(cfgFilePath), status='old', action='read' )
    read(cfgFileID, nml=heat_diffusion_ade_params)
    close(cfgFileID)

    ! For diagnostics: echo information back to terminal.
    write(*,'(">> START: Namelist we read <<")')
    write(*, nml=heat_diffusion_ade_params)
    write(*,'(">> END: Namelist we read   <<")')

    ! Assimilate data read from NAMELIST into new object's internal state.
    ! NOTE: Here, we make use of the safeguard-constant, so that default values
    !       (from the type-definition) are overwritten only if the user provided
    !       replacement values (in the NAMELIST).
    if( int(sideLength, IK) /= MISS ) createConfig%mSideLength = sideLength
    if( int(diffusivity, IK) /= MISS ) createConfig%mDiffusivity = diffusivity
    if( nX /= MISS ) createConfig%mNx = nX
    if( int(tempA, IK) /= MISS ) createConfig%mTempA = tempA
    if( int(tempB, IK) /= MISS ) createConfig%mTempB = tempB
    if( int(tempC, IK) /= MISS ) createConfig%mTempC = tempC
    if( int(tempD, IK) /= MISS ) createConfig%mTempD = tempD
  end function createConfig
end module Config_class

module Solver_class
  use NumericKinds
  use Config_class
  implicit none
  private

  type, public :: Solver
     private ! Hide internal-data from users.
     type(Config) :: mConfig
     real(RK) :: mNt, & ! # of iterations to simulate a characteristic time
          mDx, mDt, mA, mB ! Configuration-dependent factors.
     real(RK), allocatable, dimension(:,:) :: mU, mV ! main work-arrays
     integer(IK) :: mNumItersMax, mCurrIter = 0
   contains
     private ! By default, hide methods (and expose as needed).
     procedure, public :: init
     procedure, public :: run
     procedure, public :: writeAscii
     procedure, public :: getTemp
     ! Internal methods (users don't need to know about these).
     procedure :: advanceU
     procedure :: advanceV
     !final :: cleanup ! NOTE: may need to comment-out for gfortran!
  end type Solver

contains
  subroutine init( this, cfgFilePath, simTime ) ! initialization subroutine
    class(Solver), intent(inout) :: this
    character(len=*), intent(in) :: cfgFilePath
    real(RK), intent(in) :: simTime
    ! .....................................................
    integer(IK) :: nX, i, j
    real(RK) :: lambda

    this%mConfig = Config( cfgFilePath ) ! call component CTOR
    nX = this%mConfig%mNx ! for making code below more compact
    ! conservative choice for N_t, to resolve transients -- see Barakat & Clark (1966)
    this%mNt = nX**2
    ! evaluate derived parameter 'mLambda' in Solver, based on configuration
    this%mNumItersMax = int( simTime*this%mNt )
    this%mDx = 1. / nX
    this%mDt = 1. / this%mNt
    lambda = (2.*this%mDt) / (this%mDx**2)
    this%mA = (1.-lambda)/(1.+lambda)
    this%mB = lambda/(2.*(1.+lambda))
    ! allocate memory for internal arrays
    allocate( this%mU(0:nX, 0:nX), this%mV(0:nX, 0:nX) )
    ! initialize mU-field:
    ! - set initial temperature everywhere...
    this%mU = 1.
    ! - BUT re-write @ boundaries, for correct BCs
    ! -- North
    this%mU(:, nX) = [ (1./3.*(i/real(nX, RK))+2./3., i=0,nX) ]
    ! -- West
    this%mU(0, :) = [ (1./3.*(j/real(nX, RK))+1./3., j=0,nX) ]
    ! -- South
    this%mU(:, 0) = [ (-1./3.*(i/real(nX, RK))+1./3., i=0,nX) ]
    ! -- East
    this%mU(nX, :) = [ (j/real(nX, RK), j=0,nX) ]
    ! initialize mV-field (from mU-field)
    this%mV = this%mU
  end subroutine init

  real(RK) function getTemp( this, i, j ) ! GETter for temperature
    class(Solver), intent(in) :: this
    integer(IK), intent(in) :: i, j
    getTemp = 0.5*( this%mU(i,j) + this%mV(i,j) )
  end function getTemp

  subroutine run( this ) ! method for time-marching
    class(Solver), intent(inout) :: this
    integer(IK) :: k ! dummy index (time-marching)

    do k=1, this%mNumItersMax ! MAIN loop
       ! simple progress-monitor
       if( mod(k-1, (this%mNumItersMax-1)/10) == 0 ) then
          write(*, '(i5,a)') nint((k*100.0)/this%mNumItersMax), "%"
       end if

       ! NEW: OpenMP pragmas below
       !$omp parallel num_threads(2)
        !$omp sections
        !$omp section
            call this%advanceU()   ! task for 1st thread

        !$omp section
            call this%advanceV()   ! task for 2nd thread
        !$omp end sections
       !$omp end parallel
       this%mCurrIter = this%mCurrIter + 1 ! tracking time step
    end do
  end subroutine run

  subroutine advanceU( this )
    class(Solver), intent(inout) :: this
    integer(IK) :: i, j ! local variables
    ! actual update for 'mU'-field (NE-ward)
    do j=1, this%mConfig%mNx-1   ! do NOT update
       do i=1, this%mConfig%mNx-1 ! boundaries
          this%mU(i,j) = this%mA*this%mU(i,j) + this%mB*( &
               this%mU(i-1,j) + this%mU(i+1,j) + this%mU(i,j-1) + this%mU(i,j+1) )
       end do
    end do
  end subroutine advanceU

  subroutine advanceV( this ) ! similar to 'advanceU'
    class(Solver), intent(inout) :: this
    ! .....................................................
    integer(IK) :: i, j ! local variables
    ! actual update for 'mV'-field (SW-ward)
    do j=this%mConfig%mNx-1, 1, -1   ! do NOT update
       do i=this%mConfig%mNx-1, 1, -1 ! boundaries
          this%mV(i,j) = this%mA*this%mV(i,j) + this%mB*( &
               this%mV(i-1,j) + this%mV(i+1,j) + this%mV(i,j-1) + this%mV(i,j+1) )
       end do
    end do
  end subroutine advanceV

  ! method for producing a ASCII output file
  subroutine writeAscii( this, outFilePath )
    class(Solver), intent(in) :: this
    character(len=*), intent(in) :: outFilePath
    ! .....................................................
    integer(IK) :: x, y, outFileID ! local variables

    open( newunit=outFileID, file=trim(outFilePath), status='replace', action='write' )
    write(outFileID, '(a)') &
         "# output file for program solve_heat_diffusion_v3.f90"
    write(outFileID, '(a,2x,a)') '"s"', "# time unit"
    write(outFileID, '(f0.8,2x,a)') &
         (this%mCurrIter*this%mConfig%mSideLength**2)/ &
         (this%mConfig%mDiffusivity*this%mNt), &
         "# current time"
    write(outFileID, '(a,2x,a)') '"m"', "# X unit"
    write(outFileID, '(i0,2x,a)') this%mConfig%mNx, "# Nx"
    write(outFileID, '(a,2x,a)') '"m"', "# Y unit"
    write(outFileID, '(i0,2x,a)') this%mConfig%mNx, "# Ny"
    write(outFileID, '(a,2x,a)') '"degree~C"', "# temperature unit"
    ! X-axis
    do x=0, this%mConfig%mNx
       write(outFileID, '(f0.8,2x)', advance='no') this%mDx*this%mConfig%mSideLength*x
    end do
    write(outFileID, '(a)') "# XVals"
    ! Y-axis
    do y=0, this%mConfig%mNx
       write(outFileID, '(f0.8,2x)', advance='no') this%mDx*this%mConfig%mSideLength*y
    end do
    write(outFileID, '(a)') "# YVals"
    ! simulation results
    write(outFileID, '(a)') "# from next line to end: simulated temperature"
    do y=0, this%mConfig%mNx
       do x=0, this%mConfig%mNx
          write(outFileID, '(f0.8,2x)', advance='no') &
               this%mConfig%mTempD+this%getTemp(x,y)*(this%mConfig%mTempA-this%mConfig%mTempD)
       end do
       write(outFileID, *) ! newline to separate rows for R visualization script
    end do
    close(outFileID)
  end subroutine writeAscii

  ! destructor method
  subroutine cleanup( this )
    ! 'class' -> 'type' (dummy-arg cannot be polymorphic for final procedures)
    type(Solver), intent(inout) :: this
    ! in this version, we only deallocate memory
    deallocate( this%mU, this%mV )
  end subroutine cleanup
end module Solver_class

program solve_heat_diffusion_v3
  use NumericKinds
  use Solver_class
  implicit none

  type(Solver) :: square
  real(RK) :: simTime = 0.1 ! no. of characteristic time-intervals to simulate

  character(len=200) :: configFile = "heat_diffusion_config.nml", &
       outputFile = "simulation_final_temp_field.dat"

  call square%init( configFile, simTime ) ! call Initializer
  call square%run()

  call square%writeAscii( outputFile )
end program solve_heat_diffusion_v3
