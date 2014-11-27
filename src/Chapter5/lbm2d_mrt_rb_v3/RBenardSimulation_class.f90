module RBenardSimulation_class
  use NumericKinds, only : IK, RK
  use MrtSolverBoussinesq2D_class
  use OutputNetcdf_class
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
     type(OutputNetcdf) :: mOutSink ! ...and output-writer

   contains
     private
     procedure, public :: init => initRBenardSimulation
     procedure, public :: run => runRBenardSimulation
     procedure, public :: cleanup => cleanupRBenardSimulation
  end type RBenardSimulation

  ! ......................
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
    use omp_lib
    class(RBenardSimulation), intent(inout) :: this
    integer(IK) :: currIterNum, realNumThreads
    real(RK) :: tic, toc, numMLUPS ! for performance-reporting

    tic = omp_get_wtime() ! parallel

    ! MAIN loop (time-iteration)
    do currIterNum=1, this%mNumItersMax
       ! simple progress-monitor
       if( mod(currIterNum-1, (this%mnumitersmax-1)/10) == 0 ) then
          write(*, '(i5,a)') nint((currIterNum*100._RK)/this%mnumitersmax), "%"
       end if

       realNumThreads = this%mSolver%advanceTime()

       call this%mOutSink%writeOutput( this%mSolver%getRawMacros(), currIterNum )
    end do

    toc = omp_get_wtime() ! parallel

    numMLUPS = this%mNumItersMax*real(this%mNx*this%mNy, RK) / (1.0e6*(toc-tic))
    write(*,'(/,a,f0.2,a)') "Performance Information: achieved ", &
         numMLUPS, " MLUPS (mega-lattice-updates-per-second)"
    write(*,'(a,i0,a,f0.4,a)') "[ <nThreads> ", realNumThreads, &
         " </nThreads> <perf> ", numMLUPS, "</perf> ]"
  end subroutine runRBenardSimulation

  subroutine cleanupRBenardSimulation( this )
    class(RBenardSimulation), intent(inout) :: this

    call this%mSolver%cleanup()
    call this%mOutSink%cleanup()
  end subroutine cleanupRBenardSimulation
end module RBenardSimulation_class
