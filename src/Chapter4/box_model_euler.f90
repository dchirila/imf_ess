! File: box_model_euler.f90
! Purpose: Example program for simulating the evolution of the climate
!          box-model.
! NOTES: - The actual physics is encoded into the function 'dModelState'
!          (contains the RHS of the evolution-equations for the
!          model-variables).
!        - To change the precision of the floating-point calculations, the
!          'RK'-parameter can be set to 'R_SP', 'R_DP', or 'R_QP' (for single-,
!          double-, and quad-precision, respectively). WARNING: when changing
!          this, 'RK_FMT' should also be changed accordingly!

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

  ! Edit-descriptors for real-values
  character(len=*), parameter :: R_SP_FMT = "f0.6", &
       R_DP_FMT = "f0.15", R_QP_FMT = "f0.33"
  ! Alias for output-precision to use in the program (keep this in sync with RK)
  character(len=*), parameter :: RK_FMT = R_DP_FMT
end module NumericKinds

module PhysicsConstants
  use NumericKinds
  implicit none
  public

  real(RK), parameter :: &
       RHO_SEA_WATER = 1025.,    & ! [kg/m^3]
       ! .............................
       RHO_FRESH_WATER = 1000.,  & ! [kg/m^3]
       PI = 3.141593, &
       ONE_DEG_IN_RADS = PI / 180., &
       SECONDS_IN_YEAR = 365.25 * 24. * 3600., & ! [s]
       TFREEZE_SEA_WATER = -1.8, & ! [Celsius]
       S_REF = 34.9, & ! reference salinity [ppt]
       CP_WATER   = 4200.,  & ! specific heat-capacity of water
       CP_DRY_AIR = 1004.,  & ! specific heat cap. of dry air
       RH = 0.8, & ! mean relative humidity
       ALPHA = 1.5E-4, & ! thermal and
       BETA  = 8.E-4,  & ! haline expansion coefficent at 15 degree Celsius
       SREF = 34.9, & ! reference salinity
       A = 213.35, & ! For parameterization of outgoing longwave radiation
       B = 2.22,   & ! (see Budyko 1969, Chen et al. 1995)
       LV = 2.5E6, & ! latent heat of vaporisation
       LR = 1./(LV * RHO_FRESH_WATER), &
       R_E = 6.371E6, & ! Earth radius
       WIDTH_ATLANTIC = 80. ! lateral span of the Atlantic [degrees of longitude]
end module PhysicsConstants

module ModelConstants
  use NumericKinds
  use PhysicsConstants
  implicit none
  public

  real(RK), parameter :: &
       NO_YEARS = 10000., & ! total simulation time [yr]
       DT_IN_YEARS = 1./100., & ! time-step [yr]
       DTS = DT_IN_YEARS * SECONDS_IN_YEAR, & ! time-step [s]
       ! tuning-parameters for surface heat-fluxes
       Q1_S = 10., Q2_S = 50., &
       ! .............................
       Q1_M = 70., Q2_M = 50., &
       Q1_N = 20., Q2_N = 50., &
       KS = 1.e13 * 2.5, & ! tuning parameter for sensible heat fluxes
       KL = 5.10e17 * 1.5, & ! tuning parameter for latent heat fluxes
       ! fraction of the total hemispheric surface
       FRF_S = 0.16267, FRF_M = 80. / 360., FRF_N = 0.21069, &
       C = 1.5264e10 * 1., & ! tuning parameter for overturning
       ! thermal inertia (see Chen et al. 1995)
       BETA_S = 5300., BETA_M = 5300., BETA_N = 5300., &
       DZ1 =  600., & ! depth of tropical box
       DZ2 = 4000., & ! depth of ocean
       ! pre-computed factors
       RCZ1 = RHO_SEA_WATER * CP_WATER * DZ1, &
       RCZ2 = RHO_SEA_WATER * CP_WATER * DZ2, &
       ! constants for incoming solar radiation (shortwave)
       S_SOL_S = 320., S_SOL_M = 390., S_SOL_N = 270., &
       ALBEDO_S = 0.4, ALBEDO_M = 0.25, ALBEDO_N = 0.42, &
       ! latitudinal-bounds of the atmosphere boxes
       LAT1_AT_S = -90., LAT2_AT_S = -30., &
       LAT1_AT_M = -30., LAT2_AT_M =  45., &
       LAT1_AT_N =  45., LAT2_AT_N =  90., &
       ! latitudinal-bounds of the ocean boxes
       LAT1_OC_S = -60., LAT2_OC_S = -30., &
       LAT1_OC_M = -30., LAT2_OC_M =  45., &
       LAT1_OC_N =  45., LAT2_OC_N =  80., &
       LAT1_OC_D = -30., LAT2_OC_D =  45., &
       ! areas of the ocean boxes
       AREA_S = (2.*PI*R_E**2) * abs( &
       sin(LAT1_OC_S*ONE_DEG_IN_RADS) - sin(LAT2_OC_S*ONE_DEG_IN_RADS) &
       ) * (WIDTH_ATLANTIC/360.), &
       AREA_M = (2.*PI*R_E**2) * abs( &
       sin(LAT1_OC_M*ONE_DEG_IN_RADS) - sin(LAT2_OC_M*ONE_DEG_IN_RADS) &
       ) * (WIDTH_ATLANTIC/360.), &
       AREA_N = (2.*PI*R_E**2) * abs( &
       sin(LAT1_OC_N*ONE_DEG_IN_RADS) - sin(LAT2_OC_N*ONE_DEG_IN_RADS) &
       ) * (WIDTH_ATLANTIC/360.), &
       AREA_D = (2.*PI*R_E**2) * abs( &
       sin(LAT1_OC_D*ONE_DEG_IN_RADS) - sin(LAT2_OC_D*ONE_DEG_IN_RADS) &
       ) * (WIDTH_ATLANTIC/360.), &
       ! volumes of the ocean boxes
       V_S = AREA_S*DZ2, V_M = AREA_M*DZ1, V_N = AREA_N*DZ2, V_D = AREA_D*(DZ2-DZ1)

  integer, parameter :: &
       NO_T_STEP = int(NO_YEARS / DT_IN_YEARS), & ! number of model-iterations
       OUTPUT_FREQUENCY = 100
end module ModelConstants

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

module ModelState_class
  use iso_fortran_env, only : error_unit
  use PhysicsConstants
  use ModelConstants
  use GeomUtils
  implicit none

  type :: ModelState
     real(RK) :: TocS, TocM, TocN, TocD, SocS, SocM, SocN, SocD, TatS, TatM, TatN
   contains
     procedure, public :: getCurrModelState
     procedure, public :: preventOceanFreezing
     procedure, public :: computePhi
  end type ModelState

  interface ModelState
     module procedure newModelState
  end interface ModelState

  interface operator(*)
     module procedure scalarTimesModelState
     module procedure modelStateTimesScalar
  end interface operator(*)

  interface operator(+)
     module procedure addModelStates
  end interface operator(+)

  interface operator(-)
     module procedure subtractModelStates
  end interface operator(-)

contains
  ! Calculate the slope of saturation vapor pressure w.r.t temperature
  ! (see Rogers and Yau, Cloud Physics, 1976, p.16).
  ! NOTE: Unlike the other procedures in the module, this one is not type-bound.
  real(RK) function dQSdT( Tc )
    real(RK), intent(in) :: Tc
    real(RK) :: p, ex, sat
    p = 1000.
    ex = 17.67 * Tc/( Tc + 243.5 )
    sat = 6.112 * exp( ex )
    dQSdT = 243.5 * 17.67 * sat / (Tc + 243.5)**2 * 0.622 / p
  end function dQSdT

  ! User-defined CTOR (INITer).
  type(ModelState) function newModelState( TocS, TocM, TocN, TocD, &
       SocS, SocM, SocN, SocD, TatS, TatM, TatN ) result(new)
    real(RK), intent(in) :: TocS, TocM, TocN, TocD, &
         SocS, SocM, SocN, SocD, TatS, TatM, TatN
    ! ................................
    new%TocS = TocS; new%TocM = TocM; new%TocN = TocN; new%TocD = TocD
    new%SocS = SocS; new%SocM = SocM; new%SocN = SocN; new%SocD = SocD
    new%TatS = TatS; new%TatM = TatM; new%TatN = TatN
  end function newModelState

  type(ModelState) function scalarTimesModelState( scalar, state ) result(res)
    real(RK), intent(in) :: scalar
    class(ModelState), intent(in) :: state
    ! ................................
    res%TocS = scalar*state%TocS; res%TocM = scalar*state%TocM
    res%TocN = scalar*state%TocN; res%TocD = scalar*state%TocD
    res%SocS = scalar*state%SocS; res%SocM = scalar*state%SocM
    res%SocN = scalar*state%SocN; res%SocD = scalar*state%SocD
    res%TatS = scalar*state%TatS; res%TatM = scalar*state%TatM
    res%TatN = scalar*state%TatN
  end function scalarTimesModelState

  type(ModelState) function modelStateTimesScalar( state, scalar ) result(res)
    class(ModelState), intent(in) :: state
    real(RK), intent(in) :: scalar
    ! re-use 'scalarTimesModelState'
    res = scalar*state
  end function modelStateTimesScalar

  type(ModelState) function addModelStates( state1, state2 ) result(res)
    class(ModelState), intent(in) :: state1, state2
    ! ................................
    res%TocS = state1%TocS + state2%TocS; res%TocM = state1%TocM + state2%TocM
    res%TocN = state1%TocN + state2%TocN; res%TocD = state1%TocD + state2%TocD
    res%SocS = state1%SocS + state2%SocS; res%SocM = state1%SocM + state2%SocM
    res%SocN = state1%SocN + state2%SocN; res%SocD = state1%SocD + state2%SocD
    res%TatS = state1%TatS + state2%TatS; res%TatM = state1%TatM + state2%TatM
    res%TatN = state1%TatN + state2%TatN
  end function addModelStates

  type(ModelState) function subtractModelStates( state1, state2 ) result(res)
    class(ModelState), intent(in) :: state1, state2
    ! ................................
    res%TocS = state1%TocS - state2%TocS; res%TocM = state1%TocM - state2%TocM
    res%TocN = state1%TocN - state2%TocN; res%TocD = state1%TocD - state2%TocD
    res%SocS = state1%SocS - state2%SocS; res%SocM = state1%SocM - state2%SocM
    res%SocN = state1%SocN - state2%SocN; res%SocD = state1%SocD - state2%SocD
    res%TatS = state1%TatS - state2%TatS; res%TatM = state1%TatM - state2%TatM
    res%TatN = state1%TatN - state2%TatN
  end function subtractModelStates

  ! "Brute-force" resetting of ocean-temperatures, if they decrease below
  ! freezing-point.
  subroutine preventOceanFreezing( this )
    class(ModelState), intent(inout) :: this

    if( this%TocS < TFREEZE_SEA_WATER) then
       this%TocS = TFREEZE_SEA_WATER
       write(error_unit, '(a)') "Warning: TocS was reset to prevent freezing!"
    end if
    ! ................................
    if( this%TocM < TFREEZE_SEA_WATER) then
       this%TocM = TFREEZE_SEA_WATER
       write(error_unit, '(a)') "Warning: TocM was reset to prevent freezing!"
    end if

    if( this%TocN < TFREEZE_SEA_WATER) then
       this%TocN = TFREEZE_SEA_WATER
       write(error_unit, '(a)') "Warning: TocN was reset to prevent freezing!"
    end if

    if( this%TocD < TFREEZE_SEA_WATER) then
       this%TocD = TFREEZE_SEA_WATER
       write(error_unit, '(a)') "Warning: TocD was reset to prevent freezing!"
    end if
  end subroutine preventOceanFreezing

  real(RK) function computePhi( this ) result(phi)
    class(ModelState), intent(in) :: this

    phi = C*(-ALPHA*(this%TocN-this%TocS) + BETA*(this%SocN-this%SocS))
    if( phi < 0. ) then
       phi=0. ! prevent reversal of circulation
       write(error_unit, '(a)') "Warning: reversal of circulation detected!"
    end if
  end function computePhi

  function getCurrModelState( this ) result(res)
    class(ModelState), intent(in) :: this
    real(RK) :: res(13)
    ! local vars
    real(RK) :: tempGlobal

    tempGlobal = (0.5*this%TatS+1.207*this%TatM+0.293*this%TatN)/2.

    res = [ tempGlobal, this%TocS, this%TocM, this%TocN, this%TocD, &
         this%SocS, this%SocM, this%SocN, this%SocD, &
         this%TatS, this%TatM, this%TatN, &
         this%computePhi()*1.E-6 ] ! units are transformed to [Sv]
  end function getCurrModelState

  ! Physics is encoded here (i.e. RHS of evolution equations)
  type(ModelState) function dModelState( old )
    type(ModelState), intent(in) :: old
    real(RK) :: F30S, F45N, phi, Tat30S, Tat45N, FsS, FsN, FlS, FlN, hS, hM, hN, &
         fwFaS, fwFaN, rS, rM, rN, midLatS, midLatM, midLatN

    midLatS = rad2Deg( asin( (sinD(LAT1_AT_S)+sinD(LAT2_AT_S))/2._RK ) )
    midLatM = rad2Deg( asin( (sinD(LAT1_AT_M)+sinD(LAT2_AT_M))/2._RK ) )
    midLatN = rad2Deg( asin( (sinD(LAT1_AT_N)+sinD(LAT2_AT_N))/2._RK ) )

    Tat30S = linInterp(x0=midLatM, y0=old%TatM, x1=midLatS, y1=old%TatS, x=-30._RK)
    Tat45N = linInterp(x0=midLatM, y0=old%TatM, x1=midLatN, y1=old%TatN, x= 45._RK)

    FsS = KS * (old%TatM-old%TatS)/(R_E*(deg2Rad(midLatM)-deg2Rad(midLatS)))
    FsN = KS * (old%TatM-old%TatN)/(R_E*(deg2Rad(midLatN)-deg2Rad(midLatM)))

    FlS = KL*RH*dQSdT(Tat30S) / (old%TatM - old%TatS) * FsS/KS
    FlN = KL*RH*dQSdT(Tat45N) / (old%TatM - old%TatN) * FsN/KS

    F30S = FsS + FlS
    F45N = FsN + FlN

    fwFaS = ( LR*2*PI*R_E*cosD(30._RK)*FlS ) * (80./360.)
    fwFaN = ( LR*2*PI*R_E*cosD(45._RK)*FlN ) * (80./360.) * 2.5

    hS = Q1_S - Q2_S*(old%TocS-old%TatS)
    hM = Q1_M - Q2_M*(old%TocM-old%TatM)
    hN = Q1_N - Q2_N*(old%TocN-old%TatN)
    ! radiation-balance terms
    rS = S_SOL_S*(1.-ALBEDO_S) - (A+B*old%TatS)
    rM = S_SOL_M*(1.-ALBEDO_M) - (A+B*old%TatM)
    rN = S_SOL_N*(1.-ALBEDO_N) - (A+B*old%TatN)

    phi = old%computePhi()

    ! Final phase: preparing the function-result
    !! Ocean Temperatures
    dModelState%TocS = -(old%TocS-old%TocD)*phi/V_S + hS/RCZ2
    dModelState%TocM = -(old%TocM-old%TocS)*phi/V_M + hM/RCZ1
    dModelState%TocN = -(old%TocN-old%TocM)*phi/V_N + hN/RCZ2
    dModelState%TocD = -(old%TocD-old%TocN)*phi/V_D
    !! Ocean Salinities
    dModelState%SocS = -(old%SocS-old%SocD)*phi/V_S - S_REF*fwFaS/V_S
    dModelState%SocM = -(old%SocM-old%SocS)*phi/V_M + S_REF*(fwFaS+fwFaN)/V_M
    dModelState%SocN = -(old%SocN-old%SocM)*phi/V_N - S_REF*fwFaN/V_N
    dModelState%SocD = -(old%SocD-old%SocN)*phi/V_D
    !! Atmosphere Temperatures
    dModelState%TatS = ( &
        (cosD(30._RK)*F30S) / (R_E*(sinD(90._RK)-sinD(30._RK))) &
        + rS - FRF_S*hS &
        )/( CP_DRY_AIR*BETA_S )

    dModelState%TatM = ( &
        -(cosD(30._RK)*F30S+cosD(45._RK)*F45N) / (R_E*(sinD(30._RK)+sinD(45._RK))) &
        + rM - FRF_M*hM &
        )/( CP_DRY_AIR*BETA_M )

    dModelState%TatN = ( &
        (cosD(45._RK)*F45N) / (R_E*(sinD(90._RK)-sinD(45._RK))) &
        + rN - FRF_N*hN &
        )/( CP_DRY_AIR*BETA_N )
  end function dModelState
end module ModelState_class

program box_model_euler
  use PhysicsConstants
  use ModelConstants
  use ModelState_class
  implicit none

  integer :: i, outFileID
  type(ModelState) :: stateSim1E, statePerturbation

  ! Perturbation to superimpose over equilibrium state.
  statePerturbation = ModelState( TocS = 0., TocM = 0., &
       TocN = 0., TocD = 0., &
       SocS = 0., SocM = 0., &
       SocN = -0.7, SocD = 0., &
       TatS = 0., TatM = 0., TatN = 0. )

  stateSim1E = ModelState( TocS = 4.77740431, TocM = 24.42876625, &
       TocN = 2.66810894, TocD = 2.67598915, &
       SocS = 34.40753555, SocM = 35.62585068, &
       SocN = 34.92513657, SocD = 34.91130066, &
       TatS = 4.67439556, TatM = 23.30437851, TatN = 0.94061828) + statePerturbation

  ! prepare for output
  open(newunit=outFileID, file="box_model_euler.out", &
       form="formatted", status="replace")

  ! write initial conditions
  write(outFileID, '(14('//RK_FMT//', 1x))') 0._RK, stateSim1E%getCurrModelState()

  do i=1, NO_T_STEP
     ! Euler-forward step
     stateSim1E = stateSim1E + DTS*dModelState(stateSim1E)

     call stateSim1E%preventOceanFreezing()

     ! Conditional OUTPUT-writing
     if( mod(i-1, OUTPUT_FREQUENCY) == 0 ) then
        write(outFileID, '(14('// RK_FMT // ', 1x))') i*DT_IN_YEARS, &
             stateSim1E%getCurrModelState()
     end if
  end do
  close(outFileID) ! Clean-up for output
end program box_model_euler
