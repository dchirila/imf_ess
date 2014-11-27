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

