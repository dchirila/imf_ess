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

