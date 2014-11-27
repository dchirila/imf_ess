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

