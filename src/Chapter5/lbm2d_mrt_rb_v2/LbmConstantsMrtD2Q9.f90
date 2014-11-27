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

