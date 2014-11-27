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

