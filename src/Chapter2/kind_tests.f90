program kind_tests
  implicit none
  integer, parameter :: LARGE_INT = selected_int_kind(18)
  integer(kind=LARGE_INT) :: t = -123456_LARGE_INT

  integer, parameter :: R_DP = selected_real_kind(15, 307)
  real(kind=R_DP) :: x = 1.7_R_DP

  integer, parameter :: R_QP = selected_real_kind(33, 4931)
  real(kind=R_QP) :: y = 1.9_R_QP
end program kind_tests
