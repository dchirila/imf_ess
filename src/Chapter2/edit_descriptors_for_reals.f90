program edit_descriptors_for_reals
  implicit none
  ! get kind for high-precision real
  integer, parameter :: QUAD_REAL = selected_real_kind(33,4931)
  real(kind=QUAD_REAL) :: testReal

  write(*, '(a)', advance='no') "Enter a real number: "
  read '(f100.50)', testReal
  ! print with various edit-descriptors
  print '(a, f0.2    )', "f0.2     : ", testReal
  print '(a, f10.2   )', "f10.2    : ", testReal
  print '(a, f14.4   )', "f14.4    : ", testReal
  print '(a, e14.4   )', "e14.4    : ", testReal
  print '(a, e14.6e3 )', "e14.6e3  : ", testReal
  print '(a, en14.4  )', "en14.4   : ", testReal
  print '(a, en14.6e3)', "en14.6e3 : ", testReal
  print '(a, es14.4  )', "es14.4   : ", testReal
  print '(a, es14.6e3)', "es14.6e3 : ", testReal
end program edit_descriptors_for_reals
