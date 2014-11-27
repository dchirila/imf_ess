program string_variable_as_format_spec
  implicit none
  integer :: a = 1, b = 2, c = 3
  real :: d = 3.1, e = 2.2, f = 1.3
  ! format-specifier to be reused
  character(len=*), parameter :: outputFormat = '(i0, 3x, f0.10)'

  print outputFormat, a, d
  print outputFormat, b, e
  print outputFormat, c, f
end program string_variable_as_format_spec
