program do_concurrent_checkerboard_selection
  implicit none
  integer, parameter :: DOUBLE_REAL = selected_real_kind(15, 307)
  integer, parameter :: N = 5 ! side-length of the matrices
  integer :: i, j ! dummy-indices
  real(kind=DOUBLE_REAL), dimension(N,N) :: a, b ! the matrices
  character(len=100) :: outFormat

  ! Create dynamic format, using internal file
  write(outFormat, *) "(", N, "(x, f8.2))"
  ! Initialize matrix a to some random values
  call random_number( a )

  ! Pattern-selection with do concurrent
  do concurrent( i=1:N, j=1:N, mod(i+j, 2)==1 )
     b(i,j) = a(i,j)
  end do

  ! Print matrix b
  write(*, '(/,a)') "b ="
  write(*, fmt=outFormat) ( (b(i,j), j=1,N), i=1,N )
end program do_concurrent_checkerboard_selection

