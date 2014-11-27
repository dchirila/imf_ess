program where_construct3
  implicit none
  integer, parameter :: N = 7
  character(len=100) :: outFormat
  integer :: i, j
  real :: a(N,N) = 0, b(N,N) = 0, threshold = 0.5, &
       c(N,N) = 0, d(N,N) = 0 ! used in next examples

  ! write some values in a
  call random_number( a )

  ! Create dynamic format, with internal-file(=string) outFormat.
  ! This way, the format is adjusted automatically if N changes.
  write(outFormat, *) "(", N, "(x, f8.2))"

  write(*, '(a)') "a = "
  write(*, fmt=outFormat) &
       ( (a(i,j), j=1,N), i=1,N )

  ! ** Masked array-assignment **
  where( a > threshold )
     b = a
  elsewhere( a < threshold/2 )
     c = a
  end where

  write(*, '(/,a)') "b (after masked assignment) = "
  write(*, fmt=outFormat) &
       ( (b(i,j), j=1,N), i=1,N )

  write(*, '(/,a)') "c (after masked assignment) = "
  write(*, fmt=outFormat) ( (c(i,j), j=1,N), i=1,N )
end program where_construct3
